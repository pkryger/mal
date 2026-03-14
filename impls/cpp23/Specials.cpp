#include "Specials.h" // IWYU pragma: associated
#include "Core.h"
#include "Env.h"
#include "FunctionRef.h"
#include "Mal.h"
#if !defined(__cpp_lib_ranges_chunk)
#include "Ranges.h"
#endif // __cpp_lib_ranges_chunk
#include "Types.h"

#include <algorithm> // IWYU pragma: keep
#include <cassert>
#include <format>
#include <memory>
#include <optional>
#include <ranges>
#include <string_view>
#include <utility>
#include <span>
#include <string>
#include <type_traits>
// IWYU pragma: no_include <tuple>

namespace mal {
InvocableResult specialDefBang(std::string_view name, ValuesSpan values,
                               const EnvPtr &env) {
  assert(!EvalFnStack::empty());
  checkArgsIs(name, values, 2);
  if (auto symbol = values[0]->dyncast<Symbol>()) {
    auto val = EvalFnStack::top()(values[1], env);
    env->insert_or_assign(symbol->asKey(), val);
    return {val, {}};
  }
  throw EvalException{
      std::format("invalid '{}' argument {:r}", name, values[1])};
}

InvocableResult specialLetStar(std::string_view name, ValuesSpan values,
                               const EnvPtr &env) {
  assert(!EvalFnStack::empty());
  checkArgsIs(name, values, 2);
  if (auto sequence = values[0]->dyncast<Sequence>()) {
    auto bindings = sequence->values();
    if (bindings.size() % 2 != 0) {
      throw EvalException{
          std::format("odd number of let* bindings: {:r}", values[0])};
    }
    auto letEnv = make<Env>(env);
    auto &evalFn = EvalFnStack::top();
    for (auto &&[key, value] :
         bindings | std::views::chunk(2) |
             std::views::transform([&](auto &&chunk) {
               if (auto symbol = chunk[0]->template dyncast<Symbol>()) {
                 return std::pair{symbol->asKey(), evalFn(chunk[1], letEnv)};
               }
               throw EvalException{
                   std::format("invalid let* binding '({:r}'", chunk)};
             })) {
      if constexpr (std::is_trivially_copyable_v<decltype(key)>) {
        letEnv->insert_or_assign(key, std::move(value));
      } else {
        letEnv->insert_or_assign(
            std::move(key), // NOLINT(performance-move-const-arg) - false positive
            std::move(value));
      }
    }
    return {values[1], std::move(letEnv)};
  }
  throw EvalException{std::format("invalid let* bindings '{:r}'", values[0])};
}

InvocableResult specialIf(std::string_view name, ValuesSpan values,
                          const EnvPtr &env) {
  assert(!EvalFnStack::empty());
  checkArgsBetween(name, values, 2, 3);
  auto cond = EvalFnStack::top()(values[0], env);
  if (cond->isTrue()) {
    return {values[1], env};
  } else if (values.size() == 3) {
    return {values[2], env};
  } else {
    return {Constant::nilValue(), {}};
  }
}

InvocableResult specialFnStar(std::string_view name, ValuesSpan values,
                              const EnvPtr &env) {
  checkArgsIs(name, values, 2);
  if (auto sequence = values[0]->dyncast<Sequence>()) {
    return {make<Lambda>(sequence->values() |
                             std::views::transform([&](auto &&elt) {
                               if (auto symbol = elt->template dyncast<Symbol>()) {
                                 return Symbol{*symbol};
                               }
                               throwWrongArgument(name, elt);
                             }) | std::views::as_rvalue,
                         values[1], env),
            {}};
  }
  throwWrongArgument(name, values[1]);
}

InvocableResult specialDo(std::string_view name, ValuesSpan values,
                          const EnvPtr &env) {
  assert(!EvalFnStack::empty());
  checkArgsAtLeast(name, values, 1);
  for (auto &&val :
       values | std::views::take(values.size() - 1)) {
    EvalFnStack::top()(val, env);
  }
  return {values.back(), env};
}

InvocableResult specialQuote(std::string_view name, ValuesSpan values,
                             const EnvPtr &/* env */) {
  checkArgsIs(name, values, 1);
  return {values[0], {}};
}

InvocableResult specialQuasiquote(std::string_view name, ValuesSpan values,
                                  const EnvPtr &env) {
  checkArgsIs(name, values, 1);
  static auto valuesIfSequence =
      []<typename SEQUENCE>(const ValuePtr &value) -> ValuesSpan {
    if (auto sequence = value->dyncast<SEQUENCE>()) {
      return  sequence->values();
    }
    return {};
  };
  static auto argIfStartsWith = [](const Symbol &key,
                                   ValuesSpan values) -> ValuePtr {
    if (key.isEqualTo(values[0])->isTrue()) {
      checkArgsIs(key.name(), values.subspan(1), 1);
      return values[1];
    }
    return nullptr;
  };
  static const auto unquote = Symbol{"unquote"};
  static const auto splice_unquote = Symbol{"splice-unquote"};
  auto &&ast = values[0];
  if (values = valuesIfSequence.template operator()<Sequence>(ast);
      !values.empty()) {
    if (auto unquoteArg = argIfStartsWith(unquote, values);
        unquoteArg && ast->isa<List>()) {
      return {std::move(unquoteArg), env};
    }
    auto res = std::ranges::fold_left(
        values | std::views::reverse, make<List>(),
        [&](auto &&acc, auto &&elt) -> ValuePtr {
          if (auto spliceUnquote = [&]() -> ValuePtr {
                if (auto eltValues =
                        valuesIfSequence.template operator()<List>(elt);
                        !eltValues.empty()) {
                  if (auto spliceUnquote =
                          argIfStartsWith(splice_unquote, eltValues)) {
                    return spliceUnquote;
                  }
                }
                return nullptr;
              }()) {
            return make<List>(make<Symbol>("concat"), spliceUnquote, acc);
          }
          auto [v, newEnv] = specialQuasiquote(
              name, ValuesSpan{std::addressof(elt), 1}, env);
          return make<List>(
              make<Symbol>("cons"),
              newEnv ? std::move(v)
                     : make<List>(make<Symbol>("quote"), std::move(v)),
              acc);
        });
    if (ast->isa<Vector>()) {
      res = make<List>(make<Symbol>("vec"), std::move(res));
    }
    return {std::move(res), env};
  }
  return {ast, {}};
}

InvocableResult specialDefmacroBang(std::string_view name, ValuesSpan values,
                                    const EnvPtr &env) {
  assert(!EvalFnStack::empty());
  checkArgsIs(name, values, 2);
  auto &evalFn = EvalFnStack::top();
  if (auto symbol = values[0]->dyncast<Symbol>()) {
    if (auto lambda = [&]() -> const Lambda * {
      if (values[1]->isa<Symbol>()) {
        return evalFn(values[1], env)->dyncast<Lambda>();
      }
      return nullptr;
    }()) {
      auto res = make<Macro>(*lambda);
      dynamic_cast<Env *>(env.get())->insert_or_assign(symbol->asKey(), res);
      return {res, {}};
    }
    auto res = evalFn(values[1], env);
    if (auto lambda = res->dyncast<Lambda>()) {
      res = make<Macro>(std::move(const_cast<Lambda&>(*lambda)));
      env->insert_or_assign(symbol->asKey(), res);
      return {res, {}};
    }
    throwWrongArgument(name, values[1]);
  }
  throwWrongArgument(name, values[0]);
}

InvocableResult specialTryStar(std::string_view name, ValuesSpan values,
                               const EnvPtr &env) {
  assert(!EvalFnStack::empty());
  checkArgsBetween(name, values, 1, 2);
  if (values.size() == 1) {
    return {values[0], env};
  }

  if (auto list = values[1]->dyncast<List>()) {
    auto catchValues = list->values();
    if (auto catchSymbol = catchValues[0]->dyncast<Symbol>();
        catchSymbol && catchSymbol->name() == "catch*") {
      checkArgsIs(catchSymbol->name(), catchValues, 3);
      if (auto symbol = catchValues[1]->dyncast<Symbol>()) {
        auto exceptionHandler = [&](auto &&value) -> InvocableResult {
          auto catchEnv = make<Env>(env);
          catchEnv->insert_or_assign(symbol->asKey(), value);
          return {catchValues[2], std::move(catchEnv)};
        };
        try {
          return {EvalFnStack::top()(values[0], env), {}};
        } catch (CoreException ex) {
          return exceptionHandler(make<String>(ex.what()));
        } catch (EvalException ex) {
          return exceptionHandler(make<String>(ex.what()));
        } catch (MalException ex) {
          return exceptionHandler(ex.value);
        }
        assert(false);
      }
      throw EvalException{std::format("invalid '{}' argument {:r}",
                                      catchSymbol->name(), catchValues[1])};
    }
    throw EvalException{
        std::format("invalid '{}' argument {:r}", name, catchValues[0])};
  }
  throw EvalException{
      std::format("invalid '{}' argument {:r}", name, values[1])};
}


} // namespace mal
