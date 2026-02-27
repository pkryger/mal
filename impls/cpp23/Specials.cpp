#include "Specials.h" // IWYU pragma: associated
#include "Core.h"
#include "Ranges.h"
#include "Types.h"

#include <algorithm>
#include <cassert>
#include <format>
#include <memory>
#include <ranges>
#include <string_view>
#include <utility>
// IWYU pragma: no_include <span>
// IWYU pragma: no_include <string>
// IWYU pragma: no_include <tuple>
// IWYU pragma: no_include <type_traits>

namespace mal {
InvocableResult specialDefBang(std::string_view name, ValuesSpan values,
                               EnvPtr env, EvalFn evalFn) {
  checkArgsIs(name, values, 2);
  if (auto symbol = to<Symbol>(values[0])) {
    auto val = evalFn(values[1], env);
    assert(dynamic_cast<Env *>(env.get()));
    dynamic_cast<Env *>(env.get())->insert_or_assign(symbol->asKey(), val);
    return {std::move(val), std::move(env), false};
  }
  throw EvalException{
      std::format("invalid '{}' argument {:r}", name, values[1])};
}

InvocableResult specialLetStar(std::string_view name, ValuesSpan values,
                               EnvPtr env, EvalFn evalFn) {
  checkArgsIs(name, values, 2);
  if (auto sequence = to<Sequence>(values[0])) {
    auto bindings = sequence->values();
    if (bindings.size() % 2 != 0) {
      throw EvalException{
          std::format("odd number of let* bindings: {:r}", values[0])};
    }
    auto letEnv = make<Env>(env);
    for (auto &&[key, value] :
         bindings | std::views::chunk(2) |
             std::views::transform([&](auto &&chunk) {
               if (auto symbol = to<Symbol>(chunk[0])) {
                 return std::pair{symbol->asKey(), evalFn(chunk[1], letEnv)};
               }
               throw EvalException{
                   std::format("invalid let* binding '({:r}'", chunk)};
             })) {
      letEnv->insert_or_assign(std::move(key), std::move(value));
    }
    return {values[1], std::move(letEnv), true};
  }
  throw EvalException{std::format("invalid let* bindings '{:r}'", values[0])};
}

InvocableResult specialIf(std::string_view name, ValuesSpan values, EnvPtr env,
                          EvalFn evalFn) {
  checkArgsBetween(name, values, 2, 3);
  auto cond = evalFn(values[0], env);
  if (cond->isTrue()) {
    return {values[1], std::move(env), true};
  } else if (values.size() == 3) {
    return {values[2], std::move(env), true};
  } else {
    return {Constant::nilValue(), std::move(env), false};
  }
}

InvocableResult specialFnStar(std::string_view name, ValuesSpan values,
                              EnvPtr env, EvalFn evalFn) {
  checkArgsIs(name, values, 2);
  if (auto sequence = to<Sequence>(values[0])) {
    return {make<Lambda>(sequence->values() |
                             std::views::transform([&](auto &&elt) {
                               if (auto symbol = to<Symbol>(elt)) {
                                 return Symbol{*symbol};
                               }
                               throwWrongArgument(name, elt);
                             }) | std::views::as_rvalue,
                         values[1], env),
            std::move(env), false};
  }
  throwWrongArgument(name, values[1]);
}

InvocableResult specialDo(std::string_view name, ValuesSpan values, EnvPtr env,
                          EvalFn evalFn) {
  checkArgsAtLeast(name, values, 1);
  for (auto &&val :
       values | std::views::take(values.size() - 1)) {
    evalFn(val, env);
  }
  return {values.back(), std::move(env), true};
}

InvocableResult specialQuote(std::string_view name, ValuesSpan values,
                             EnvPtr env, EvalFn /* evalFn */) {
  checkArgsIs(name, values, 1);
  return {values[0], std::move(env), false};
}

InvocableResult specialQuasiquote(std::string_view name, ValuesSpan values,
                                  EnvPtr env, EvalFn evalFn) {
  checkArgsIs(name, values, 1);
  static auto valuesIfSequence =
      []<typename SEQUENCE>(const ValuePtr &value) -> ValuesSpan {
    if (auto sequence = to<SEQUENCE>(value)) {
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
        unquoteArg && to<List>(ast)) {
      return {std::move(unquoteArg), std::move(env), true};
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
          auto [v, _, needsEval] = specialQuasiquote(
              name, ValuesSpan{std::addressof(elt), 1}, env, evalFn);
          return make<List>(
              make<Symbol>("cons"),
              needsEval ? std::move(v)
                        : make<List>(make<Symbol>("quote"), std::move(v)),
              acc);
        });
    if (to<Vector>(ast)) {
      res = make<List>(make<Symbol>("vec"), std::move(res));
    }
    return {std::move(res), std::move(env), true};
  }
  return {ast, std::move(env), false};
}

InvocableResult specialDefmacroBang(std::string_view name, ValuesSpan values,
                                    EnvPtr env, EvalFn evalFn) {
  checkArgsIs(name, values, 2);
  if (auto symbol = to<Symbol>(values[0])) {
    auto res = evalFn(values[1], env);
    if (auto lambda = to<Lambda>(res)) {
      res = make<Macro>(std::move(const_cast<Lambda&>(*lambda)));
      dynamic_cast<Env *>(env.get())->insert_or_assign(symbol->asKey(), res);
      return {res, std::move(env), false};
    }
    throwWrongArgument(name, res);
  }
  throwWrongArgument(name, values[0]);
}

InvocableResult specialTryStar(std::string_view name, ValuesSpan values,
                               EnvPtr env, EvalFn evalFn) {
  checkArgsBetween(name, values, 1, 2);
  if (values.size() == 1) {
    return {values[0], env, true};
  }

  if (auto list = to<List>(values[1])) {
    auto catchValues = list->values();
    if (auto catchSymbol = to<Symbol>(catchValues[0]);
        catchSymbol && catchSymbol->name() == "catch*") {
      checkArgsIs(catchSymbol->name(), catchValues, 3);
      if (auto symbol = to<Symbol>(catchValues[1])) {
        auto exceptionHandler = [&](auto &&value) -> InvocableResult {
          auto catchEnv = make<Env>(env);
          catchEnv->insert_or_assign(symbol->asKey(), value);
          return {catchValues[2], catchEnv, true};
        };
        try {
          return {evalFn(values[0], env), env, false};
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
