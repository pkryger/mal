#include "Core.h"
#include "ReadLine.h"
#include "Reader.h"
#include "Ranges.h"
#include "Types.h"

#include <algorithm>
#include <array>
#include <cassert>
#include <format>
#include <memory>
#include <optional>
#include <print>
#include <ranges>
#include <span>
#include <string>
#include <tuple>
#include <type_traits>
#include <utility>
#include <vector> // IWYU pragma: keep
// IWYU pragma: no_include <__vector/vector.h>

namespace mal {

static ReadLine rl("~/.mal_history");

ValuePtr READ(std::string str) { return readStr(std::move(str)); }

using SpecialForm = InvocableResult (*)(std::string, ValuesSpan, EnvPtr);

ValuePtr EVAL(ValuePtr, EnvPtr);

InvocableResult specialDefBang(std::string name, ValuesSpan values, EnvPtr env) {
  checkArgsIs(std::move(name), values, 2);
  if (auto symbol = to<Symbol>(values[0])) {
    auto val = EVAL(values[1], env);
    env->insert_or_assign(symbol->asKey(), val);
    return {val, env, false};
  }
  throw EvalException{std::format("invalid def! argument {:r}", values[1])};
}

InvocableResult specialLetStar(std::string name, ValuesSpan values, EnvPtr env) {
  checkArgsIs(std::move(name), values, 2);
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
                 return std::pair{symbol->asKey(), EVAL(chunk[1], letEnv)};
               }
               throw EvalException{
                   std::format("invalid let* binding '({:r}'", chunk)};
             })) {
      letEnv->insert_or_assign(std::move(key), std::move(value));
    }
    return {values[1], letEnv, true};
  }
  throw EvalException{std::format("invalid let* bindings '{:r}'", values[0])};
}

InvocableResult specialIf(std::string name, ValuesSpan values,
                                    EnvPtr env) {
  checkArgsBetween(std::move(name), values, 2, 3);
  auto cond = EVAL(values[0], env);
  if (cond->isTrue()) {
    return {values[1], env, true};
  } else if (values.size() == 3) {
    return {values[2], env, true};
  } else {
    return {Constant::nilValue(), env, false};
  }
}

InvocableResult specialFnStar(std::string name, ValuesSpan values,
                                        EnvPtr env) {
  checkArgsAtLeast(name, values, 2);
  if (auto sequence = to<Sequence>(values[0])) {
    return {make<Lambda>(
                sequence->values() | std::views::transform([&](auto &&elt) {
                  if (auto symbol = to<Symbol>(elt)) {
                    return symbol->asKey();
                  }
                  throwWrongArgument(std::move(name), elt);
                }) | std::ranges::to<std::vector>(),
                values[1], env),
            env, false};
  }
  throwWrongArgument(std::move(name), values[1]);
}

InvocableResult specialDo(std::string name, ValuesSpan values,
                      EnvPtr env) {
  checkArgsAtLeast(std::move(name), values, 1);
  for (auto &&val :
       values | std::views::take(values.size() - 1)) {
    EVAL(val, env);
  }
  return {values.back(), env, true};
}

using Specials = const std::pair<std::string, SpecialForm>;
static const std::array specials{
  Specials{"def!", &specialDefBang},
  Specials{"let*", &specialLetStar},
  Specials{"if", &specialIf},
  Specials{"fn*", &specialFnStar},
  Specials{"do", &specialDo},
};

ValuePtr EVAL(ValuePtr ast, EnvPtr env) {
  assert(ast);
  assert(env);
  bool needsEval{true};
  while (needsEval) {
    if (auto dbg = env->find("DEBUG-EVAL"); dbg && dbg->isTrue()) {
      std::print("EVAL: {:r}\n", ast);
    }

    if (auto list = to<List>(ast)) {
      auto&& values = list->values();
      if (values.empty()) {
        return ast->eval(env);
      }
      if (auto special = [&]() -> Specials * {
        if (auto symbol = to<Symbol>(values[0])) {
          auto res = std::find_if(
              specials.begin(), specials.end(),
              [&](auto &&elt) noexcept { return *symbol == elt.first; });
          return res != specials.end() ? res : nullptr;
        }
        return nullptr;
      }()) {
        std::tie(ast, env, needsEval) =
            special->second(special->first, values.subspan(1), env);
      } else {
        std::tie(ast, env, needsEval) = list->invoke(env);
      }
    } else {
      return ast->eval(env);
    }
  }
  return ast;
}

std::string PRINT(ValuePtr ast) {
  assert(ast);
  return std::format("{:r}", ast);
}

std::string rep(std::string str) {
  static Env env = []() {
    Env env{nullptr};
    installBuiltIns(env);
    return env;
  }();
  static EnvPtr envPtr =
      std::shared_ptr<Env>(std::addressof(env), [](auto &&) noexcept {});

  static auto defaultNot = [&]() {
    return EVAL(READ("(def! not (fn* (a) (if a false true)))"), envPtr);
  }();

  return PRINT(EVAL(READ(std::move(str)), envPtr));
}

}  // namespace mal

int main() {
  while (auto line = mal::rl.get("user> ")) {
    std::string out;
    try {
      out = mal::rep(std::move(line.value()));
    } catch (mal::ReaderException ex) {
      out = std::string{"[reader] "} + ex.what();
    } catch (mal::CoreException ex) {
      out = std::string{"[core] "} + ex.what();
    } catch (mal::EvalException ex) {
      out = std::string{"[eval] "} + ex.what();
    }
    std::print("{}\n", out);
  }
}
