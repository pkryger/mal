#include "Core.h"
#include "ReadLine.h"
#include "Reader.h"
#include "Ranges.h"
#include "Types.h"
#include <algorithm>
#include <cstddef>
#include <memory>
#include <utility>
#include <ranges>
#include <print>
#include <format>

namespace mal {

static ReadLine rl("~/.mal_history");

ValuePtr READ(std::string str) { return readStr(std::move(str)); }

using SpecialForm = ValuePtr (*)(std::string, ValuesSpan,  EnvPtr);

ValuePtr EVAL(ValuePtr, EnvPtr);

ValuePtr specialDefBang(std::string name, ValuesSpan values, EnvPtr env) {
  checkArgsIs(std::move(name), values, 2);
  if (auto symbol = to<Symbol>(values[0])) {
    auto val = EVAL(values[1], env);
    env->insert_or_assign(symbol->asKey(), val);
    return val;
  }
  throw EvalException{std::format("invalid def! argument {:r}", values[1])};
}

ValuePtr specialLetStar(std::string name, ValuesSpan values, EnvPtr env) {
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
      letEnv->insert_or_assign(std::move(key), value);
    }
    return EVAL(values[1], letEnv);
  }
  throw EvalException{std::format("invalid let* bindings '{:r}'", values[0])};
}

ValuePtr specialIf(std::string name, ValuesSpan values, EnvPtr env) {
  checkArgsBetween(std::move(name), values, 2, 3);
  auto cond = EVAL(values[0], env);
  if (cond->isTrue()) {
    return EVAL(values[1], env);
  } else if (values.size() == 3) {
    return EVAL(values[2], env);
  } else {
    return Constant::nilValue();
  }
}

ValuePtr specialFnStar(std::string name, ValuesSpan values, EnvPtr env) {
  checkArgsAtLeast(name, values, 2);
  if (auto sequence = to<Sequence>(values[0])) {
    return make<Lambda>(
        sequence->values() | std::views::transform([&](auto &&elt) {
          if (auto symbol = to<Symbol>(elt)) {
            return symbol->asKey();
          }
          throwWrongArgument(std::move(name), elt);
        }) | std::ranges::to<std::vector>(),
        values[1], env);
  }
  throwWrongArgument(std::move(name), values[1]);
}

ValuePtr specialDo(std::string name, ValuesSpan values,
                      EnvPtr env) {
  checkArgsAtLeast(std::move(name), values, 1);
  for (auto &&val :
       values | std::views::take(values.size() - 1)) {
    EVAL(val, env);
  }
  return EVAL(values.back(), env);
}

ValuePtr EVAL(ValuePtr ast, EnvPtr env) {
  assert(ast);
  assert(env);
  if (auto dbg = env->find("DEBUG-EVAL"); dbg && dbg->isTrue()) {
    std::print("EVAL: {:r}\n", ast);
  }
  if (auto list = to<List>(ast)) {
    auto&& values = list->values();
    if (values.empty()) {
      return ast->eval(env);
    }
    using Specials = const std::pair<std::string, SpecialForm>;
    static const Specials specials[]{
        {"def!", &specialDefBang},
        {"let*", &specialLetStar},
        {"if", &specialIf},
        {"fn*", &specialFnStar},
        {"do", &specialDo},
    };
    if (auto special = [&]() -> Specials * {
          if (auto symbol = to<Symbol>(values[0])) {
            auto res = std::find_if(
                std::begin(specials), std::end(specials),
                [&](auto &&elt) noexcept { return *symbol == elt.first; });
            return res != std::end(specials) ? res : nullptr;
          }
          return nullptr;
        }()) {
      return special->second(special->first, values.subspan(1), env);
    }
  }
  return ast->eval(env);
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
  return PRINT(EVAL(READ(std::move(str)), envPtr));
}

}  // namespace mal

int main() {
  std::string line;
  while (mal::rl.get("user> ", line)) {
    std::string out;
    try {
      out = mal::rep(std::move(line));
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
