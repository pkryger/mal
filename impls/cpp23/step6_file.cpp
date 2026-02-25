#include "Core.h"
#include "ReadLine.h"
#include "Reader.h"
#include "Specials.h"
#include "Types.h"

#include <algorithm>
#include <array>
#include <cassert>
#include <cstddef>
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

namespace mal {

static ReadLine rl("~/.mal_history");

ValuePtr READ(std::string str) { return readStr(std::move(str)); }

using Special = const std::pair<std::string, SpecialForm>;
static const std::array specials{
  Special{"def!", specialDefBang},
  Special{"let*", specialLetStar},
  Special{"if", specialIf},
  Special{"fn*", specialFnStar},
  Special{"do", specialDo},
};

ValuePtr EVAL(ValuePtr ast, EnvPtr env) {
  assert(ast);
  assert(env);
  bool needsEval{true};
  static auto debug_eval = make<Symbol>("DEBUG-EVAL");
  while (needsEval) {
    if (auto dbg = env->find(debug_eval->asKey()); dbg && dbg->isTrue()) {
      std::print("EVAL: {:r}\n", ast);
    }

    if (auto list = to<List>(ast)) {
      auto&& values = list->values();
      if (values.empty()) {
        return ast->eval(env);
      }
      if (auto special = [&]() -> Special * {
        if (auto symbol = to<Symbol>(values[0])) {
          auto res = std::ranges::find_if(specials, [&](auto &&elt) noexcept {
            return *symbol == elt.first;
          });
          return res != specials.end() ? res : nullptr;
        }
        return nullptr;
      }()) {
        std::tie(ast, env, needsEval) =
            special->second(special->first, values.subspan(1), env, EVAL);
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

EnvPtr repEnv(std::span<const char*> args)
{
  static Env env = []() {
    Env env{nullptr};
    prepareEnv(EVAL, env);
    return env;
  }();
  static EnvPtr envPtr =
      std::shared_ptr<Env>(std::addressof(env), [](auto &&) noexcept {});


  static auto defaultEval = [&]() {
    auto eval = make<Eval>(envPtr);
    env.insert_or_assign(Symbol{"eval"}.asKey(), eval);
    return eval;
  }();

  static auto defaultNot = [&]() {
    return EVAL(READ("(def! not (fn* (a) (if a false true)))"), envPtr);
  }();

  static auto defaultLoadFile = [&]() {
    return EVAL(READ("(def! load-file (fn* (f) (eval (read-string (str \"(do "
                     "\" (slurp f) \"\nnil)\")))))"),
                envPtr);
  }();

  env.insert_or_assign(
      Symbol{"*ARGV*"}.asKey(),
      make<List>(args | std::views::drop(1) |
                 std::views::transform([](auto &&arg) -> ValuePtr {
                   return make<String>(arg);
                 })));

  return envPtr;
}

std::string rep(std::string str, EnvPtr envPtr) {
  std::string out;
  try {
    out = PRINT(EVAL(READ(std::move(str)), envPtr));
  } catch (mal::ReaderException ex) {
    out = std::string{"[reader] "} + ex.what();
  } catch (mal::CoreException ex) {
    out = std::string{"[core] "} + ex.what();
  } catch (mal::EvalException ex) {
    out = std::string{"[eval] "} + ex.what();
  }
  return out;
}

}  // namespace mal

int main(int argc, const char *argv[]) {
  auto args = std::span{argv, static_cast<std::size_t>(argc)}.subspan(1);
  auto envPtr = mal::repEnv(args);
  if (!args.empty()) {
    mal::rep(std::format("(load-file \"{}\")", args[0]), envPtr);
    return 0;
  }
  while (auto line = mal::rl.get("user> ")) {
    std::print("{}\n", rep(std::move(line.value()), envPtr));
  }
}
