#include "Env.h"
#include "GarbageCollector.h"
#include "Mal.h"
#include "Core.h"
#include "ReadLine.h"
#include "Reader.h"
#include "Specials.h"
#include "Types.h"

#include <algorithm>
#include <array>
#include <cassert>
#include <format>
#include <memory>
#include <optional>
#include <print>
#include <string>
#include <utility>
// IWYU pragma: no_include <string_view>
// IWYU pragma: no_include <tuple>

namespace mal {

ValuePtr READ(const std::string &str) { return readStr(str); }

using Special = const std::pair<std::string, SpecialForm>;
static const std::array specials{
  Special{"def!", specialDefBang},
  Special{"let*", specialLetStar},
  Special{"if", specialIf},
  Special{"fn*", specialFnStar},
  Special{"do", specialDo},
};

ValuePtr EVAL(ValuePtr ast, const EnvPtr &env) {
  assert(ast);
  assert(env);
  if (auto dbg = env->find(debugEval.asKey()); dbg && dbg->isTrue()) {
    std::print("EVAL: {:l}\n", ast);
  }
  if (auto list = ast->dyncast<List>()) {
    auto&& values = list->values();
    if (values.empty()) {
      return ast->eval(env);
    }
    if (auto special = [&]() -> Special * {
          if (auto symbol = values.front()->dyncast<Symbol>()) {
            auto res = std::ranges::find_if(specials, [&](auto &&elt) noexcept {
              return *symbol == elt.first;
            });
            return res != specials.end() ? res : nullptr;
          }
          return nullptr;
        }()) {
      auto [ast, evalEnv] =
          special->second(special->first, values.subspan(1), env);
      if (evalEnv) {
        return EVAL(ast, *evalEnv);
      }
      return ast;
    }
  }
  return ast->eval(env);
}

std::string PRINT(ValuePtr ast) {
  assert(ast);
  return std::format("{:r}", ast);
}

std::string rep(const std::string &str) {
  static GarbageCollector<GarbageCollectiblePtr> gc;
  static auto gcRegister = [&](GarbageCollectiblePtr value) {
    gc.registerValue(std::move(value));
  };
  static GarbageCollectStack::Guard gcGuard{gcRegister};
  static EvalFnStack::Guard evalGuard{EVAL};

  static Env env = []() {
    Env env{nullptr};
    prepareEnv(env);
    return env;
  }();
  static EnvPtr envPtr =
      std::shared_ptr<Env>(std::addressof(env), [](auto &&) noexcept {});

  static auto defaultNot = [&]() {
    return EVAL(READ("(def! not (fn* (a) (if a false true)))"), envPtr);
  }();

  return PRINT(EVAL(READ(str), envPtr));
}

}  // namespace mal

int main() {
  static mal::ReadLine rl("~/.mal_history");
  while (auto line = rl.get("user> ")) {
    std::string out;
    try {
      out = mal::rep(line.value());
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
