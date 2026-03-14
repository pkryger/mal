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
#include <tuple>
#include <utility>
// IWYU pragma: no_include <string_view>

namespace mal {

static ReadLine rl("~/.mal_history");

ValuePtr READ(const std::string &str) { return readStr(str); }

using Special = const std::pair<std::string, SpecialForm>;
static const std::array specials{
  Special{"def!", specialDefBang},
  Special{"let*", specialLetStar},
  Special{"if", specialIf},
  Special{"fn*", specialFnStar},
  Special{"do", specialDo},
};

ValuePtr EVAL(ValuePtr ast, const EnvPtr &evalEnv) {
  assert(ast);
  assert(evalEnv);
  std::optional<EnvPtr> env{evalEnv};

  while (env) {
    if (auto dbg = (*env)->find(debugEval.asKey()); dbg && dbg->isTrue()) {
      std::print("EVAL: {:l}\n", ast);
    }

    if (auto list = ast->dyncast<List>()) {
      auto&& values = list->values();
      if (values.empty()) {
        return ast->eval(*env);
      }
      if (auto special = [&]() -> Special * {
        if (auto symbol = values[0]->dyncast<Symbol>()) {
          auto res = std::ranges::find_if(specials, [&](auto &&elt) noexcept {
            return *symbol == elt.first;
          });
          return res != specials.end() ? res : nullptr;
        }
        return nullptr;
      }()) {
        std::tie(ast, env) =
            special->second(special->first, values.subspan(1), *env);
      } else {
        std::tie(ast, env) = [&]() {
          auto data = list->values();
          auto op = EVAL(data[0], *env);
          if (auto invocable = op->dyncast<Invocable>()) {
            return invocable->apply(false, data.subspan(1), *env);
          }
          throw EvalException{std::format("invalid function '{:r}'", op)};
        }();
      }
    } else {
      return ast->eval(*env);
    }
  }
  return ast;
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
  while (auto line = mal::rl.get("user> ")) {
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
