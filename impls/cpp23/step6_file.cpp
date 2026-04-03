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
#include <cstddef>
#include <format>
#include <memory>
#include <optional>
#include <print>
#include <ranges>
#include <span>
#include <string>
#include <tuple>
#include <utility>

namespace {
// NOLINTNEXTLINE(google-build-using-namespace)
using namespace mal;

ValuePtr READ(const std::string &str) { return readStr(str); }

using Special = const std::pair<std::string, SpecialForm>;
const std::array specials{
  Special{"def!", specialDefBang},
  Special{"let*", specialLetStar},
  Special{"if", specialIf},
  Special{"fn*", specialFnStar},
  Special{"do", specialDo},
};

// NOLINTNEXTLINE(misc-no-recursion)
ValuePtr EVAL(ValuePtr ast, const EnvPtr &evalEnv) {
  assert(ast);
  assert(evalEnv);
  std::optional<EnvPtr> env{evalEnv};

  while (env) {
    if (auto dbg = (*env)->find(debugEval.asKey()); dbg && dbg->isTrue()) {
      std::print("EVAL: {:l}\n", ast);
    }

    if (const auto *list = ast->dyncast<List>()) {
      auto&& values = list->values();
      if (values.empty()) {
        return ast->eval(*env);
      }
      if (const auto *special = [&]() -> Special * {
            if (const auto *symbol = values.front()->dyncast<Symbol>()) {
              const auto *res =
                  std::ranges::find_if(specials, [&](auto &&elt) noexcept {
                    return *symbol == elt.first;
                  });
              return res != specials.end() ? res : nullptr;
            }
            return nullptr;
          }()) {
        std::tie(ast, env) =
            special->second(special->first, values.subspan(1), *env);
      } else {
        // NOLINTNEXTLINE(misc-no-recursion)
        std::tie(ast, env) = [&]() {
          auto data = list->values();
          assert(!data.empty());
          auto value = EVAL(data.front(), *env);
          if (const auto *invocable = value->dyncast<Invocable>()) {
            return invocable->apply(false, data.subspan(1), *env);
          }
          throw EvalException{std::format("invalid function '{:r}'", value)};
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

EnvPtr repEnv(std::span<const char*> args) {
  static GarbageCollector<GarbageCollectiblePtr> garbageCollector;
  static auto gcRegister = [&](GarbageCollectiblePtr value) {
    garbageCollector.registerValue(std::move(value));
  };
  static const GarbageCollectStack::Guard gcGuard{gcRegister};
  static const EvalFnStack::Guard evalGuard{EVAL};

  static Env env = []() {
    Env env{nullptr};
    prepareEnv(env);
    return env;
  }();
  static const EnvPtr envPtr =
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

std::string rep(const std::string &str, const EnvPtr &envPtr) {
  std::string out;
  try {
    out = PRINT(EVAL(READ(str), envPtr));
  } catch (const mal::ReaderException &ex) {
    out = std::string{"[reader] "} + ex.what();
  } catch (const mal::CoreException &ex) {
    out = std::string{"[core] "} + ex.what();
  } catch (const mal::EvalException &ex) {
    out = std::string{"[eval] "} + ex.what();
  }
  return out;
}

}  // namespace

int main(int argc, const char *argv[]) {
  auto args = std::span{argv, static_cast<std::size_t>(argc)}.subspan(1);
  auto envPtr = repEnv(args);
  if (!args.empty()) {
    rep(std::format("(load-file \"{}\")", args.front()), envPtr);
    return 0;
  }
  static mal::ReadLine readLine("~/.mal_history");
  while (auto line = readLine.get("user> ")) {
    std::print("{}\n", rep(line.value(), envPtr));
  }
}
