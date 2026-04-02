#include "Env.h"
#include "GarbageCollector.h"
#include "Mal.h"
#include "Core.h"
#include "ReadLine.h"
#include "Reader.h"
#include "Types.h"

#include <cassert>
#include <format>
#include <memory>
#include <optional>
#include <print>
#include <string>
#include <utility>

namespace {
using namespace mal;

ValuePtr READ(const std::string &str) { return readStr(str); }

// NOLINTNEXTLINE(performance-unnecessary-value-param) - interface
ValuePtr EVAL(ValuePtr ast, const EnvPtr &env) {
  assert(ast);
  assert(env);
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
  static const GarbageCollectStack::Guard gcGuard{gcRegister};
  static const EvalFnStack::Guard evalGuard{EVAL};

  static Env env = []() {
    Env env{nullptr};
    prepareEnv(env);
    return env;
  }();
  static const EnvPtr envPtr =
    std::shared_ptr<Env>(std::addressof(env), [](auto &&) noexcept {});
  return PRINT(EVAL(READ(str), envPtr));
}

} // namespace

int main() {
  static mal::ReadLine rl("~/.mal_history");
  while (auto line = rl.get("user> ")) {
    std::string out;
    try {
      out = rep(line.value());
    } catch (const mal::ReaderException &ex) {
      out = std::string{"[reader] "} + ex.what();
    } catch (const mal::CoreException &ex) {
      out = std::string{"[core] "} + ex.what();
    } catch (const mal::EvalException &ex) {
      out = std::string{"[eval] "} + ex.what();
    }
    std::print("{}\n", out);
  }
}
