#include "Mal.h"
#include "GarbageCollector.h"
#include "ReadLine.h"
#include "Reader.h"
#include "Types.h" // IWYU pragma: keep

#include <cassert>
#include <memory>
#include <format>
#include <optional>
#include <print>
#include <string>
#include <utility>

namespace mal {
static ReadLine rl("~/.mal_history");

ValuePtr READ(const std::string &str) { return readStr(str); }

ValuePtr EVAL(ValuePtr val, const EnvPtr &/* envPtr */) {
  assert(val);
  return val;
}

std::string PRINT(ValuePtr val) {
  assert(val);
  return std::format("{:l}", val);
}

std::string rep(const std::string &str) {
  static GarbageCollector<GarbageCollectiblePtr> gc;
  static auto gcRegister = [&](GarbageCollectiblePtr value) {
    gc.registerValue(std::move(value));
  };
  static GarbageCollectStack::Guard gcGuard{gcRegister};
  static EvalFnStack::Guard evalGuard{EVAL};

  return PRINT(EVAL(READ(str), nullptr));
}

} // namespace mal

int main() {
  while (auto line = mal::rl.get("user> ")) {
    std::string out;
    try {
      out = mal::rep(line.value());
    } catch (mal::ReaderException ex) {
      out = std::string{"ReaderException: "} + ex.what();
    }
    std::print("{}\n", out);
  }
}
