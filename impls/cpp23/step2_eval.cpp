#include "Core.h"
#include "ReadLine.h"
#include "Reader.h"
#include "Types.h"
#include <iostream>
#include <memory>

namespace mal {
static ReadLine rl("~/.mal_history");

ValuePtr READ(std::string str) { return readStr(std::move(str)); }

ValuePtr EVAL(ValuePtr ast, EnvPtr env) {
  assert(ast);
  assert(env);
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

} // namespace mal

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
    std::cout << out << "\n";
  }
}
