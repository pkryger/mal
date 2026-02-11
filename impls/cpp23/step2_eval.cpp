#include "Core.h"
#include "ReadLine.h"
#include "Reader.h"
#include "Types.h"
#include <iostream>
#include <memory>

static ReadLine rl("~/.mal_history");

MalValuePtr READ(std::string str) { return readStr(std::move(str)); }

MalValuePtr EVAL(MalValuePtr ast, MalEnvPtr env) {
  assert(ast);
  assert(env);
  return ast->eval(env);
}

std::string PRINT(MalValuePtr ast) {
  assert(ast);
  return ast->print(true);
}

std::string rep(std::string str) {
  static MalEnv env = []() {
    MalEnv env{nullptr};
    installBuiltIns(env);
    return env;
  }();
  static MalEnvPtr envPtr =
    std::shared_ptr<MalEnv>(std::addressof(env), [](auto &&) noexcept {});
  return PRINT(EVAL(READ(std::move(str)), envPtr));
}

int main() {
  std::string line;
  while (rl.get("user> ", line)) {
    std::string out;
    try {
      out = rep(std::move(line));
    } catch (ReaderException ex) {
      out = std::string{"[reader] "} + ex.what();
    } catch (CoreException ex) {
      out = std::string{"[core] "} + ex.what();
    } catch (EvalException ex) {
      out = std::string{"[eval] "} + ex.what();
    }
    std::cout << out << "\n";
  }
}
