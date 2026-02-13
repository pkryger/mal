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
  if (auto dbg = env->find("DEBUG-EVAL");
      dbg && dbg->isTrue()) {
    std::cout << "EVAL: " << ast->print(true) << "\n";
  }
  if (auto list = dynamic_cast<MalList *>(ast.get())) {
    auto&& lvalues = list->values();
    if (lvalues.empty()) {
      return ast->eval(env);
    }
    auto frontValueIs = [&](std::string name) noexcept {
      if (auto symbol = dynamic_cast<MalSymbol *>(lvalues[0].get())) {
        return *symbol == MalSymbol{std::move(name)};
      }
      return false;
    };
    if (frontValueIs("def!")) {
      if (lvalues.size() != 3) {
        throw EvalException{"wrong number of def! arguments " +
          std::to_string(lvalues.size())};
      }
      if (auto symbol = dynamic_cast<MalSymbol *>(lvalues[1].get())) {
        auto val = EVAL(lvalues[2], env);
        env->insert_or_assign(symbol->asKey(), val);
        return val;
      }
      throw EvalException{
          std::format("invalid def! argument '{:r}'", lvalues[1])};
    }
    if (frontValueIs("let*")) {
      if (lvalues.size() != 3) {
        throw EvalException{
            std::format("wrong number of let* arguments: {:r}", lvalues)};
      }
      if (auto bindings = dynamic_cast<MalSequence *>(lvalues[1].get())) {
        auto bvalues = bindings->values();
        if (bvalues.size() % 2 != 0) {
          throw EvalException{
              std::format("odd number of let* bindings: {:r}", bvalues)};
        }
        auto letEnv = std::make_shared<MalEnv>(env);
        for (auto i = bvalues.begin(); i != bvalues.end(); i += 2) {
          if (auto symbol = dynamic_cast<MalSymbol *>(i->get())) {
            letEnv->insert_or_assign(symbol->asKey(),
                                     EVAL(*(i + 1), letEnv));
          } else {
            throw EvalException{std::format("invalid let* binding '{:r}'", *i)};
          }
        }
        return EVAL(lvalues[2], letEnv);
      }
      throw EvalException{
          std::format("invalid let* bindings '{:r}'", lvalues[1])};
    }
  }
  return ast->eval(env);
}

std::string PRINT(MalValuePtr ast) {
  assert(ast);
  return std::format("{:r}", ast);
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
