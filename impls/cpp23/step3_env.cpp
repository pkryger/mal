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
  if (auto dbg = env->find("DEBUG-EVAL");
      dbg && dbg->isTrue()) {
    std::cout << "EVAL: " << ast->print(true) << "\n";
  }
  if (auto list = to<List>(ast)) {
    auto&& lvalues = list->values();
    if (lvalues.empty()) {
      return ast->eval(env);
    }
    auto frontValueIs = [&](std::string name) noexcept {
      if (auto symbol = to<Symbol>(lvalues[0])) {
        return *symbol == Symbol{std::move(name)};
      }
      return false;
    };
    if (frontValueIs("def!")) {
      if (lvalues.size() != 3) {
        throw EvalException{"wrong number of def! arguments " +
          std::to_string(lvalues.size())};
      }
      if (auto symbol = to<Symbol>(lvalues[1])) {
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
      if (auto bindings = to<Sequence>(lvalues[1])) {
        auto bvalues = bindings->values();
        if (bvalues.size() % 2 != 0) {
          throw EvalException{
              std::format("odd number of let* bindings: {:r}", bvalues)};
        }
        auto letEnv = make<Env>(env);
        for (auto i = bvalues.begin(); i != bvalues.end(); i += 2) {
          if (auto symbol = to<Symbol>(*i)) {
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
  while (auto line = mal::rl.get("user> ")) {
    std::string out;
    try {
      out = mal::rep(std::move(line.value()));
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
