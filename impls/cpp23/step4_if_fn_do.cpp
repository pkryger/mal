#include "Core.h"
#include "ReadLine.h"
#include "Reader.h"
#include "Ranges.h"
#include "Types.h"
#include <algorithm>
#include <cstddef>
#include <memory>
#include <utility>
#include <ranges>
#include <print>
#include <format>

static ReadLine rl("~/.mal_history");

MalValuePtr READ(std::string str) { return readStr(std::move(str)); }

using MalSpecialForm = MalValuePtr (*)(std::string, MalValues,  MalEnvPtr);

MalValuePtr EVAL(MalValuePtr, MalEnvPtr);

MalValuePtr specialDefBang(std::string name, MalValues values, MalEnvPtr env) {
  checkArgsIs(std::move(name), values, 2);
  if (auto symbol = dynamic_cast<MalSymbol *>(values[0].get())) {
    auto val = EVAL(values[1], env);
    env->insert_or_assign(symbol->asKey(), val);
    return val;
  }
  throw EvalException{std::format("invalid def! argument {:r}", values[1])};
}

MalValuePtr specialLetStar(std::string name, MalValues values, MalEnvPtr env) {
  checkArgsIs(std::move(name), values, 2);
  if (auto sequence = dynamic_cast<MalSequence *>(values[0].get())) {
    auto bindings = sequence->values();
    if (bindings.size() % 2 != 0) {
      throw EvalException{
          std::format("odd number of let* bindings: {:r}", values[0])};
    }
    auto letEnv = std::make_shared<MalEnv>(env);
    for (auto &&[key, value] :
         bindings | std::views::chunk(2) |
             std::views::transform([&](auto &&chunk) {
               if (auto symbol = dynamic_cast<MalSymbol *>(chunk[0].get())) {
                 return std::pair{symbol->asKey(), EVAL(chunk[1], letEnv)};
               }
               throw EvalException{
                   std::format("invalid let* binding '({:r}'", chunk)};
             })) {
      letEnv->insert_or_assign(std::move(key), value);
    }
    return EVAL(values[1], letEnv);
  }
  throw EvalException{std::format("invalid let* bindings '{:r}'", values[0])};
}

MalValuePtr specialIf(std::string name, MalValues values, MalEnvPtr env) {
  checkArgsBetween(std::move(name), values, 2, 3);
  auto cond = EVAL(values[0], env);
  if (cond->isTrue()) {
    return EVAL(values[1], env);
  } else if (values.size() == 3) {
    return EVAL(values[2], env);
  } else {
    return MalConstant::nilValue();
  }
}

MalValuePtr specialFnStar(std::string name, MalValues values, MalEnvPtr env) {
  checkArgsAtLeast(name, values, 2);
  if (auto sequence = dynamic_cast<MalSequence *>(values[0].get())) {
    return std::make_shared<MalLambda>(
        sequence->values() | std::views::transform([&](auto &&elt) {
          if (auto symbol = dynamic_cast<MalSymbol *>(elt.get())) {
            return symbol->asKey();
          }
          throwWrongArgument(std::move(name), elt);
        }) | std::ranges::to<std::vector>(),
        values[1], env);
  }
  throwWrongArgument(std::move(name), values[1]);
}

MalValuePtr specialDo(std::string name, MalValues values,
                      MalEnvPtr env) {
  checkArgsAtLeast(std::move(name), values, 1);
  for (auto &&val :
       values | std::views::take(values.size() - 1)) {
    EVAL(val, env);
  }
  return EVAL(values.back(), env);
}

MalValuePtr EVAL(MalValuePtr ast, MalEnvPtr env) {
  assert(ast);
  assert(env);
  if (auto dbg = env->find("DEBUG-EVAL"); dbg && dbg->isTrue()) {
    std::print("EVAL: {:r}\n", ast);
  }
  if (auto list = dynamic_cast<MalList *>(ast.get())) {
    auto&& values = list->values();
    if (values.empty()) {
      return ast->eval(env);
    }
    using Specials = const std::pair<std::string, MalSpecialForm>;
    static const Specials specials[]{
        {"def!", &specialDefBang},
        {"let*", &specialLetStar},
        {"if", &specialIf},
        {"fn*", &specialFnStar},
        {"do", &specialDo},
    };
    if (auto special = [&]() -> Specials * {
          if (auto symbol = dynamic_cast<MalSymbol *>(values[0].get())) {
            auto res = std::find_if(
                std::begin(specials), std::end(specials),
                [&](auto &&elt) noexcept { return *symbol == elt.first; });
            return res != std::end(specials) ? res : nullptr;
          }
          return nullptr;
        }()) {
      return special->second(special->first, values.subspan(1), env);
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
    std::print("{}\n", out);
  }
}
