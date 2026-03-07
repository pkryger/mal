#include "Core.h" // IWYU pragma: associated
#include "Env.h"
#include "Mal.h"
#include "Ranges.h"
#include "ReadLine.h"
#include "Reader.h"
#include "Types.h"

#include <algorithm>
#include <array>
#include <cassert>
#include <chrono>
#include <cstddef>
#include <cstdint>
#include <filesystem>
#include <format>
#include <fstream>
#include <iterator>
#include <memory>
#include <optional>
#include <print>
#include <ranges>
#include <string>
#include <string_view>
#include <tuple>
#include <utility>
// IWYU pragma: no_include <__vector/vector.h>
// IWYU pragma: no_include <type_traits>

namespace mal {

static ReadLine coreReadLine{};

void checkArgsIs(std::string_view name, ValuesSpan values, std::size_t expected) {

  if (auto actual = values.size(); actual != expected) {
    throw CoreException{
        std::format("wrong number of '{}' arguments: {}, expected: {}", name,
                    actual, expected)};

  }
}

void checkArgsAtLeast(std::string_view name, ValuesSpan values,
                      std::size_t expected) {
  if (auto actual = values.size(); actual < expected) {
    throw CoreException{
        std::format("wrong number of '{}' arguments: {}, expected: ({} . many)",
                    name, actual, expected)};
  }
}

void checkArgsBetween(std::string_view name, ValuesSpan values,
                      std::size_t expectedMin, std::size_t expectedMax) {
  if (auto actual = values.size();
       expectedMax < actual || actual < expectedMin) {
    throw CoreException{
        std::format("wrong number of '{}' arguments: {}, expected: ({} . {})",
                    name, actual, expectedMin, expectedMax)};
  }
}

[[noreturn]]
void throwWrongArgument(std::string_view name, ValuePtr val) {
  throw CoreException{
      std::format("wrong argument for '{}' call '{:r}'", name, val)};
}

} // namespace mal

namespace {
using mal::Atom;
using mal::Constant;
using mal::CoreException;
using mal::coreReadLine;
using mal::EnvPtr;
using mal::EvalFnStack;
using mal::Hash;
using mal::Integer;
using mal::Invocable;
using mal::InvocableResult;
using mal::Keyword;
using mal::List;
using mal::Macro;
using mal::make;
using mal::MalException;
using mal::MetaMixIn;
using mal::readStr;
using mal::Sequence;
using mal::String;
using mal::StringBase;
using mal::Symbol;
using mal::to;
using mal::ValuePtr;
using mal::ValuesContainer;
using mal::ValuesSpan;
using mal::Vector;

template <typename TYPE>
InvocableResult typeQuestion(std::string_view name, ValuesSpan values,
                             EnvPtr env) {
  checkArgsIs(name, values, 1);
  return {to<TYPE>(values[0]) ? Constant::trueValue() : Constant::falseValue(),
          std::move(env), false};
}

namespace detail {

template <typename BINARY_OP, typename UNARY_OP>
ValuePtr accumulateIntegers(std::string_view name, ValuesSpan values,
                            BINARY_OP &&binary_op, UNARY_OP &&unary_op,
                            int minArgs = 1) {
  assert(minArgs > 0);
  checkArgsAtLeast(name, values, minArgs);
  if (auto init = to<Integer>(values.front())) {
    if (values.size() == 1) {
      return make<Integer>(
          std::forward<UNARY_OP>(unary_op)(init->value()));
    }
    return make<Integer>(std::ranges::fold_left(
        values.subspan(1) | std::views::transform([&](auto elt) {
          if (auto integer = to<Integer>(elt)) {
            return integer->value();
          }
          throwWrongArgument(name, elt);
        }),
        init->value(), std::forward<BINARY_OP>(binary_op)));
  }
  throwWrongArgument(name, values.front());
}

} // namespace detail

InvocableResult addition(std::string_view name, ValuesSpan values, EnvPtr env) {
  return {detail::accumulateIntegers(
              name, values,
              [](auto &&acc, auto &&v) noexcept { return acc + v; },
              [](auto &&v) noexcept { return v; }),
          std::move(env), false};
}

InvocableResult subtraction(std::string_view name, ValuesSpan values,
                            EnvPtr env) {
  return {detail::accumulateIntegers(
              name, values, [](auto &&acc, auto &&v) { return acc - v; },
              [](auto &&v) noexcept { return -v; }),
          std::move(env), false};
}

InvocableResult multiplication(std::string_view name, ValuesSpan values,
                               EnvPtr env) {
  return {detail::accumulateIntegers(
              name, values, [](auto &&acc, auto &&v) { return acc * v; },
              [](auto &&v) noexcept { return v; }),
          std::move(env), false};
}

InvocableResult division(std::string_view name, ValuesSpan values, EnvPtr env) {
  return {detail::accumulateIntegers(
              name, values,
              [&](auto &&acc, auto &&v) {
                if (v == 0ll)
                  throw CoreException{std::format("'{}' division by 0", name)};
                return acc / v;
              },
              [&](auto &&) -> std::int64_t {
                throw CoreException{std::format(
                    "wrong number of '{}' arguments: 1, expected (2 . many)",
                    name)};
              },
              2),
          std::move(env), false};
}

InvocableResult list(std::string_view /* name */, ValuesSpan values,
                     EnvPtr env) {
  return {make<List>(values), std::move(env), false};
}

InvocableResult emptyQuestion(std::string_view name, ValuesSpan values,
                              EnvPtr env) {
  checkArgsIs(name, values, 1);
  if (auto sequence = to<Sequence>(values.front())) {
    return {sequence->values().empty() ? Constant::trueValue()
                                       : Constant::falseValue(),
            std::move(env), false};
  }
  throwWrongArgument(name, values.front());

}

InvocableResult count(std::string_view name, ValuesSpan values, EnvPtr env) {
  checkArgsAtLeast(name, values, 1);
  if (values.front().get() == Constant::nilValue().get()) {
    return {make<Integer>(0), std::move(env), false};
  }
  if (auto sequence = to<Sequence>(values.front())) {
    return {make<Integer>(sequence->values().size()), std::move(env), false};
  }
  throwWrongArgument(name, values.front());
}

template <typename BINARY_OP>
InvocableResult compareIntegers(std::string_view name, ValuesSpan values,
                                EnvPtr env, BINARY_OP &&binary_op) {
  checkArgsAtLeast(name, values, 2);
  auto notMatching = std::ranges::adjacent_find(
      values,
      [binary_op = std::forward<BINARY_OP>(binary_op)](auto &&lhs, auto &&rhs) {
        return !binary_op(lhs, rhs);
      },
      [name = name](auto &&elt) {
        if (auto integer = to<Integer>(elt)) {
          return integer->value();
        }
        throwWrongArgument(name, elt);
      });
  return {notMatching == values.end() ? Constant::trueValue()
                                      : Constant::falseValue(),
          std::move(env), false};

}

InvocableResult lt(std::string_view name, ValuesSpan values, EnvPtr env) {
  return compareIntegers(
      name, values, env,
      [](auto &&lhs, auto &&rhs) noexcept { return lhs < rhs; });
}

InvocableResult lte(std::string_view name, ValuesSpan values, EnvPtr env) {
  return compareIntegers(
      name, values, env,
      [](auto &&lhs, auto &&rhs) noexcept { return lhs <= rhs; });
}

InvocableResult gt(std::string_view name, ValuesSpan values, EnvPtr env) {
  return compareIntegers(
      name, values, env,
      [](auto &&lhs, auto &&rhs) noexcept { return lhs > rhs; });
}

InvocableResult gte(std::string_view name, ValuesSpan values, EnvPtr env) {
  return compareIntegers(
      name, values, env,
      [](auto &&lhs, auto &&rhs) noexcept { return lhs >= rhs; });
}

InvocableResult equal(std::string_view name, ValuesSpan values, EnvPtr env) {
  checkArgsAtLeast(name, values, 2);
  auto notEqual =
      std::ranges::adjacent_find(values, [](auto &&lhs, auto &&rhs) {
        return !lhs->isEqualTo(rhs)->isTrue();
      });
  return {notEqual == values.end() ? Constant::trueValue()
                                   : Constant::falseValue(),
          std::move(env), false};
}

InvocableResult not_(std::string_view name, ValuesSpan values, EnvPtr env) {
  checkArgsIs(name, values, 1);
  return {values.front()->isTrue() ? Constant::falseValue()
                                   : Constant::trueValue(),
          std::move(env), false};
}

InvocableResult prn(std::string_view name, ValuesSpan values, EnvPtr env) {
  std::print("{:r}\n", values);
  return {Constant::nilValue(), std::move(env), false};
}

InvocableResult println(std::string_view name, ValuesSpan values, EnvPtr env) {
  std::print("{}\n", values);
  return {Constant::nilValue(), std::move(env), false};
}

InvocableResult pr_str(std::string_view name, ValuesSpan values, EnvPtr env) {
  return {make<String>(std::format("{:r}", values)), std::move(env), false};
}

InvocableResult str(std::string_view name, ValuesSpan values, EnvPtr env) {
  return {make<String>(values | std::views::transform([](auto &&elt) {
                         return std::format("{}", elt);
                       }) |
                       std::views::join | std::ranges::to<std::string>()),
          std::move(env), false};
}

InvocableResult slurp(std::string_view name, ValuesSpan values, EnvPtr env) {
  checkArgsIs(name, values, 1);
  if (auto string = to<String>(values[0])) {
    auto path = string->data();
    auto file_size = std::filesystem::file_size(path);
    std::ifstream file{path, std::ios::in | std::ios::binary};
    if (!file) {
      throw CoreException(std::format("{}: file {} open error",
                                      name, std::move(path)));
    }
    std::string content(file_size, '\0');
    file.read(content.data(), file_size);
    return {make<String>(std::move(content)), std::move(env), false};
  }
   throwWrongArgument(name, values[0]);
}

InvocableResult read_string(std::string_view name, ValuesSpan values,
                            EnvPtr env) {
  checkArgsIs(name, values, 1);
  if (to<String>(values[0])) {
    return {readStr(std::format("{}", values[0])), std::move(env), false};
  }
  throwWrongArgument(name, values[0]);
}

InvocableResult atom(std::string_view name, ValuesSpan values, EnvPtr env) {
  checkArgsIs(name, values, 1);
  return {make<Atom>(values[0]), std::move(env), false};
}

InvocableResult deref(std::string_view name, ValuesSpan values, EnvPtr env) {
  checkArgsIs(name, values, 1);
  if (auto atom = to<Atom>(values[0])) {
    return {atom->value(), std::move(env), false};
  }
  throwWrongArgument(name, values[0]);
}

InvocableResult resetBang(std::string_view name, ValuesSpan values,
                          EnvPtr env) {
  checkArgsIs(name, values, 2);
  if (auto atom = to<Atom>(values[0])) {
    return {atom->reset(values[1]), std::move(env), false};
  }
  throwWrongArgument(name, values[0]);
}

namespace detail {

ValuesContainer cons(ValuePtr value, ValuesSpan values) {
  ValuesContainer res;
  res.reserve(values.size() + 1);
  res.emplace_back(std::move(value));
  std::ranges::copy(values, std::back_inserter(res));
  return res;
}

} // namespace detail

InvocableResult swapBang(std::string_view name, ValuesSpan values, EnvPtr env) {
  assert(!EvalFnStack::empty());
  checkArgsAtLeast(name, values, 2);
  if (auto atom = to<Atom>(values[0])) {
    if (auto fn = to<Invocable>(values[1])) {
      auto args = detail::cons(atom->value(), values.subspan(2));
      auto [ast, evalEnv, needsEval] = fn->apply(ValuesSpan{args}, env);
      if (needsEval) {
        ast = EvalFnStack::top()(ast, evalEnv);
      }
      return {atom->reset(ast), std::move(env), false};
    }
    throwWrongArgument(name, values[1]);
  }
  throwWrongArgument(name, values[0]);
}

InvocableResult cons(std::string_view name, ValuesSpan values, EnvPtr env) {
  checkArgsIs(name, values, 2);
  if (auto sequence = to<Sequence>(values[1])) {
    return {make<List>(detail::cons(values[0], sequence->values())),
            std::move(env), false};
  }
  return {make<List>(values), std::move(env), false};
}

InvocableResult concat(std::string_view name, ValuesSpan values, EnvPtr env) {
  return {make<List>(values | std::views::transform([&](auto elt) {
                       if (auto sequence = to<Sequence>(elt)) {
                         return sequence->values();
                       }
                       throwWrongArgument(name, elt);
                     }) |
                     std::views::join),
          std::move(env), false};
}

InvocableResult vec(std::string_view name, ValuesSpan values, EnvPtr env) {
  checkArgsIs(name, values, 1);
  if (to<Vector>(values[0])) {
    return {values[0], std::move(env), false};
  }
  if (auto list = to<List>(values[0])) {
    return {make<Vector>(list->values()), std::move(env), false};
  }
  throwWrongArgument(name, values[0]);
}

InvocableResult nth(std::string_view name, ValuesSpan values, EnvPtr env) {
  checkArgsIs(name, values, 2);
  if (auto integer = to<Integer>(values[1])) {
    auto index = integer->value();
    if (auto sequence = to<Sequence>(values[0])) {
      auto values = sequence->values();
      if (0 <= index && index < values.size()) {
        return {values[index], std::move(env), false};
      }
      throw CoreException{std::format("index out of bounds {} for '{}'", index,
                                      name)};
    }
    if (auto constant = to<Constant>(values[0]);
        constant && index == 0 && Constant::nilValue()->isEqualTo(values[0])) {
      return {Constant::nilValue(), std::move(env), false};
    }
    throwWrongArgument(name, values[0]);
  }
  throwWrongArgument(name, values[1]);
}

InvocableResult first(std::string_view name, ValuesSpan values, EnvPtr env) {
  checkArgsIs(name, values, 1);
  if (auto sequence = to<Sequence>(values[0])) {
    auto values = sequence->values();
    if (!values.empty()) {
      return {values[0], std::move(env), false};
    }
    return {Constant::nilValue(), std::move(env), false};
  }
  if (auto constant = to<Constant>(values[0]);
      constant && Constant::nilValue()->isEqualTo(values[0])) {
    return {Constant::nilValue(), std::move(env), false};
  }
  throwWrongArgument(name, values[0]);
}

InvocableResult rest(std::string_view name, ValuesSpan values, EnvPtr env) {
  checkArgsIs(name, values, 1);
  if (auto sequence = to<Sequence>(values[0])) {
    auto values = sequence->values();
    if (!values.empty()) {
      return {make<List>(values.subspan(1)), std::move(env), false};
    }
    return {make<List>(), std::move(env), false};
  }
  if (auto constant = to<Constant>(values[0]);
      constant && Constant::nilValue()->isEqualTo(values[0])) {
    return {make<List>(), std::move(env), false};
  }
  throwWrongArgument(name, values[0]);
}

[[noreturn]]
InvocableResult throw_(std::string_view name, ValuesSpan values, EnvPtr env) {
  checkArgsIs(name, values, 1);
  throw MalException{std::format("Exception: {:r}", values[0]), values[0]};
}

InvocableResult map(std::string_view name, ValuesSpan values, EnvPtr env) {
  assert(!EvalFnStack::empty());
  checkArgsIs(name, values, 2);
  if (auto invocable = to<Invocable>(values[0])) {
    if (auto sequence = to<Sequence>(values[1])) {
      return {
          make<List>(
              sequence->values() | std::views::transform([&](auto &&value) {
                auto [ast, evalEnv, needsEval] =
                    invocable->apply(ValuesSpan{std::addressof(value), 1}, env);
                if (needsEval) {
                  return EvalFnStack::top()(std::move(ast), std::move(evalEnv));
                }
                return ast;
              })),
          std::move(env), false};
    }
    throwWrongArgument(name, values[1]);
  }
  throwWrongArgument(name, values[0]);
}

InvocableResult apply(std::string_view name, ValuesSpan values, EnvPtr env) {
  checkArgsAtLeast(name, values, 2);
  if (auto invocable = to<Invocable>(values[0])) {
    ValuesContainer argsContainer;
    auto args = [&]() {
      if (auto sequence = to<Sequence>(values.back())) {
        auto backValues = sequence->values();
        if (backValues.size() == 0) {
          if (values.size() == 2) {
            return values.subspan(2);
          }
          return values.subspan(1);
        }
        argsContainer.reserve(values.size() - 1 + backValues.size());
        std::ranges::copy(values.begin() + 1, values.end() - 1,
                          std::back_inserter(argsContainer));
        std::ranges::copy(backValues, std::back_inserter(argsContainer));
        return ValuesSpan{argsContainer};
      }
      return values.subspan(1);
    }();
    return invocable->apply(args, env);
  }
  throwWrongArgument(name, values[0]);
}

namespace detail {

InvocableResult constantQuestion(std::string_view name, ValuesSpan values,
                                 EnvPtr env, ValuePtr constant) {
  checkArgsIs(name, values, 1);
  return {constant->isEqualTo(values[0]), std::move(env), false};
}

} // namespace detail

InvocableResult nilQuestion(std::string_view name, ValuesSpan values,
                     EnvPtr env) {
  return detail::constantQuestion(name, values, std::move(env), Constant::nilValue());
}

InvocableResult trueQuestion(std::string_view name, ValuesSpan values,
                     EnvPtr env) {
  return detail::constantQuestion(name, values, std::move(env), Constant::trueValue());
}

InvocableResult falseQuestion(std::string_view name, ValuesSpan values,
                     EnvPtr env) {
  return detail::constantQuestion(name, values, std::move(env), Constant::falseValue());
}

InvocableResult symbol(std::string_view name, ValuesSpan values, EnvPtr env) {
  checkArgsIs(name, values, 1);
  if (auto string = to<String>(values[0])) {
    return {make<Symbol>(string->data()), std::move(env), false};
  }
  throwWrongArgument(name, values[0]);
}

InvocableResult keyword(std::string_view name, ValuesSpan values, EnvPtr env) {
  checkArgsIs(name, values, 1);
  if (to<Keyword>(values[0])) {
    return {values[0], std::move(env), false};
  }
  if (auto string = to<String>(values[0])) {
    return {make<Keyword>(std::format(":{}", string->data())), std::move(env),
            false};
  }
  throwWrongArgument(name, values[0]);
}

InvocableResult vector(std::string_view /*name*/, ValuesSpan values,
                EnvPtr env) {

  return {make<Vector>(values), std::move(env), false};
}

InvocableResult hash_map(std::string_view name, ValuesSpan values,
                  EnvPtr env) {
  if (values.size() % 2 == 0) {
    for (auto key : values | std::views::stride(2)) {
      if (!to<StringBase>(key)) {
        mal::throwWrongArgument(name, key);
      }
    }
    return {make<Hash>(values), std::move(env), false};
  }
  throw CoreException{
      std::format("odd number of arguments for '{}', {}", name, values.size())};
}

InvocableResult assoc(std::string_view name, ValuesSpan values, EnvPtr env) {
  checkArgsAtLeast(name, values, 1);
  if (values.size() % 2 == 1) {
    if (auto hash = to<Hash>(values[0])) {
      for (auto key : values.subspan(1) | std::views::stride(2)) {
        if (!to<StringBase>(key)) {
          mal::throwWrongArgument(name, key);
        }
      }
      return {make<Hash>(*hash, values.subspan(1)), std::move(env), false};
    }
    throwWrongArgument(name, values[0]);
  }
  throw CoreException{std::format("odd number of arguments for '{}', {}", name,
                                  values.size() - 1)};
}

InvocableResult get(std::string_view name, ValuesSpan values, EnvPtr env) {
  checkArgsIs(name, values, 2);
  if (Constant::nilValue()->isEqualTo(values[0])->isTrue()) {
    return {Constant::nilValue(), std::move(env), false};
  }
  if (auto hash = to<Hash>(values[0])) {
    if (to<StringBase>(values[1])) {
      if (auto res = hash->find(values[1]))
        return {res, std::move(env), false};
      else {
        return {Constant::nilValue(), std::move(env), false};
      }
    }
    throwWrongArgument(name, values[1]);
  }
  throwWrongArgument(name, values[0]);
}

InvocableResult containsQuestion(std::string_view name, ValuesSpan values,
                                 EnvPtr env) {

  checkArgsIs(name, values, 2);
  if (Constant::nilValue()->isEqualTo(values[0])->isTrue()) {
    return {Constant::nilValue(), std::move(env), false};
  }
  if (auto hash = to<Hash>(values[0])) {
    if (to<StringBase>(values[1])) {
      if (hash->find(values[1]))
        return {Constant::trueValue(), std::move(env), false};
      else {
        return {Constant::falseValue(), std::move(env), false};
      }
    }
    throwWrongArgument(name, values[1]);
  }
  throwWrongArgument(name, values[0]);
}

InvocableResult keys(std::string_view name, ValuesSpan values, EnvPtr env) {
  checkArgsIs(name, values, 1);
  if (Constant::nilValue()->isEqualTo(values[0])->isTrue()) {
    return {Constant::nilValue(), std::move(env), false};
  }
  if (auto hash = to<Hash>(values[0])) {
    return {make<List>(*hash | std::views::transform(
                                   [](auto &&elt) { return elt.first; })),
            std::move(env), false};
  }
  throwWrongArgument(name, values[0]);
}

InvocableResult vals(std::string_view name, ValuesSpan values, EnvPtr env) {
  checkArgsIs(name, values, 1);
  if (Constant::nilValue()->isEqualTo(values[0])->isTrue()) {
    return {Constant::nilValue(), std::move(env), false};
  }
  if (auto hash = to<Hash>(values[0])) {
    return {make<List>(*hash | std::views::transform(
                                   [](auto &&elt) { return elt.second; })),
            std::move(env), false};
  }
  throwWrongArgument(name, values[0]);
}

InvocableResult dissoc(std::string_view name, ValuesSpan values, EnvPtr env) {
  checkArgsAtLeast(name, values, 1);
  if (auto hash = to<Hash>(values[0])) {
    values = values.subspan(1);
    for (auto key : values) {
      if (!to<StringBase>(key)) {
        throwWrongArgument(name, key);
      }
    }
    return {make<Hash>(*hash | std::views::filter([&](auto &&elt) {
              return !std::ranges::contains(values, elt.first);
            })),
            std::move(env), false};
  }
  throwWrongArgument(name, values[0]);
}

InvocableResult readline(std::string_view name, ValuesSpan values, EnvPtr env) {
  checkArgsIs(name, values, 1);
  if (auto prompt = to<String>(values[0])) {
    if (auto res = coreReadLine.get(prompt->data())) {
      return {make<String>(*res), std::move(env), false};
    }
    return {Constant::nilValue(), std::move(env), false};
  }
  throwWrongArgument(name, values[0]);
}

InvocableResult time_ms(std::string_view name, ValuesSpan values, EnvPtr env) {
  checkArgsIs(name, values, 0);
  return {make<Integer>(std::chrono::duration_cast<std::chrono::milliseconds>(
                            std::chrono::steady_clock::now().time_since_epoch())
                            .count()),
          std::move(env), false};
}

InvocableResult seq(std::string_view name, ValuesSpan values, EnvPtr env) {
  checkArgsIs(name, values, 1);
  if (to<Constant>(values[0])) {
    if (Constant::nilValue()->isEqualTo(values[0])->isTrue()) {
      return {Constant::nilValue(), std::move(env), false};
    }
    throwWrongArgument(name, values[0]);
  }
  if (auto sequence = to<Sequence>(values[0])) {
    if (sequence->values().empty()) {
      return {Constant::nilValue(), std::move(env), false};
    }
    if (auto vector = to<Vector>(values[0])) {
      return {make<List>(vector->values()), std::move(env), false};
    }
    return {values[0], std::move(env), false};
  }
  if (auto string = to<String>(values[0])) {
    if (string->data().empty()) {
      return {Constant::nilValue(), std::move(env), false};
    }
    return {make<List>(string->data() | std::views::transform([](auto &&c) {
                         return make<String>(std::string{c});
                       })),
            std::move(env), false};
  }
  throwWrongArgument(name, values[0]);
}

InvocableResult conj(std::string_view name, ValuesSpan values, EnvPtr env) {
  checkArgsAtLeast(name, values, 2);
  if (auto list = to<List>(values[0])) {
    values = values.subspan(1);
    auto res =
        values | std::views::reverse | std::ranges::to<ValuesContainer>();
    res.reserve(res.size() + list->values().size());
    std::ranges::copy(list->values(), std::back_inserter(res));
    return {make<List>(std::move(res)), std::move(env), false};
  }
  if (auto vector = to<Vector>(values[0])) {
    values = values.subspan(1);
    auto res = vector->values() | std::ranges::to<ValuesContainer>();
    res.reserve(res.size() + values.size());
    std::ranges::copy(values, std::back_inserter(res));
    return {make<Vector>(std::move(res)), std::move(env), false};
  }
  throwWrongArgument(name, values[0]);
}

InvocableResult meta(std::string_view name, ValuesSpan values, EnvPtr env) {
  checkArgsIs(name, values, 1);
  if (auto metaMixIn = to<MetaMixIn>(values[0])) {
    return {metaMixIn->meta(), std::move(env), false};
  }
  throwWrongArgument(name, values[0]);
}

InvocableResult with_meta(std::string_view name, ValuesSpan values, EnvPtr env) {
  checkArgsIs(name, values, 2);
  if (auto metaMixIn = to<MetaMixIn>(values[0])) {
    return {metaMixIn->cloneWithMeta(values[1]), std::move(env), false};
  }
  throwWrongArgument(name, values[0]);
}

InvocableResult fnQuestion(std::string_view name, ValuesSpan values, EnvPtr env) {
  checkArgsIs(name, values, 1);
  return {to<Invocable>(values[0]) && !to<Macro>(values[0])
              ? Constant::trueValue()
              : Constant::falseValue(),
          std::move(env), false};
}

} // namespace

namespace mal {
void prepareEnv(Env &env) {
  static std::array builtIns{
      BuiltIn{"atom?", typeQuestion<Atom>},
      BuiltIn{"keyword?", typeQuestion<Keyword>},
      BuiltIn{"list?", typeQuestion<List>},
      BuiltIn{"macro?", typeQuestion<Macro>},
      BuiltIn{"map?", typeQuestion<Hash>},
      BuiltIn{"number?", typeQuestion<Integer>},
      BuiltIn{"sequential?", typeQuestion<Sequence>},
      BuiltIn{"string?", typeQuestion<String>},
      BuiltIn{"symbol?", typeQuestion<Symbol>},
      BuiltIn{"vector?", typeQuestion<Vector>},
      BuiltIn{"+", addition},
      BuiltIn{"-", subtraction},
      BuiltIn{"*", multiplication},
      BuiltIn{"/", division},
      BuiltIn{"list", list},
      BuiltIn{"empty?", emptyQuestion},
      BuiltIn{"count", count},
      BuiltIn{"<", lt},
      BuiltIn{"<=", lte},
      BuiltIn{">", gt},
      BuiltIn{">=", gte},
      BuiltIn{"=", equal},
      BuiltIn{"not", not_},
      BuiltIn{"prn", prn},
      BuiltIn{"println", println},
      BuiltIn{"pr-str", pr_str},
      BuiltIn{"str", str},
      BuiltIn{"slurp", slurp},
      BuiltIn{"read-string", read_string},
      BuiltIn{"atom", atom},
      BuiltIn{"deref", deref},
      BuiltIn{"reset!", resetBang},
      BuiltIn{"swap!", swapBang},
      BuiltIn{"cons", cons},
      BuiltIn{"concat", concat},
      BuiltIn{"vec", vec},
      BuiltIn{"nth", nth},
      BuiltIn{"first", first},
      BuiltIn{"rest", rest},
      BuiltIn{"throw", throw_},
      BuiltIn{"map", map},
      BuiltIn{"nil?", nilQuestion},
      BuiltIn{"true?", trueQuestion},
      BuiltIn{"false?", falseQuestion},
      BuiltIn{"apply", apply},
      BuiltIn{"symbol", symbol},
      BuiltIn{"keyword", keyword},
      BuiltIn{"vector", vector},
      BuiltIn{"hash-map", hash_map},
      BuiltIn{"assoc", assoc},
      BuiltIn{"get", get},
      BuiltIn{"contains?", containsQuestion},
      BuiltIn{"keys", keys},
      BuiltIn{"vals", vals},
      BuiltIn{"dissoc", dissoc},
      BuiltIn{"readline", readline},
      BuiltIn{"time-ms", time_ms},
      BuiltIn{"seq", seq},
      BuiltIn{"conj", conj},
      BuiltIn{"meta", meta},
      BuiltIn{"with-meta", with_meta},
      BuiltIn{"fn?", fnQuestion},
  };

  for (auto &builtIn : builtIns) {
    env.insert_or_assign(
        builtIn.asKey(),
        std::shared_ptr<BuiltIn>(std::addressof(builtIn), [](auto &&) {}));
  }
}

} // namespace mal
