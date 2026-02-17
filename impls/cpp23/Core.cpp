#include "Core.h"
#include "Types.h"

#include <cstddef>
#include <iostream>
#include <format>
#include <ranges>
#include <string>
#include <utility>
#include <memory>
#include <algorithm>
#include <print>

namespace mal {
void checkArgsIs(std::string name, ValuesSpan values, std::size_t expected) {

  if (auto actual = values.size(); actual != expected) {
    throw CoreException{
        std::format("wrong number of '{}' arguments: {}, expected: {}",
                    std::move(name), actual, expected)};

  }
}

void checkArgsAtLeast(std::string name, ValuesSpan values,
                      std::size_t expected) {
  if (auto actual = values.size(); actual < expected) {
    throw CoreException{
        std::format("wrong number of '{}' arguments: {}, expected: ({} . many)",
                    std::move(name), actual, expected)};
  }
}

void checkArgsBetween(std::string name, ValuesSpan values,
                      std::size_t expectedMin, std::size_t expectedMax) {
  if (auto actual = values.size();
       expectedMax < actual || actual < expectedMin) {
    throw CoreException{
        std::format("wrong number of '{}' arguments: {}, expected: ({} . {})",
                    std::move(name), actual, expectedMin, expectedMax)};
  }
}

[[noreturn]]
void throwWrongArgument(std::string name, ValuePtr val) {
  throw CoreException{
      std::format("wrong argument for '{}' call '{:r}'", std::move(name), val)};
}

} // namespace mal

namespace {

using mal::Constant;
using mal::CoreException;
using mal::Integer;
using mal::List;
using mal::Sequence;
using mal::String;
using mal::ValuePtr;
using mal::ValuesSpan;
using mal::ValuesContainer;

template <typename BINARY_OP, typename UNARY_OP>
ValuePtr accumulateIntegers(std::string name, ValuesSpan values,
                            BINARY_OP &&binary_op, UNARY_OP &&unary_op,
                            int minArgs = 1) {

  assert(minArgs > 0);
  checkArgsAtLeast(std::move(name), values, minArgs);
  if (auto init = dynamic_cast<Integer *>(values.front().get())) {
    if (values.size() == 1) {
      return std::make_shared<Integer>(
          std::forward<UNARY_OP>(unary_op)(init->value()));
    }
    return std::make_shared<Integer>(std::ranges::fold_left(
        values.subspan(1) | std::views::transform([&](auto elt) {
          if (auto integer = dynamic_cast<Integer *>(elt.get())) {
            return integer->value();
          }
          throwWrongArgument(std::move(name), elt);
        }),
        init->value(), std::forward<BINARY_OP>(binary_op)));
  }
  throwWrongArgument(std::move(name), values.front());
}

ValuePtr addition(std::string name, ValuesSpan values) {
  return accumulateIntegers(
      std::move(name), values,
      [](auto &&acc, auto &&v) noexcept { return acc + v; },
      [](auto &&v) noexcept { return v; });
}

ValuePtr subtraction(std::string name, ValuesSpan values) {
  return accumulateIntegers(
      std::move(name), values,
      [](auto &&acc, auto &&v) { return acc - v; },
      [](auto &&v) noexcept { return -v; });
}

ValuePtr multiplication(std::string name, ValuesSpan values) {
  return accumulateIntegers(
      std::move(name), values,
      [](auto &&acc, auto &&v) { return acc * v; },
      [](auto &&v) noexcept { return v; });
}

ValuePtr division(std::string name, ValuesSpan values) {
  return accumulateIntegers(
      name, values,
      [&](auto &&acc, auto &&v) {
        if (v == 0ll)
          throw CoreException{std::format("'{}' division by 0", name)};
        return acc / v;
      },
      [&](auto &&) -> std::int64_t {
        throw CoreException{std::format(
            "wrong number of '{}' arguments: 1, expected (2 . many)", name)};

      },
      2);
}

ValuePtr list(std::string, ValuesSpan values) {
  return std::make_shared<List>(ValuesContainer{values.begin(), values.end()});
}

ValuePtr listQuestion(std::string name, ValuesSpan values) {
  checkArgsIs(      std::move(name), values, 1);
  if (dynamic_cast<List *>(values.front().get())) {
    return Constant::trueValue();
  }
  return Constant::falseValue();
}

ValuePtr emptyQuestion(std::string name, ValuesSpan values) {
  checkArgsIs(name, values, 1);
  if (auto sequence = dynamic_cast<Sequence *>(values.front().get())) {
    return sequence->values().empty() ? Constant::trueValue()
                                      : Constant::falseValue();
  }
  throwWrongArgument(std::move(name), values.front());

}

ValuePtr count(std::string name, ValuesSpan values) {
  checkArgsAtLeast(name, values, 1);
  if (values.front().get() == Constant::nilValue().get()) {
    return std::make_shared<Integer>(0);
  }
  if (auto sequence = dynamic_cast<Sequence *>(values.front().get())) {
    return std::make_shared<Integer>(sequence->values().size());
  }
  throwWrongArgument(std::move(name), values.front());
}

template <typename BINARY_OP>
ValuePtr compareIntegers(std::string name, ValuesSpan values,
                            BINARY_OP &&binary_op) {
  checkArgsAtLeast(name, values, 2);
  auto notMatching = std::ranges::adjacent_find(
      values,
      [binary_op = std::forward<BINARY_OP>(binary_op)](auto &&lhs, auto &&rhs) {
        return !binary_op(lhs, rhs);
      },
      [name = std::move(name)](auto &&elt) {
        if (auto integer = dynamic_cast<Integer *>(elt.get())) {
          return integer->value();
        }
        throwWrongArgument(std::move(name), elt);
      });
  return notMatching == values.end() ? Constant::trueValue()
                                     : Constant::falseValue();

}

ValuePtr lt(std::string name, ValuesSpan values) {
  return compareIntegers(
      std::move(name), values,
      [](auto &&lhs, auto &&rhs) noexcept { return lhs < rhs; });
}

ValuePtr lte(std::string name, ValuesSpan values) {
  return compareIntegers(
            std::move(name), values,
      [](auto &&lhs, auto &&rhs) noexcept { return lhs <= rhs; });
}

ValuePtr gt(std::string name, ValuesSpan values) {
  return compareIntegers(
            std::move(name), values,
      [](auto &&lhs, auto &&rhs) noexcept { return lhs > rhs; });
}

ValuePtr gte(std::string name, ValuesSpan values) {
  return compareIntegers(
            std::move(name), values,
      [](auto &&lhs, auto &&rhs) noexcept { return lhs >= rhs; });
}

ValuePtr equal(std::string name, ValuesSpan values) {
  checkArgsAtLeast(std::move(name), values, 2);
  auto notEqual =
      std::ranges::adjacent_find(values, [](auto &&lhs, auto &&rhs) {
        return !lhs->isEqualTo(rhs)->isTrue();
      });
  return notEqual == values.end() ? Constant::trueValue()
                                  : Constant::falseValue();
}

ValuePtr not_(std::string name, ValuesSpan values) {
  checkArgsIs(      std::move(name), values, 1);
  return values.front()->isTrue() ? Constant::falseValue()
                                  : Constant::trueValue();
}

ValuePtr prn(std::string name, ValuesSpan values) {
  std::cout << std::format("{:r}\n", values);
  return Constant::nilValue();
}

ValuePtr println(std::string name, ValuesSpan values) {
  std::print("{}\n", values);
  return Constant::nilValue();
}

ValuePtr pr_str(std::string name, ValuesSpan values) {
  return std::make_shared<String>(
      std::format("{:r}", values));
}

ValuePtr str(std::string name, ValuesSpan values) {
  return std::make_shared<String>(
      values |
      std::views::transform([](auto &&elt) { return std::format("{}", elt); }) |
      std::views::join | std::ranges::to<std::string>());
}

} // namespace

namespace mal {
void installBuiltIns(Env &env) {
  static ValuesContainer builtIns{
      std::make_shared<BuiltIn>("+", &addition),
      std::make_shared<BuiltIn>("-", &subtraction),
      std::make_shared<BuiltIn>("*", &multiplication),
      std::make_shared<BuiltIn>("/", &division),
      std::make_shared<BuiltIn>("list", &list),
      std::make_shared<BuiltIn>("list?", &listQuestion),
      std::make_shared<BuiltIn>("empty?", &emptyQuestion),
      std::make_shared<BuiltIn>("count", &count),
      std::make_shared<BuiltIn>("<", &lt),
      std::make_shared<BuiltIn>("<=", &lte),
      std::make_shared<BuiltIn>(">", &gt),
      std::make_shared<BuiltIn>(">=", &gte),
      std::make_shared<BuiltIn>("=", &equal),
      std::make_shared<BuiltIn>("not", &not_),
      std::make_shared<BuiltIn>("prn", &prn),
      std::make_shared<BuiltIn>("println", &println),
      std::make_shared<BuiltIn>("pr-str", &pr_str),
      std::make_shared<BuiltIn>("str", &str),
  };

  for (auto &builtIn : builtIns) {
    env.insert_or_assign(dynamic_cast<BuiltIn *>(builtIn.get())->asKey(),
                         builtIn);
  }
}

} // namespace mal
