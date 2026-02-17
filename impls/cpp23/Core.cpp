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
using mal::make;
using mal::Sequence;
using mal::String;
using mal::to;
using mal::ValuePtr;
using mal::ValuesContainer;
using mal::ValuesSpan;

template <typename BINARY_OP, typename UNARY_OP>
ValuePtr accumulateIntegers(std::string name, ValuesSpan values,
                            BINARY_OP &&binary_op, UNARY_OP &&unary_op,
                            int minArgs = 1) {

  assert(minArgs > 0);
  checkArgsAtLeast(std::move(name), values, minArgs);
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
  return make<List>(ValuesContainer{values.begin(), values.end()});
}

ValuePtr listQuestion(std::string name, ValuesSpan values) {
  checkArgsIs(      std::move(name), values, 1);
  if (to<List>(values.front())) {
    return Constant::trueValue();
  }
  return Constant::falseValue();
}

ValuePtr emptyQuestion(std::string name, ValuesSpan values) {
  checkArgsIs(name, values, 1);
  if (auto sequence = to<Sequence>(values.front())) {
    return sequence->values().empty() ? Constant::trueValue()
                                      : Constant::falseValue();
  }
  throwWrongArgument(std::move(name), values.front());

}

ValuePtr count(std::string name, ValuesSpan values) {
  checkArgsAtLeast(name, values, 1);
  if (values.front().get() == Constant::nilValue().get()) {
    return make<Integer>(0);
  }
  if (auto sequence = to<Sequence>(values.front())) {
    return make<Integer>(sequence->values().size());
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
        if (auto integer = to<Integer>(elt)) {
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
  return make<String>(
      std::format("{:r}", values));
}

ValuePtr str(std::string name, ValuesSpan values) {
  return make<String>(
      values |
      std::views::transform([](auto &&elt) { return std::format("{}", elt); }) |
      std::views::join | std::ranges::to<std::string>());
}

} // namespace

namespace mal {
void installBuiltIns(Env &env) {
  static ValuesContainer builtIns{
      make<BuiltIn>("+", &addition),
      make<BuiltIn>("-", &subtraction),
      make<BuiltIn>("*", &multiplication),
      make<BuiltIn>("/", &division),
      make<BuiltIn>("list", &list),
      make<BuiltIn>("list?", &listQuestion),
      make<BuiltIn>("empty?", &emptyQuestion),
      make<BuiltIn>("count", &count),
      make<BuiltIn>("<", &lt),
      make<BuiltIn>("<=", &lte),
      make<BuiltIn>(">", &gt),
      make<BuiltIn>(">=", &gte),
      make<BuiltIn>("=", &equal),
      make<BuiltIn>("not", &not_),
      make<BuiltIn>("prn", &prn),
      make<BuiltIn>("println", &println),
      make<BuiltIn>("pr-str", &pr_str),
      make<BuiltIn>("str", &str),
  };

  for (auto &builtIn : builtIns) {
    env.insert_or_assign(to<BuiltIn>(builtIn)->asKey(),
                         builtIn);
  }
}

} // namespace mal
