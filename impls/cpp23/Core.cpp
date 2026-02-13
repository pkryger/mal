#include "Core.h"
#include "Types.h"

#include <cstddef>
#include <iostream>
#include <format>
#include <ranges>
#include <span>
#include <string>
#include <utility>
#include <memory>
#include <algorithm>
#include <print>

void checkArgsIs(std::string name, MalValues values, std::size_t expected) {

  if (auto actual = values.size(); actual != expected) {
    throw CoreException{
        std::format("wrong number of '{}' arguments: {}, expected: {}",
                    std::move(name), actual, expected)};

  }
}

void checkArgsAtLeast(std::string name, MalValues values,
                      std::size_t expected) {
  if (auto actual = values.size(); actual < expected) {
    throw CoreException{
        std::format("wrong number of '{}' arguments: {}, expected: ({} . many)",
                    std::move(name), actual, expected)};
  }
}

void checkArgsBetween(std::string name, MalValues values,
                      std::size_t expectedMin, std::size_t expectedMax) {
  if (auto actual = values.size();
       expectedMax < actual || actual < expectedMin) {
    throw CoreException{
        std::format("wrong number of '{}' arguments: {}, expected: ({} . {})",
                    std::move(name), actual, expectedMin, expectedMax)};
  }
}

[[noreturn]]
void throwWrongArgument(std::string name, MalValuePtr val) {
  throw CoreException{
      std::format("wrong argument for '{}' call '{:r}'", std::move(name), val)};
}

namespace {

template <typename BINARY_OP, typename UNARY_OP>
MalValuePtr accumulateIntegers(std::string name, MalValues values,
                               BINARY_OP &&binary_op, UNARY_OP &&unary_op,
                               int minArgs = 1) {

  assert(minArgs > 0);
  checkArgsAtLeast(std::move(name), values, minArgs);
  if (auto init = dynamic_cast<MalInteger *>(values.front().get())) {
    if (values.size() == 1) {
      return std::make_shared<MalInteger>(
          std::forward<UNARY_OP>(unary_op)(init->value()));
    }
    return std::make_shared<MalInteger>(std::ranges::fold_left(
        values.subspan(1) | std::views::transform([&](auto elt) {
          if (auto integer = dynamic_cast<MalInteger *>(elt.get())) {
            return integer->value();
          }
          throwWrongArgument(std::move(name), elt);
        }),
        init->value(), std::forward<BINARY_OP>(binary_op)));
  }
  throwWrongArgument(std::move(name), values.front());
}


MalValuePtr addition(std::string name, MalValues values) {
  return accumulateIntegers(
      std::move(name), values,
      [](auto &&acc, auto &&v) noexcept { return acc + v; },
      [](auto &&v) noexcept { return v; });
}

MalValuePtr subtraction(std::string name, MalValues values) {
  return accumulateIntegers(
      std::move(name), values,
      [](auto &&acc, auto &&v) { return acc - v; },
      [](auto &&v) noexcept { return -v; });
}

MalValuePtr multiplication(std::string name, MalValues values) {
  return accumulateIntegers(
      std::move(name), values,
      [](auto &&acc, auto &&v) { return acc * v; },
      [](auto &&v) noexcept { return v; });
}

MalValuePtr division(std::string name, MalValues values) {
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

MalValuePtr list(std::string, MalValues values) {
  return std::make_shared<MalList>(MalValueVec{values.begin(), values.end()});
}

MalValuePtr listQuestion(std::string name, MalValues values) {
  checkArgsIs(      std::move(name), values, 1);
  if (dynamic_cast<MalList *>(values.front().get())) {
    return MalConstant::trueValue();
  }
  return MalConstant::falseValue();
}

MalValuePtr emptyQuestion(std::string name, MalValues values) {
  checkArgsIs(name, values, 1);
  if (auto sequence = dynamic_cast<MalSequence *>(values.front().get())) {
    return sequence->values().empty() ? MalConstant::trueValue()
                                      : MalConstant::falseValue();
  }
  throwWrongArgument(std::move(name), values.front());

}

MalValuePtr count(std::string name, MalValues values) {
  checkArgsAtLeast(name, values, 1);
  if (values.front().get() == MalConstant::nilValue().get()) {
    return std::make_shared<MalInteger>(0);
  }
  if (auto sequence = dynamic_cast<MalSequence *>(values.front().get())) {
    return std::make_shared<MalInteger>(sequence->values().size());
  }
  throwWrongArgument(std::move(name), values.front());
}

template <typename BINARY_OP>
MalValuePtr compareIntegers(std::string name, MalValues values,
                            BINARY_OP &&binary_op) {
  checkArgsAtLeast(name, values, 2);
  auto notMatching = std::ranges::adjacent_find(
      values,
      [binary_op = std::forward<BINARY_OP>(binary_op)](auto &&lhs, auto &&rhs) {
        return !binary_op(lhs, rhs);
      },
      [name = std::move(name)](auto &&elt) {
        if (auto integer = dynamic_cast<MalInteger *>(elt.get())) {
          return integer->value();
        }
        throwWrongArgument(std::move(name), elt);
      });
  return notMatching == values.end() ? MalConstant::trueValue()
                                     : MalConstant::falseValue();

}

MalValuePtr lt(std::string name, MalValues values) {
  return compareIntegers(
      std::move(name), values,
      [](auto &&lhs, auto &&rhs) noexcept { return lhs < rhs; });
}

MalValuePtr lte(std::string name, MalValues values) {
  return compareIntegers(
            std::move(name), values,
      [](auto &&lhs, auto &&rhs) noexcept { return lhs <= rhs; });
}

MalValuePtr gt(std::string name, MalValues values) {
  return compareIntegers(
            std::move(name), values,
      [](auto &&lhs, auto &&rhs) noexcept { return lhs > rhs; });
}

MalValuePtr gte(std::string name, MalValues values) {
  return compareIntegers(
            std::move(name), values,
      [](auto &&lhs, auto &&rhs) noexcept { return lhs >= rhs; });
}

MalValuePtr equal(std::string name, MalValues values) {
  checkArgsAtLeast(std::move(name), values, 2);
  auto notEqual =
      std::ranges::adjacent_find(values, [](auto &&lhs, auto &&rhs) {
        return !lhs->isEqualTo(rhs)->isTrue();
      });
  return notEqual == values.end() ? MalConstant::trueValue()
                                  : MalConstant::falseValue();
}

MalValuePtr not_(std::string name, MalValues values) {
  checkArgsIs(      std::move(name), values, 1);
  return values.front()->isTrue() ? MalConstant::falseValue()
                                  : MalConstant::trueValue();
}

MalValuePtr prn(std::string name, MalValues values) {
  std::cout << std::format("{:r}\n", values);
  return MalConstant::nilValue();
}

MalValuePtr println(std::string name, MalValues values) {
  std::print("{}\n", values);
  return MalConstant::nilValue();
}

MalValuePtr pr_str(std::string name, MalValues values) {
  return std::make_shared<MalString>(
      std::format("{:r}", values));
}

MalValuePtr str(std::string name, MalValues values) {
  return std::make_shared<MalString>(
      values |
      std::views::transform([](auto &&elt) { return std::format("{}", elt); }) |
      std::views::join | std::ranges::to<std::string>());
}

} // namespace


void installBuiltIns(MalEnv &env) {
  static MalValueVec builtIns{
      std::make_shared<MalBuiltIn>("+", &addition),
      std::make_shared<MalBuiltIn>("-", &subtraction),
      std::make_shared<MalBuiltIn>("*", &multiplication),
      std::make_shared<MalBuiltIn>("/", &division),
      std::make_shared<MalBuiltIn>("list", &list),
      std::make_shared<MalBuiltIn>("list?", &listQuestion),
      std::make_shared<MalBuiltIn>("empty?", &emptyQuestion),
      std::make_shared<MalBuiltIn>("count", &count),
      std::make_shared<MalBuiltIn>("<", &lt),
      std::make_shared<MalBuiltIn>("<=", &lte),
      std::make_shared<MalBuiltIn>(">", &gt),
      std::make_shared<MalBuiltIn>(">=", &gte),
      std::make_shared<MalBuiltIn>("=", &equal),
      std::make_shared<MalBuiltIn>("not", &not_),
      std::make_shared<MalBuiltIn>("prn", &prn),
      std::make_shared<MalBuiltIn>("println", &println),
      std::make_shared<MalBuiltIn>("pr-str", &pr_str),
      std::make_shared<MalBuiltIn>("str", &str),
  };

  for (auto &builtIn : builtIns) {
    env.insert_or_assign(dynamic_cast<MalBuiltIn *>(builtIn.get())->asKey(),
                         builtIn);

  }

}
