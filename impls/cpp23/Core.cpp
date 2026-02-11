#include "Core.h"
#include "Types.h"

#include <numeric>
#include <utility>
#include <memory>

namespace {

template <typename BINARY_OP, typename UNARY_OP>
MalValuePtr accumulateIntegers(MalValueIter first, MalValueIter last,
                               BINARY_OP &&binary_op, UNARY_OP &&unary_op) {

  if (first == last) {
    throw CoreException{"wrong number of arguments 0"};
  }
  if (auto init = dynamic_cast<MalInteger *>(first->get())) {
    ++first;
    if (first == last) {
      return std::make_shared<MalInteger>(
          std::forward<UNARY_OP>(unary_op)(init->value()));
    }
    return std::make_shared<MalInteger>(
        std::accumulate(first, last, init->value(), [&](auto &&acc, auto &&v) {
          return std::forward<BINARY_OP>(binary_op)(
              std::forward<decltype(acc)>(acc), [&]() {
                if (auto integer = dynamic_cast<MalInteger *>(v.get())) {
                  return integer->value();
                }
                throw CoreException("wrong argument: " + v->print(true));
              }());
        }));
  }
  throw CoreException{"wrong argument: " + (*first)->print(true)};
}


MalValuePtr addition(MalValueIter first, MalValueIter last) {
  return accumulateIntegers(
      first, last, [](auto &&acc, auto &&v) noexcept { return acc + v; },
      [](auto &&v) noexcept { return v; });
}

MalValuePtr subtraction(MalValueIter first, MalValueIter last) {
  return accumulateIntegers(
      first, last, [](auto &&acc, auto &&v) { return acc - v; },
      [](auto &&v) noexcept { return -v; });
}

MalValuePtr multiplication(MalValueIter first, MalValueIter last) {
  return accumulateIntegers(
      first, last, [](auto &&acc, auto &&v) { return acc * v; },
      [](auto &&v) noexcept { return v; });
}

MalValuePtr division(MalValueIter first, MalValueIter last) {
  return accumulateIntegers(
      first, last,
      [](auto &&acc, auto &&v) {
        if (v == 0ll)
          throw CoreException{"arith error"};
        return acc / v;
      },
      [](auto &&) -> std::int64_t {
        throw CoreException{"wrong number of arguments 1"};
      });
}

} // namespace

void installBuiltIns(MalEnv &env) {
  auto nilDeleter = [](auto&& ) noexcept {};
  static MalBuiltIn _addition = MalBuiltIn{"+", &addition};
  static MalBuiltIn _subtraction = MalBuiltIn{"-", &subtraction};
  static MalBuiltIn _multiplication = MalBuiltIn{"*", &multiplication};
  static MalBuiltIn _division = MalBuiltIn{"/", &division};
  static MalValueVec builtIns {
    MalValuePtr{std::addressof(_addition), nilDeleter},
    MalValuePtr{std::addressof(_subtraction), nilDeleter},
    MalValuePtr{std::addressof(_multiplication), nilDeleter},
    MalValuePtr{std::addressof(_division), nilDeleter},
  };

  for (auto &builtIn : builtIns) {
    env.insert_or_assign(builtIn->print(false), builtIn);
  }

}
