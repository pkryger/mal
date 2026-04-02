#include "Core.h" // IWYU pragma: associated
#include "Env.h"
#include "Mal.h"
#if !defined(__cpp_lib_ranges_stride)
#include "Ranges.h"
#endif // __cpp_lib_ranges_stride
#include "ReadLine.h"
#include "Reader.h"
#include "Types.h"

#include <algorithm>
#include <array>
#include <cassert>
#include <chrono>
#include <cstddef>
#include <cstdint>
#include <cstdlib>
#include <filesystem>
#include <format>
#include <fstream>
#include <ios>
#include <iterator>
#include <limits>
#include <memory>
#include <optional>
#include <print>
#include <ranges>
#include <string>
#include <string_view>
#include <tuple>
#include <utility>
// IWYU pragma: no_include <__vector/vector.h>
// IWYU pragma: no_include <_stdlib.h>

namespace mal {

void checkArgsIs(std::string_view name, std::size_t actual,
                 std::size_t expected) {
  if (actual != expected) {
    throw CoreException{
        std::format("wrong number of '{}' arguments: {}, expected: {}", name,
                    actual, expected)};

  }
}

void checkArgsIs(std::string_view name, ValuesSpan values,
                 std::size_t expected) {
  return checkArgsIs(name, values.size(), expected);
}

void checkArgsAtLeast(std::string_view name, std::size_t actual,
                      std::size_t expected) {
  if (actual < expected) {
    throw CoreException{
        std::format("wrong number of '{}' arguments: {}, expected: ({} . many)",
                    name, actual, expected)};
  }
}

void checkArgsAtLeast(std::string_view name, ValuesSpan values,
                      std::size_t expected) {
  return checkArgsAtLeast(name, values.size(), expected);
}

void checkArgsBetween(std::string_view name, std::size_t actual,
                      std::size_t expectedMin, std::size_t expectedMax) {
  if (expectedMax < actual || actual < expectedMin) {
    throw CoreException{
        std::format("wrong number of '{}' arguments: {}, expected: ({} . {})",
                    name, actual, expectedMin, expectedMax)};
  }
}

void checkArgsBetween(std::string_view name, ValuesSpan values,
                      std::size_t expectedMin, std::size_t expectedMax) {
  return checkArgsBetween(name, values.size(), expectedMin, expectedMax);
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
using mal::ReadLine;
using mal::readStr;
using mal::Sequence;
using mal::String;
using mal::StringBase;
using mal::Symbol;
using mal::ValuePtr;
using mal::ValuesContainer;
using mal::ValuesSpan;
using mal::Vector;

template <typename TYPE>
InvocableResult typeQuestion(std::string_view name, ValuesSpan values,
                             const EnvPtr &/* env */) {
  checkArgsIs(name, values, 1);
  return {values[0]->isa<TYPE>() ? Constant::trueValue()
                                 : Constant::falseValue(),
          {}};
}

namespace detail {

template <typename BINARY_OP, typename UNARY_OP>
ValuePtr accumulateIntegers(std::string_view name, ValuesSpan values,
                            BINARY_OP &&binary_op, UNARY_OP &&unary_op,
                            std::size_t minArgs = 1) {
  assert(minArgs > 0);
  checkArgsAtLeast(name, values, minArgs);
  if (auto init = values.front()->dyncast<Integer>()) {
    if (values.size() == 1) {
      return make<Integer>(
          std::forward<UNARY_OP>(unary_op)(init->value()));
    }
    return make<Integer>(std::ranges::fold_left(
        values.subspan(1) | std::views::transform([&](auto &&elt) {
          if (auto integer = elt->template dyncast<Integer>()) {
            return integer->value();
          }
          throwWrongArgument(name, elt);
        }),
        init->value(), std::forward<BINARY_OP>(binary_op)));
  }
  throwWrongArgument(name, values.front());
}

} // namespace detail

InvocableResult addition(std::string_view name, ValuesSpan values,
                         const EnvPtr &/* env */) {
  return {detail::accumulateIntegers(
              name, values,
              [](auto &&acc, auto &&v) noexcept { return acc + v; },
              [](auto &&v) noexcept { return v; }),
          {}};
}

InvocableResult subtraction(std::string_view name, ValuesSpan values,
                            const EnvPtr &/* env */) {
  return {detail::accumulateIntegers(
              name, values, [](auto &&acc, auto &&v) { return acc - v; },
              [](auto &&v) noexcept { return -v; }),
          {}};
}

InvocableResult multiplication(std::string_view name, ValuesSpan values,
                               const EnvPtr &/* env */) {
  return {detail::accumulateIntegers(
              name, values, [](auto &&acc, auto &&v) { return acc * v; },
              [](auto &&v) noexcept { return v; }),
          {}};
}

InvocableResult division(std::string_view name, ValuesSpan values,
                         const EnvPtr &/* env */) {
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
          {}};
}

InvocableResult list(std::string_view /* name */, ValuesSpan values,
                     const EnvPtr &/* env */) {
  return {make<List>(values), {}};
}

InvocableResult emptyQuestion(std::string_view name, ValuesSpan values,
                              const EnvPtr &/* env */) {
  checkArgsIs(name, values, 1);
  if (auto sequence = values.front()->dyncast<Sequence>()) {
    return {sequence->values().empty() ? Constant::trueValue()
                                       : Constant::falseValue(),
            {}};
  }
  throwWrongArgument(name, values.front());

}

InvocableResult count(std::string_view name, ValuesSpan values,
                      const EnvPtr &/* env */) {
  checkArgsAtLeast(name, values, 1);
  if (values.front().get() == Constant::nilValue().get()) {
    return {make<Integer>(0), {}};
  }
  if (auto sequence = values.front()->dyncast<Sequence>()) {
    return {make<Integer>(sequence->values().size()), {}};
  }
  throwWrongArgument(name, values.front());
}

template <typename BINARY_OP>
InvocableResult compareIntegers(std::string_view name, ValuesSpan values,
                                const EnvPtr & /* env */,
                                BINARY_OP &&binary_op) {
  checkArgsAtLeast(name, values, 2);
  auto notMatching = std::ranges::adjacent_find(
      values,
      [binary_op = std::forward<BINARY_OP>(binary_op)](auto &&lhs, auto &&rhs) {
        return !binary_op(lhs, rhs);
      },
      [name = name](auto &&elt) {
        if (auto integer = elt->template dyncast<Integer>()) {
          return integer->value();
        }
        throwWrongArgument(name, elt);
      });
  return {notMatching == values.end() ? Constant::trueValue()
                                      : Constant::falseValue(),
          {}};

}

InvocableResult lt(std::string_view name, ValuesSpan values,
                   const EnvPtr &env) {
  return compareIntegers(
      name, values, env,
      [](auto &&lhs, auto &&rhs) noexcept { return lhs < rhs; });
}

InvocableResult lte(std::string_view name, ValuesSpan values,
                    const EnvPtr &env) {
  return compareIntegers(
      name, values, env,
      [](auto &&lhs, auto &&rhs) noexcept { return lhs <= rhs; });
}

InvocableResult gt(std::string_view name, ValuesSpan values,
                   const EnvPtr &env) {
  return compareIntegers(
      name, values, env,
      [](auto &&lhs, auto &&rhs) noexcept { return lhs > rhs; });
}

InvocableResult gte(std::string_view name, ValuesSpan values,
                    const EnvPtr &env) {
  return compareIntegers(
      name, values, env,
      [](auto &&lhs, auto &&rhs) noexcept { return lhs >= rhs; });
}

InvocableResult equal(std::string_view name, ValuesSpan values,
                      const EnvPtr &/* env */) {
  checkArgsAtLeast(name, values, 2);
  auto notEqual =
      std::ranges::adjacent_find(values, [](auto &&lhs, auto &&rhs) {
        return !lhs->isEqualTo(rhs)->isTrue();
      });
  return {notEqual == values.end() ? Constant::trueValue()
                                   : Constant::falseValue(),
          {}};
}

InvocableResult not_(std::string_view name, ValuesSpan values,
                     const EnvPtr &/* env */) {
  checkArgsIs(name, values, 1);
  return {values.front()->isTrue() ? Constant::falseValue()
                                   : Constant::trueValue(),
          {}};
}

InvocableResult prn(std::string_view /* name */, ValuesSpan values,
                    const EnvPtr &/* env */) {
  std::print("{:r}\n", values);
  return {Constant::nilValue(), {}};
}

InvocableResult println(std::string_view /* name */, ValuesSpan values,
                        const EnvPtr &/* env */) {
  std::print("{}\n", values);
  return {Constant::nilValue(), {}};
}

InvocableResult pr_str(std::string_view /* name */, ValuesSpan values,
                       const EnvPtr & /* env */) {
  static constexpr std::string_view malImpl{"cpp23"};
  if (auto envImpl =
      // NOLINTNEXTLINE(concurrency-mt-unsafe) - single threaded
      std::getenv("MAL_IMPL");
      envImpl && envImpl == malImpl) {
    return {make<String>(std::format("{:l}", values)), {}};
  }
  return {make<String>(std::format("{:r}", values)), {}};
}

InvocableResult str(std::string_view /* name */, ValuesSpan values,
                    const EnvPtr &/* env */) {
  return {make<String>(values | std::views::transform([](auto &&elt) {
                         return std::format("{}", elt);
                       }) |
                       std::views::join | std::ranges::to<std::string>()),
          {}};
}

InvocableResult slurp(std::string_view name, ValuesSpan values,
                      const EnvPtr &/* env */) {
  checkArgsIs(name, values, 1);
  if (auto string = values[0]->dyncast<String>()) {
    auto path = string->data();
    auto file_size = std::filesystem::file_size(path);
    if (file_size >
        static_cast<uintmax_t>(std::numeric_limits<std::streamsize>::max())) {
      throw CoreException(
          std::format("{}: file {} is too big", name, std::move(path)));
    }
    std::ifstream file{path, std::ios::in | std::ios::binary};
    if (!file) {
      throw CoreException(std::format("{}: file {} open error",
                                      name, std::move(path)));
    }
    std::string content(file_size, '\0');
    file.read(content.data(), static_cast<std::streamsize>(file_size));
    return {make<String>(std::move(content)), {}};
  }
   throwWrongArgument(name, values[0]);
}

InvocableResult read_string(std::string_view name, ValuesSpan values,
                            const EnvPtr &/* env */) {
  checkArgsIs(name, values, 1);
  if (values[0]->isa<String>()) {
    return {readStr(std::format("{}", values[0])), {}};
  }
  throwWrongArgument(name, values[0]);
}

InvocableResult atom(std::string_view name, ValuesSpan values,
                     const EnvPtr &/* env */) {
  checkArgsIs(name, values, 1);
  return {make<Atom>(values[0]), {}};
}

InvocableResult deref(std::string_view name, ValuesSpan values,
                      const EnvPtr &/* env */) {
  checkArgsIs(name, values, 1);
  if (auto atom = values[0]->dyncast<Atom>()) {
    return {atom->value(), {}};
  }
  throwWrongArgument(name, values[0]);
}

InvocableResult resetBang(std::string_view name, ValuesSpan values,
                          const EnvPtr &/* env */) {
  checkArgsIs(name, values, 2);
  if (auto atom = values[0]->dyncast<Atom>()) {
    return {atom->reset(values[1]), {}};
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

InvocableResult swapBang(std::string_view name, ValuesSpan values,
                         const EnvPtr &env) {
  assert(!EvalFnStack::empty());
  checkArgsAtLeast(name, values, 2);
  if (auto atom = values[0]->dyncast<Atom>()) {
    if (auto fn = values[1]->dyncast<Invocable>()) {
      auto args = detail::cons(atom->value(), values.subspan(2));
      auto [ast, evalEnv] = fn->apply(true, ValuesSpan{args}, env);
      if (evalEnv) {
        ast = EvalFnStack::top()(ast, *evalEnv);
      }
      return {atom->reset(ast), {}};
    }
    throwWrongArgument(name, values[1]);
  }
  throwWrongArgument(name, values[0]);
}

InvocableResult cons(std::string_view name, ValuesSpan values,
                     const EnvPtr &/* env */) {
  checkArgsIs(name, values, 2);
  if (auto sequence = values[1]->dyncast<Sequence>()) {
    return {make<List>(detail::cons(values[0], sequence->values())),
            {}};
  }
  return {make<List>(values), {}};
}

InvocableResult concat(std::string_view name, ValuesSpan values,
                       const EnvPtr &/* env */) {
  return {make<List>(values | std::views::transform([&](auto &&elt) {
                       if (auto sequence = elt->template dyncast<Sequence>()) {
                         return sequence->values();
                       }
                       throwWrongArgument(name, elt);
                     }) |
                     std::views::join),
          {}};
}

InvocableResult vec(std::string_view name, ValuesSpan values,
                    const EnvPtr &/* env */) {
  checkArgsIs(name, values, 1);
  if (values[0]->isa<Vector>()) {
    return {values[0], {}};
  }
  if (auto list = values[0]->dyncast<List>()) {
    return {make<Vector>(list->values()), {}};
  }
  throwWrongArgument(name, values[0]);
}

InvocableResult nth(std::string_view name, ValuesSpan values,
                    const EnvPtr &/* env */) {
  checkArgsIs(name, values, 2);
  if (auto integer = values[1]->dyncast<Integer>()) {
    static_assert(sizeof(decltype(std::declval<Integer>().value())) >=
                  sizeof(ValuesSpan::size_type));
    auto index = static_cast<ValuesSpan::size_type>(integer->value());
    if (auto sequence = values[0]->dyncast<Sequence>()) {
      values = sequence->values();
      if (0 <= index && index < values.size()) {
        return {values[index], {}};
      }
      throw CoreException{std::format("index out of bounds {} for '{}'", index,
                                      name)};
    }
    if (auto constant = values[0]->dyncast<Constant>();
        constant && index == 0 && Constant::nilValue()->isEqualTo(values[0])) {
      return {Constant::nilValue(), {}};
    }
    throwWrongArgument(name, values[0]);
  }
  throwWrongArgument(name, values[1]);
}

InvocableResult first(std::string_view name, ValuesSpan values,
                      const EnvPtr &/* env */) {
  checkArgsIs(name, values, 1);
  if (auto sequence = values[0]->dyncast<Sequence>()) {
    values = sequence->values();
    if (!values.empty()) {
      return {values[0], {}};
    }
    return {Constant::nilValue(), {}};
  }
  if (auto constant = values[0]->dyncast<Constant>();
      constant && Constant::nilValue()->isEqualTo(values[0])) {
    return {Constant::nilValue(), {}};
  }
  throwWrongArgument(name, values[0]);
}

InvocableResult rest(std::string_view name, ValuesSpan values,
                     const EnvPtr &/* env */) {
  checkArgsIs(name, values, 1);
  if (auto sequence = values[0]->dyncast<Sequence>()) {
    values = sequence->values();
    if (!values.empty()) {
      return {make<List>(values.subspan(1)), {}};
    }
    return {make<List>(), {}};
  }
  if (auto constant = values[0]->dyncast<Constant>();
      constant && Constant::nilValue()->isEqualTo(values[0])) {
    return {make<List>(), {}};
  }
  throwWrongArgument(name, values[0]);
}

[[noreturn]]
InvocableResult throw_(std::string_view name, ValuesSpan values,
                       const EnvPtr &/* env */) {
  checkArgsIs(name, values, 1);
  throw MalException{std::format("Exception: {:r}", values[0]), values[0]};
}

InvocableResult map(std::string_view name, ValuesSpan values,
                    const EnvPtr &env) {
  assert(!EvalFnStack::empty());
  checkArgsIs(name, values, 2);
  if (auto invocable = values[0]->dyncast<Invocable>()) {
    if (auto sequence = values[1]->dyncast<Sequence>()) {
      return {
        make<List>(sequence->values() |
                   std::views::transform([&](auto &&value) {
                     auto [ast, evalEnv] = invocable->apply(
                         true, ValuesSpan{std::addressof(value), 1}, env);
                if (evalEnv) {
                  return EvalFnStack::top()(std::move(ast), *evalEnv);
                }
                return ast;
              })),
          {}};
    }
    throwWrongArgument(name, values[1]);
  }
  throwWrongArgument(name, values[0]);
}

InvocableResult apply(std::string_view name, ValuesSpan values,
                      const EnvPtr &env) {
  checkArgsAtLeast(name, values, 2);
  if (auto invocable = values[0]->dyncast<Invocable>()) {
    ValuesContainer argsContainer;
    auto args = [&]() {
      if (auto sequence = values.back()->dyncast<Sequence>()) {
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
    return invocable->apply(true, args, env);
  }
  throwWrongArgument(name, values[0]);
}

namespace detail {

InvocableResult constantQuestion(std::string_view name, ValuesSpan values,
                                 const ValuePtr &constant) {
  checkArgsIs(name, values, 1);
  return {constant->isEqualTo(values[0]), {}};
}

} // namespace detail

InvocableResult nilQuestion(std::string_view name, ValuesSpan values,
                     const EnvPtr &/* env */) {
  return detail::constantQuestion(name, values, Constant::nilValue());
}

InvocableResult trueQuestion(std::string_view name, ValuesSpan values,
                     const EnvPtr &/* env */) {
  return detail::constantQuestion(name, values, Constant::trueValue());
}

InvocableResult falseQuestion(std::string_view name, ValuesSpan values,
                     const EnvPtr &/* env */) {
  return detail::constantQuestion(name, values, Constant::falseValue());
}

InvocableResult symbol(std::string_view name, ValuesSpan values,
                       const EnvPtr &/* env */) {
  checkArgsIs(name, values, 1);
  if (auto string = values[0]->dyncast<String>()) {
    return {make<Symbol>(string->data()), {}};
  }
  throwWrongArgument(name, values[0]);
}

InvocableResult keyword(std::string_view name, ValuesSpan values,
                        const EnvPtr &/* env */) {
  checkArgsIs(name, values, 1);
  if (values[0]->isa<Keyword>()) {
    return {values[0], {}};
  }
  if (auto string = values[0]->dyncast<String>()) {
    return {make<Keyword>(std::format(":{}", string->data())), {}};
  }
  throwWrongArgument(name, values[0]);
}

InvocableResult vector(std::string_view /*name*/, ValuesSpan values,
                const EnvPtr &/* env */) {

  return {make<Vector>(values), {}};
}

InvocableResult hash_map(std::string_view name, ValuesSpan values,
                  const EnvPtr &/* env */) {
  if (values.size() % 2 == 0) {
    for (auto &&key : values |
#ifdef __cpp_lib_ranges_stride
                          std::views::stride(2)
#else
                          mal::views::Stride(2)
#endif // __cpp_lib_ranges_stride
    ) {
      if (!key->isa<StringBase>()) {
        mal::throwWrongArgument(name, key);
      }
    }
    return {make<Hash>(values), {}};
  }
  throw CoreException{
      std::format("odd number of arguments for '{}', {}", name, values.size())};
}

InvocableResult assoc(std::string_view name, ValuesSpan values,
                      const EnvPtr &/* env */) {
  checkArgsAtLeast(name, values, 1);
  if (values.size() % 2 == 1) {
    if (auto hash = values[0]->dyncast<Hash>()) {
      for (auto &&key : values.subspan(1) |
#ifdef __cpp_lib_ranges_stride
                            std::views::stride(2)
#else
                            mal::views::Stride(2)
#endif // __cpp_lib_ranges_stride
        ) {
        if (!key->isa<StringBase>()) {
          mal::throwWrongArgument(name, key);
        }
      }
      return {make<Hash>(*hash, values.subspan(1)), {}};
    }
    throwWrongArgument(name, values[0]);
  }
  throw CoreException{std::format("odd number of arguments for '{}', {}", name,
                                  values.size() - 1)};
}

InvocableResult get(std::string_view name, ValuesSpan values,
                    const EnvPtr &/* env */) {
  checkArgsIs(name, values, 2);
  if (Constant::nilValue()->isEqualTo(values[0])->isTrue()) {
    return {Constant::nilValue(), {}};
  }
  if (auto hash = values[0]->dyncast<Hash>()) {
    if (values[1]->isa<StringBase>()) {
      if (auto res = hash->find(values[1]))
        return {res, {}};
      else {
        return {Constant::nilValue(), {}};
      }
    }
    throwWrongArgument(name, values[1]);
  }
  throwWrongArgument(name, values[0]);
}

InvocableResult containsQuestion(std::string_view name, ValuesSpan values,
                                 const EnvPtr &/* env */) {
  checkArgsIs(name, values, 2);
  if (Constant::nilValue()->isEqualTo(values[0])->isTrue()) {
    return {Constant::nilValue(), {}};
  }
  if (auto hash = values[0]->dyncast<Hash>()) {
    if (values[1]->isa<StringBase>()) {
      if (hash->find(values[1]))
        return {Constant::trueValue(), {}};
      else {
        return {Constant::falseValue(), {}};
      }
    }
    throwWrongArgument(name, values[1]);
  }
  throwWrongArgument(name, values[0]);
}

InvocableResult keys(std::string_view name, ValuesSpan values,
                     const EnvPtr &/* env */) {
  checkArgsIs(name, values, 1);
  if (Constant::nilValue()->isEqualTo(values[0])->isTrue()) {
    return {Constant::nilValue(), {}};
  }
  if (auto hash = values[0]->dyncast<Hash>()) {
    return {make<List>(*hash | std::views::transform(
                                   [](auto &&elt) { return elt.first; })),
            {}};
  }
  throwWrongArgument(name, values[0]);
}

InvocableResult vals(std::string_view name, ValuesSpan values,
                     const EnvPtr &/* env */) {
  checkArgsIs(name, values, 1);
  if (Constant::nilValue()->isEqualTo(values[0])->isTrue()) {
    return {Constant::nilValue(), {}};
  }
  if (auto hash = values[0]->dyncast<Hash>()) {
    return {make<List>(*hash | std::views::transform(
                                   [](auto &&elt) { return elt.second; })),
            {}};
  }
  throwWrongArgument(name, values[0]);
}

InvocableResult dissoc(std::string_view name, ValuesSpan values,
                       const EnvPtr &/* env */) {
  checkArgsAtLeast(name, values, 1);
  if (auto hash = values[0]->dyncast<Hash>()) {
    values = values.subspan(1);
    for (auto &&key : values) {
      if (!key->isa<StringBase>()) {
        throwWrongArgument(name, key);
      }
    }
    return {make<Hash>(*hash | std::views::filter([&](auto &&elt) {
              return !std::ranges::contains(values, elt.first);
            })),
            {}};
  }
  throwWrongArgument(name, values[0]);
}

InvocableResult readline(std::string_view name, ValuesSpan values,
                         const EnvPtr & /* env */) {
  static ReadLine coreReadLine{};
  checkArgsIs(name, values, 1);
  if (auto prompt = values[0]->dyncast<String>()) {
    if (auto res = coreReadLine.get(prompt->data())) {
      return {make<String>(*res), {}};
    }
    return {Constant::nilValue(), {}};
  }
  throwWrongArgument(name, values[0]);
}

InvocableResult time_ms(std::string_view name, ValuesSpan values,
                        const EnvPtr &/* env */) {
  checkArgsIs(name, values, 0);
  return {make<Integer>(std::chrono::duration_cast<std::chrono::milliseconds>(
                            std::chrono::steady_clock::now().time_since_epoch())
                            .count()),
          {}};
}

InvocableResult seq(std::string_view name, ValuesSpan values,
                    const EnvPtr &/* env */) {
  checkArgsIs(name, values, 1);
  if (values[0]->isa<Constant>()) {
    if (Constant::nilValue()->isEqualTo(values[0])->isTrue()) {
      return {Constant::nilValue(), {}};
    }
    throwWrongArgument(name, values[0]);
  }
  if (auto sequence = values[0]->dyncast<Sequence>()) {
    if (sequence->values().empty()) {
      return {Constant::nilValue(), {}};
    }
    if (auto vector = values[0]->dyncast<Vector>()) {
      return {make<List>(vector->values()), {}};
    }
    return {values[0], {}};
  }
  if (auto string = values[0]->dyncast<String>()) {
    if (string->data().empty()) {
      return {Constant::nilValue(), {}};
    }
    return {make<List>(string->data() | std::views::transform([](auto &&c) {
                         return make<String>(std::string{c});
                       })),
            {}};
  }
  throwWrongArgument(name, values[0]);
}

InvocableResult conj(std::string_view name, ValuesSpan values,
                     const EnvPtr &/* env */) {
  checkArgsAtLeast(name, values, 2);
  if (auto list = values[0]->dyncast<List>()) {
    values = values.subspan(1);
    auto res =
        values | std::views::reverse | std::ranges::to<ValuesContainer>();
    res.reserve(res.size() + list->values().size());
    std::ranges::copy(list->values(), std::back_inserter(res));
    return {make<List>(std::move(res)), {}};
  }
  if (auto vector = values[0]->dyncast<Vector>()) {
    values = values.subspan(1);
    auto res = vector->values() | std::ranges::to<ValuesContainer>();
    res.reserve(res.size() + values.size());
    std::ranges::copy(values, std::back_inserter(res));
    return {make<Vector>(std::move(res)), {}};
  }
  throwWrongArgument(name, values[0]);
}

InvocableResult meta(std::string_view name, ValuesSpan values,
                     const EnvPtr &/* env */) {
  checkArgsIs(name, values, 1);
  if (auto metaMixIn = dynamic_cast<const MetaMixIn *>(values[0].get())) {
    return {metaMixIn->meta(), {}};
  }
  throwWrongArgument(name, values[0]);
}

InvocableResult with_meta(std::string_view name, ValuesSpan values,
                          const EnvPtr &/* env */) {
  checkArgsIs(name, values, 2);
  if (auto metaMixIn = dynamic_cast<const MetaMixIn *>(values[0].get())) {
    return {metaMixIn->cloneWithMeta(values[1]), {}};
  }
  throwWrongArgument(name, values[0]);
}

InvocableResult fnQuestion(std::string_view name, ValuesSpan values, const EnvPtr & /* env */) {
  checkArgsIs(name, values, 1);
  return {values[0]->isa<Invocable>() && !values[0]->isa<Macro>()
              ? Constant::trueValue()
              : Constant::falseValue(),
          {}};
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
