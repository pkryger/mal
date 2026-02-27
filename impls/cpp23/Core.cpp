#include "Core.h" // IWYU pragma: associated
#include "Reader.h"
#include "Ranges.h"
#include "Types.h"

#include <algorithm>
#include <array>
#include <cassert>
#include <cstddef>
#include <cstdint>
#include <filesystem>
#include <format>
#include <fstream>
#include <functional>
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


namespace mal {
static std::optional<std::reference_wrapper<EvalFn>> repEvalFn;

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
using mal::EnvPtr;
using mal::Hash;
using mal::Integer;
using mal::Invocable;
using mal::Keyword;
using mal::List;
using mal::make;
using mal::MalException;
using mal::readStr;
using mal::repEvalFn;
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
ValuePtr typeQuestion(std::string_view name, ValuesSpan values,
                      EnvPtr /* env */) {
  checkArgsIs(name, values, 1);
  return to<TYPE>(values[0]) ? Constant::trueValue() : Constant::falseValue();
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

ValuePtr addition(std::string_view name, ValuesSpan values, EnvPtr /* env */) {
  return detail::accumulateIntegers(
      name, values,
      [](auto &&acc, auto &&v) noexcept { return acc + v; },
      [](auto &&v) noexcept { return v; });
}

ValuePtr subtraction(std::string_view name, ValuesSpan values, EnvPtr /* env */) {
  return detail::accumulateIntegers(
      name, values,
      [](auto &&acc, auto &&v) { return acc - v; },
      [](auto &&v) noexcept { return -v; });
}

ValuePtr multiplication(std::string_view name, ValuesSpan values, EnvPtr /* env */) {
  return detail::accumulateIntegers(
      name, values,
      [](auto &&acc, auto &&v) { return acc * v; },
      [](auto &&v) noexcept { return v; });
}

ValuePtr division(std::string_view name, ValuesSpan values, EnvPtr /* env */) {
  return detail::accumulateIntegers(
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

ValuePtr list(std::string_view /* name */, ValuesSpan values, EnvPtr /* env */) {
  return make<List>(values);
}

ValuePtr emptyQuestion(std::string_view name, ValuesSpan values, EnvPtr /* env */) {
  checkArgsIs(name, values, 1);
  if (auto sequence = to<Sequence>(values.front())) {
    return sequence->values().empty() ? Constant::trueValue()
                                      : Constant::falseValue();
  }
  throwWrongArgument(name, values.front());

}

ValuePtr count(std::string_view name, ValuesSpan values, EnvPtr /* env */) {
  checkArgsAtLeast(name, values, 1);
  if (values.front().get() == Constant::nilValue().get()) {
    return make<Integer>(0);
  }
  if (auto sequence = to<Sequence>(values.front())) {
    return make<Integer>(sequence->values().size());
  }
  throwWrongArgument(name, values.front());
}

template <typename BINARY_OP>
ValuePtr compareIntegers(std::string_view name, ValuesSpan values,
                            BINARY_OP &&binary_op) {
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
  return notMatching == values.end() ? Constant::trueValue()
                                     : Constant::falseValue();

}

ValuePtr lt(std::string_view name, ValuesSpan values, EnvPtr /* env */) {
  return compareIntegers(
      name, values,
      [](auto &&lhs, auto &&rhs) noexcept { return lhs < rhs; });
}

ValuePtr lte(std::string_view name, ValuesSpan values, EnvPtr /* env */) {
  return compareIntegers(
            name, values,
      [](auto &&lhs, auto &&rhs) noexcept { return lhs <= rhs; });
}

ValuePtr gt(std::string_view name, ValuesSpan values, EnvPtr /* env */) {
  return compareIntegers(
            name, values,
      [](auto &&lhs, auto &&rhs) noexcept { return lhs > rhs; });
}

ValuePtr gte(std::string_view name, ValuesSpan values, EnvPtr /* env */) {
  return compareIntegers(
            name, values,
      [](auto &&lhs, auto &&rhs) noexcept { return lhs >= rhs; });
}

ValuePtr equal(std::string_view name, ValuesSpan values, EnvPtr /* env */) {
  checkArgsAtLeast(name, values, 2);
  auto notEqual =
      std::ranges::adjacent_find(values, [](auto &&lhs, auto &&rhs) {
        return !lhs->isEqualTo(rhs)->isTrue();
      });
  return notEqual == values.end() ? Constant::trueValue()
                                  : Constant::falseValue();
}

ValuePtr not_(std::string_view name, ValuesSpan values, EnvPtr /* env */) {
  checkArgsIs(name, values, 1);
  return values.front()->isTrue() ? Constant::falseValue()
                                  : Constant::trueValue();
}

ValuePtr prn(std::string_view name, ValuesSpan values, EnvPtr /* env */) {
  std::print("{:r}\n", values);
  return Constant::nilValue();
}

ValuePtr println(std::string_view name, ValuesSpan values, EnvPtr /* env */) {
  std::print("{}\n", values);
  return Constant::nilValue();
}

ValuePtr pr_str(std::string_view name, ValuesSpan values, EnvPtr /* env */) {
  return make<String>(
      std::format("{:r}", values));
}

ValuePtr str(std::string_view name, ValuesSpan values, EnvPtr /* env */) {
  return make<String>(
      values |
      std::views::transform([](auto &&elt) { return std::format("{}", elt); }) |
      std::views::join | std::ranges::to<std::string>());
}

ValuePtr slurp(std::string_view name, ValuesSpan values, EnvPtr /* env */) {
  checkArgsIs(name, values, 1);
  if (auto string = to<String>(values[0])) {
    auto path = std::format("{}", values[0]);
    auto file_size = std::filesystem::file_size(path);
    std::ifstream file{path, std::ios::in | std::ios::binary};
    if (!file) {
      throw CoreException(std::format("{}: file {} open error",
                                      name, std::move(path)));
    }
    std::string content(file_size, '\0');
    file.read(content.data(), file_size);
    return make<String>(std::move(content));
  }
   throwWrongArgument(name, values[0]);
}

ValuePtr read_string(std::string_view name, ValuesSpan values, EnvPtr /* env */) {
  checkArgsIs(name, values, 1);
  if (to<String>(values[0])) {
    return readStr(std::format("{}", values[0]));
  }
  throwWrongArgument(name, values[0]);
}

ValuePtr atom(std::string_view name, ValuesSpan values, EnvPtr /* env */) {
  checkArgsIs(name, values, 1);
  return make<Atom>(values[0]);
}

ValuePtr deref(std::string_view name, ValuesSpan values, EnvPtr /* env */) {
  checkArgsIs(name, values, 1);
  if (auto atom = to<Atom>(values[0])) {
    return atom->value();
  }
  throwWrongArgument(name, values[0]);
}

ValuePtr resetBang(std::string_view name, ValuesSpan values, EnvPtr /* env */) {
  checkArgsIs(name, values, 2);
  if (auto atom = to<Atom>(values[0])) {
    return atom->reset(values[1]);
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

ValuePtr swapBang(std::string_view name, ValuesSpan values, EnvPtr env) {
  assert(repEvalFn);
  checkArgsAtLeast(name, values, 2);
  if (auto atom = to<Atom>(values[0])) {
    if (auto fn = to<Invocable>(values[1])) {
      auto args = detail::cons(atom->value(), values.subspan(2));
      auto [ast, evalEnv, needsEval] = fn->apply(ValuesSpan{args}, env);
      if (needsEval) {
        ast = repEvalFn.value()(ast, evalEnv);
      }
      return atom->reset(ast);
    }
    throwWrongArgument(name, values[1]);
  }
  throwWrongArgument(name, values[0]);
}

ValuePtr cons(std::string_view name, ValuesSpan values, EnvPtr /* env */) {
  checkArgsIs(name, values, 2);
  if (auto sequence = to<Sequence>(values[1])) {
    return make<List>(detail::cons(values[0], sequence->values()));
  }
  return make<List>(values);
}

ValuePtr concat(std::string_view name, ValuesSpan values, EnvPtr /* env */) {
  return make<List>(values | std::views::transform([&](auto elt) {
                      if (auto sequence = to<Sequence>(elt)) {
                        return sequence->values();
                      }
                      throwWrongArgument(name, elt);
                    }) |
                    std::views::join);
}

ValuePtr vec(std::string_view name, ValuesSpan values, EnvPtr /* env */) {
  checkArgsIs(name, values, 1);
  if (to<Vector>(values[0])) {
    return values[0];
  }
  if (auto list = to<List>(values[0])) {
    return make<Vector>(list->values());
  }
  throwWrongArgument(name, values[0]);
}

ValuePtr nth(std::string_view name, ValuesSpan values, EnvPtr /* env */) {
  checkArgsIs(name, values, 2);
  if (auto integer = to<Integer>(values[1])) {
    auto index = integer->value();
    if (auto sequence = to<Sequence>(values[0])) {
      auto values = sequence->values();
      if (0 <= index && index < values.size()) {
        return values[index];
      }
      throw CoreException{std::format("index out of bounds {} for '{}'", index,
                                      name)};
    }
    if (auto constant = to<Constant>(values[0]);
        constant && index == 0 && Constant::nilValue()->isEqualTo(values[0])) {
      return Constant::nilValue();
    }
    throwWrongArgument(name, values[0]);
  }
  throwWrongArgument(name, values[1]);
}

ValuePtr first(std::string_view name, ValuesSpan values, EnvPtr /* env */) {
  checkArgsIs(name, values, 1);
  if (auto sequence = to<Sequence>(values[0])) {
    auto values = sequence->values();
    if (!values.empty()) {
      return values[0];
    }
    return Constant::nilValue();
  }
  if (auto constant = to<Constant>(values[0]);
      constant && Constant::nilValue()->isEqualTo(values[0])) {
    return Constant::nilValue();
  }
  throwWrongArgument(name, values[0]);
}

ValuePtr rest(std::string_view name, ValuesSpan values, EnvPtr /* env */) {
  checkArgsIs(name, values, 1);
  if (auto sequence = to<Sequence>(values[0])) {
    auto values = sequence->values();
    if (!values.empty()) {
      return make<List>(values.subspan(1));
    }
    return make<List>();
  }
  if (auto constant = to<Constant>(values[0]);
      constant && Constant::nilValue()->isEqualTo(values[0])) {
    return make<List>();
  }
  throwWrongArgument(name, values[0]);
}

[[noreturn]]
ValuePtr throw_(std::string_view name, ValuesSpan values, EnvPtr /* env */) {
  checkArgsIs(name, values, 1);
  throw MalException{std::format("Exception: {:r}", values[0]), values[0]};
}

ValuePtr map(std::string_view name, ValuesSpan values, EnvPtr env) {
  checkArgsIs(name, values, 2);
  if (auto invocable = to<Invocable>(values[0])) {
    if (auto sequence = to<Sequence>(values[1])) {
      return make<List>(
          sequence->values() | std::views::transform([&](auto &&value) {
            auto [ast, evalEnv, needsEval] =
                invocable->apply(ValuesSpan{std::addressof(value), 1}, env);
            if (needsEval) {
              return repEvalFn.value()(std::move(ast), std::move(evalEnv));
            }
            return ast;
      }));
    }
    throwWrongArgument(name, values[1]);
  }
  throwWrongArgument(name, values[0]);
}

ValuePtr apply(std::string_view name, ValuesSpan values, EnvPtr env) {
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
    auto [ast, evalEnv, needsEval] = invocable->apply(args, env);
    if (needsEval) {
      return repEvalFn.value()(std::move(ast), std::move(evalEnv));
    }
    return ast;
  }
  throwWrongArgument(name, values[0]);
}

namespace detail {

ValuePtr constantQuestion(std::string_view name, ValuesSpan values,
                          ValuePtr constant) {
  checkArgsIs(name, values, 1);
  return constant->isEqualTo(values[0]);
}

} // namespace detail

ValuePtr nilQuestion(std::string_view name, ValuesSpan values,
                     EnvPtr /* env */) {
  return detail::constantQuestion(name, values, Constant::nilValue());
}

ValuePtr trueQuestion(std::string_view name, ValuesSpan values,
                     EnvPtr /* env */) {
  return detail::constantQuestion(name, values, Constant::trueValue());
}

ValuePtr falseQuestion(std::string_view name, ValuesSpan values,
                     EnvPtr /* env */) {
  return detail::constantQuestion(name, values, Constant::falseValue());
}

ValuePtr symbol(std::string_view name, ValuesSpan values, EnvPtr /* env */) {
  checkArgsIs(name, values, 1);
  if (auto string = to<String>(values[0])) {
    return make<Symbol>(string->data());
  }
  throwWrongArgument(name, values[0]);
}

ValuePtr keyword(std::string_view name, ValuesSpan values, EnvPtr /* env */) {
  checkArgsIs(name, values, 1);
  if (to<Keyword>(values[0])) {
    return values[0];
  }
  if (auto string = to<String>(values[0])) {
    return make<Keyword>(std::format(":{}", string->data()));
  }
  throwWrongArgument(name, values[0]);
}

ValuePtr vector(std::string_view /*name*/, ValuesSpan values,
                EnvPtr /* env */) {

  return make<Vector>(values);
}

ValuePtr hash_map(std::string_view name, ValuesSpan values,
                  EnvPtr /* env */) {
  if (values.size() % 2 == 0) {
    for (auto key : values | std::views::stride(2)) {
      if (!to<StringBase>(key)) {
        mal::throwWrongArgument(name, key);
      }
    }
    return make<Hash>(values);
  }
  throw CoreException(
      std::format("odd number of arguments for '{}', {}", name, values.size()));
}

ValuePtr assoc(std::string_view name, ValuesSpan values, EnvPtr /* env */) {
  checkArgsAtLeast(name, values, 1);
  if (values.size() % 2 == 1) {
    if (auto hash = to<Hash>(values[0])) {
      for (auto key : values.subspan(1) | std::views::stride(2)) {
        if (!to<StringBase>(key)) {
          mal::throwWrongArgument(name, key);
        }
      }
      return make<Hash>(*hash, values.subspan(1));
    }
    throwWrongArgument(name, values[0]);
  }
  throw CoreException(
      std::format("odd number of arguments for '{}', {}", name, values.size() - 1));
}

ValuePtr get(std::string_view name, ValuesSpan values, EnvPtr /* env */) {
  checkArgsIs(name, values, 2);
  if (Constant::nilValue()->isEqualTo(values[0])->isTrue()) {
    return Constant::nilValue();
  }
  if (auto hash = to<Hash>(values[0])) {
    if (to<StringBase>(values[1])) {
      if (auto res = hash->find(values[1]))
        return res;
      else {
        return Constant::nilValue();
      }
    }
    throwWrongArgument(name, values[1]);
  }
  throwWrongArgument(name, values[0]);
}

ValuePtr containsQuestion(std::string_view name, ValuesSpan values, EnvPtr /* env */) {
  checkArgsIs(name, values, 2);
  if (Constant::nilValue()->isEqualTo(values[0])->isTrue()) {
    return Constant::nilValue();
  }
  if (auto hash = to<Hash>(values[0])) {
    if (to<StringBase>(values[1])) {
      if (hash->find(values[1]))
        return Constant::trueValue();
      else {
        return Constant::falseValue();
      }
    }
    throwWrongArgument(name, values[1]);
  }
  throwWrongArgument(name, values[0]);
}

ValuePtr keys(std::string_view name, ValuesSpan values, EnvPtr /* env */) {
  checkArgsIs(name, values, 1);
  if (Constant::nilValue()->isEqualTo(values[0])->isTrue()) {
    return Constant::nilValue();
  }
  if (auto hash = to<Hash>(values[0])) {
    return make<List>(
        *hash | std::views::transform([](auto &&elt) { return elt.first; }));
  }
  throwWrongArgument(name, values[0]);
}

ValuePtr vals(std::string_view name, ValuesSpan values, EnvPtr /* env */) {
  checkArgsIs(name, values, 1);
  if (Constant::nilValue()->isEqualTo(values[0])->isTrue()) {
    return Constant::nilValue();
  }
  if (auto hash = to<Hash>(values[0])) {
    return make<List>(
        *hash | std::views::transform([](auto &&elt) { return elt.second; }));
  }
  throwWrongArgument(name, values[0]);
}

ValuePtr dissoc(std::string_view name, ValuesSpan values, EnvPtr /* env */) {
  checkArgsAtLeast(name, values, 1);
  if (auto hash = to<Hash>(values[0])) {
    values = values.subspan(1);
    for (auto key : values) {
      if (!to<StringBase>(key)) {
        throwWrongArgument(name, key);
      }
    }
    return make<Hash>(*hash | std::views::filter([&](auto &&elt) {
      return !std::ranges::contains(values, elt.first);
    }) | std::views::transform([](auto &&elt) {
      return std::array{elt.first, elt.second};
    }) | std::views::join |
                      std::ranges::to<ValuesContainer>());
  }
  throwWrongArgument(name, values[0]);
}

} // namespace

namespace mal {
void prepareEnv(EvalFn &evalFn, Env &env) {
  repEvalFn = evalFn;
  static std::array builtIns{
      make<BuiltIn>("atom?", typeQuestion<Atom>),
      make<BuiltIn>("keyword?", typeQuestion<Keyword>),
      make<BuiltIn>("list?", typeQuestion<List>),
      make<BuiltIn>("macro?", typeQuestion<Macro>),
      make<BuiltIn>("map?", typeQuestion<Hash>),
      make<BuiltIn>("sequential?", typeQuestion<Sequence>),
      make<BuiltIn>("symbol?", typeQuestion<Symbol>),
      make<BuiltIn>("vector?", typeQuestion<Vector>),
      make<BuiltIn>("+", addition),
      make<BuiltIn>("-", subtraction),
      make<BuiltIn>("*", multiplication),
      make<BuiltIn>("/", division),
      make<BuiltIn>("list", list),
      make<BuiltIn>("empty?", emptyQuestion),
      make<BuiltIn>("count", count),
      make<BuiltIn>("<", lt),
      make<BuiltIn>("<=", lte),
      make<BuiltIn>(">", gt),
      make<BuiltIn>(">=", gte),
      make<BuiltIn>("=", equal),
      make<BuiltIn>("not", not_),
      make<BuiltIn>("prn", prn),
      make<BuiltIn>("println", println),
      make<BuiltIn>("pr-str", pr_str),
      make<BuiltIn>("str", str),
      make<BuiltIn>("slurp", slurp),
      make<BuiltIn>("read-string", read_string),
      make<BuiltIn>("atom", atom),
      make<BuiltIn>("deref", deref),
      make<BuiltIn>("reset!", resetBang),
      make<BuiltIn>("swap!", swapBang),
      make<BuiltIn>("cons", cons),
      make<BuiltIn>("concat", concat),
      make<BuiltIn>("vec", vec),
      make<BuiltIn>("nth", nth),
      make<BuiltIn>("first", first),
      make<BuiltIn>("rest", rest),
      make<BuiltIn>("throw", throw_),
      make<BuiltIn>("map", map),
      make<BuiltIn>("nil?", nilQuestion),
      make<BuiltIn>("true?", trueQuestion),
      make<BuiltIn>("false?", falseQuestion),
      make<BuiltIn>("apply", apply),
      make<BuiltIn>("symbol", symbol),
      make<BuiltIn>("keyword", keyword),
      make<BuiltIn>("vector", vector),
      make<BuiltIn>("hash-map", hash_map),
      make<BuiltIn>("assoc", assoc),
      make<BuiltIn>("get", get),
      make<BuiltIn>("contains?", containsQuestion),
      make<BuiltIn>("keys", keys),
      make<BuiltIn>("vals", vals),
      make<BuiltIn>("dissoc", dissoc),
  };

  for (auto &builtIn : builtIns) {
    env.insert_or_assign(to<BuiltIn>(builtIn)->asKey(),
                         builtIn);
  }
}

} // namespace mal
