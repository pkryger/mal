#include "Types.h" // IWYU pragma: associated
#include "Core.h"
#include "Env.h"
#include "FunctionRef.h"
#include "Hierarchy.h"
#include "InPlaceAllocator.h"
#include "Mal.h"
#include "StringEscaping.h"

// NOLINTNEXTLINE(readability-use-concise-preprocessor-directives) - consistent checks
#if !defined(__cpp_lib_ranges_chunk)
#include "Ranges.h"
#endif // __cpp_lib_ranges_chunk

#include <algorithm>
#include <cassert>
#include <cstddef>
#include <cstdint>
#include <format>
#include <optional>
#include <ranges>
#include <span>
#include <string>
#include <string_view>
#include <tuple>
#include <utility>
#include <vector>

// NOLINTBEGIN(misc-include-cleaner) - looks like namespace std freaks clang-tidy out
std::size_t
std::hash<mal::ValuePtr>::operator()(const mal::ValuePtr &value) const {
  assert(value->isa<mal::StringBase>());
  if (const auto *stringBase = value->dyncast<mal::StringBase>()) {
    return std::hash<std::string>{}(stringBase->data_);
  }
  throw mal::EvalException{std::format("invalid key '{:r}'", value)};
}
// NOLINTEND(misc-include-cleaner)

namespace {

using mal::EnvPtr;
using mal::EvalFnStack;
using mal::InPlaceAllocator;
using mal::ValuePtr;
using mal::ValuesContainer;
using mal::ValuesSpan;

template <typename VALUES>
auto evalValues(VALUES &&values, const EnvPtr &evalEnv)  {
  assert(!EvalFnStack::empty());
  auto &evalFn = EvalFnStack::top();
  return std::forward<VALUES>(values) |
         std::views::transform(
             [&](auto &&value) { return evalFn(value, evalEnv); });
}

class WithEvaledValues {
public:
  static constexpr std::size_t InPlaceLimit = 32;
  using Allocator = InPlaceAllocator<ValuePtr, InPlaceLimit>;

  explicit WithEvaledValues(ValuesSpan values, EnvPtr evalEnv)
      : values_{values}, evalEnv_{std::move(evalEnv)} {}

  template <typename FUNC> auto operator()(FUNC &&func) {
    auto view = evalValues(values_, evalEnv_);
    if (InPlaceLimit < values_.size()) [[unlikely]] {
      return std::forward<FUNC>(func)(view |
                                      std::ranges::to<ValuesContainer>());
    }

    return std::forward<FUNC>(func)(std::vector<ValuePtr, Allocator>{
        std::from_range, view, Allocator{storage_}});
  }

private:
  ValuesSpan values_;
  EnvPtr evalEnv_;
  Allocator::Storage storage_;
};

} // namespace

namespace mal {

bool Value::isTrue() const noexcept {
  return this != Constant::nilValue().get() &&
         this != Constant::falseValue().get();
}

bool operator==(const ValuePtr &lhs, const ValuePtr &rhs) {
  return lhs->isEqualTo(rhs)->isTrue();
}

ValuePtr Integer::isEqualTo(ValuePtr rhs) const {
  if (this == rhs.get()) {
    return Constant::trueValue();
  }
  if (const auto *other = rhs->dyncast<Integer>();
      (other != nullptr) && data == other->data) {
    return Constant::trueValue();
  }
  return Constant::falseValue();
}

ValuePtr StringBase::isEqualTo(ValuePtr rhs) const {
  if (this == rhs.get()) {
    return Constant::trueValue();
  }
  if (const auto *other = rhs->dyncast<StringBase>();
      (other != nullptr) && other->lowId_ == this->lowId_ &&
      data_ == other->data_) {
    return Constant::trueValue();
  }
  return Constant::falseValue();
}

ValuePtr Symbol::eval(const EnvPtr &env) const {
  assert(env);
  if (auto value = env->find(asKey())) {
    return value;
  }
  throw EvalException{std::format("'{}' not found", data_)};
}

ValuePtr Symbol::isEqualTo(ValuePtr rhs) const {
  if (this == rhs.get()) {
    return Constant::trueValue();
  }
  if (const auto *other = rhs->dyncast<Symbol>();
      (other != nullptr) && asKey() == other->asKey()) {
    return Constant::trueValue();
  }
  return Constant::falseValue();
}

ValuePtr Constant::isEqualTo(ValuePtr rhs) const {
  if (this == rhs.get()) {
    return Constant::trueValue();
  }
  return Constant::falseValue();
}

std::string String::print(PrintType readably) const {
  return readably ? escapeString(StringBase::data_) : StringBase::data_;
}

ValuePtr Atom::isEqualTo(ValuePtr rhs) const {
  if (this == rhs.get()) {
    return Constant::trueValue();
  }
  if (const auto *atom = rhs->dyncast<Atom>()) {
    return data->isEqualTo(atom->data);
  }
  return Constant::falseValue();
}

ValuePtr Sequence::isEqualTo(ValuePtr rhs) const {
  if (this == rhs.get()) {
    return Constant::trueValue();
  }
  if (const auto *other = rhs->dyncast<Sequence>();
      (other != nullptr) && data.size() == other->data.size()) {
    auto res =
        std::ranges::mismatch(data, other->data, [](auto &&lhs, auto &&rhs) {
          return lhs->isEqualTo(rhs)->isTrue();
        });
    return res.in1 == data.end() && res.in2 == other->data.end()
               ? Constant::trueValue()
               : Constant::falseValue();
  }
  return Constant::falseValue();
}

std::string List::print(PrintType readably) const {
  if (readably == MalReadably) {
    return std::format("({:l})", data);
  }
  if (auto fromMacro = [&]() -> std::optional<std::string> {
        if (data.size() == 2) {
          if (const auto *symbol = data.at(0)->dyncast<Symbol>()) {
            return symbol->fromMacro_;
          }
        }
        return {};
      }()) {
    return readably ? std::format("{}{:r}", *fromMacro, data.at(1))
                    : std::format("{}{}", *fromMacro, data.at(1));

  }
  return readably ? std::format("({:r})", data) : std::format("({})", data);
}

ValuePtr Vector::eval(const EnvPtr &env) const {
  assert(env);
  assert(!EvalFnStack::empty());
  auto &evalFn = EvalFnStack::top();
  return make<Vector>(data | std::views::transform([&](auto &&value) {
                        return evalFn(value, env);
                      }));
}

ValuePtr List::eval(const EnvPtr &env) const {
  assert(env);
  assert(!EvalFnStack::empty());
  if (data.empty()) {
    return shared_from_this();
  }
  auto &evalFn = EvalFnStack::top();
  auto [ast, evalEnv] = [&]() {
    auto value = evalFn(data[0], env);
    if (const auto *invocable = value->dyncast<Invocable>()) {
      return invocable->apply(false, ValuesSpan{data}.subspan(1), env);
    }
    throw EvalException{std::format("invalid function '{:r}'", value)};
  }();

  return evalEnv ? EvalFnStack::top()(ast, *evalEnv) : ast;
}

std::string Hash::print(PrintType readably) const {
  return readably ? std::format("{{{:r}}}", data) : std::format("{{{}}}", data);
}

ValuePtr Hash::eval(const EnvPtr &env) const {
  assert(env);
  assert(!EvalFnStack::empty());
  auto &evalFn = EvalFnStack::top();
  return make<Hash>(data | std::views::transform([&](auto &&elt) {
                      return std::pair{elt.first, evalFn(elt.second, env)};
                    }));
}

ValuePtr Hash::isEqualTo(ValuePtr rhs) const {
  if (this == rhs.get()) {
    return Constant::trueValue();
  }
  if (const auto *other = rhs->dyncast<Hash>()) {
    return data == other->data ? Constant::trueValue() : Constant::falseValue();
  }
  return Constant::falseValue();
}

Hash::Hash(const Hash &other, ValuesSpan values)
    : Value{typeInfo<Hash>.low_}, data{other.data} {
  for (auto [key, value] : values |
#ifdef __cpp_lib_ranges_chunk
                               std::views::chunk(2)
#else
                               mal::views::Chunk(2)
#endif // __cpp_lib_ranges_chunk
                               | std::views::transform([](auto &&chunk) {
                                   assert(chunk[0]->template isa<StringBase>());
                                   return std::tie(chunk[0], chunk[1]);
                                 })) {
    data.insert_or_assign(key, value);
  }
}

ValuePtr Hash::find(const ValuePtr &key) const {
  if (auto res = data.find(key); res != data.end()) {
    return res->second;
  }
  return nullptr;
}

InvocableResult BuiltIn::apply(bool evaled, ValuesSpan values,
                               const EnvPtr &evalEnv) const {
  if (evaled) {
    return handler_(name_, values, evalEnv);
  }
  return WithEvaledValues{values, evalEnv}([&](auto &&values) {
    return handler_(name_, std::forward<decltype(values)>(values), evalEnv);
  });
}

ValuePtr BuiltIn::isEqualTo(ValuePtr rhs) const {
  if (this == rhs.get()) {
    return Constant::trueValue();
  }
  if (const auto *other = rhs->dyncast<BuiltIn>();
      (other != nullptr) && name_ == other->name_) {
    return Constant::trueValue();
  }
  return Constant::falseValue();
}

FunctionBase::FunctionBase(std::uint32_t lowId, Params params, ValuePtr body,
                           EnvPtr env)
    : Invocable{lowId}, bindSize{[&]() -> std::size_t {
        if (auto iter = std::ranges::find_if(
                params, [](auto &&elt) { return elt.name().at(0) == '&'; });
            iter != params.end()) {
          if ((params.end() - iter) != 2) {
            throw EvalException{
                "there must be exactly one parameter after the &"};
          }
          return static_cast<std::size_t>(iter - params.begin());
        }
        return params.size();
      }()},
      params{std::move(params)}, body{std::move(body)},
      capturedEnv{std::move(env)} {}

template <typename TYPE>
ValuePtr FunctionBase::isEqualImpl(const ValuePtr &rhs) const {
  if (this == rhs.get()) {
    return Constant::trueValue();
  }
  if (auto other = rhs->dyncast<TYPE>();
      other && bindSize == other->bindSize &&
      params.size() == other->params.size()) {
    auto res = std::ranges::mismatch(params, other->params,
                                     [](auto &&lhs, auto &&rhs) noexcept {
                                       return lhs == rhs;
                                     });
    if (res.in1 != params.end() || res.in2 != other->params.end()) {
      return Constant::falseValue();
    }
    return body->isEqualTo(other->body);
  }
  return Constant::falseValue();
}

template <typename VALUES>
EnvPtr FunctionBase::makeApplyEnv(VALUES &&values, const EnvPtr &evalEnv) const {
  auto applyEnv = make<ApplyEnv>(
      evalEnv, capturedEnv,
      std::views::zip(
          params | std::views::take(bindSize) |
              std::views::transform([](auto &&param) { return param.asKey(); }),
          std::forward<VALUES>(values)));
  if (bindSize == params.size()) {
    checkArgsIs(print(Simply), values.size(), bindSize);
  } else {
    checkArgsAtLeast(print(Simply), values.size(), bindSize);
    applyEnv->insert_or_assign(
        params.back().asKey(),
        make<List>(std::forward<VALUES>(values) | std::views::drop(bindSize)));
  }
  return applyEnv;
}

std::string Lambda::print(PrintType readable) const {
  if (readable) {
    return std::format("#<λ@{:p} {} {:r}>",
                       static_cast<const void *>(this), params, body);

  }
  return std::format("#<λ@{:p}>", static_cast<const void *>(this));
}

ValuePtr Lambda::isEqualTo(ValuePtr rhs) const {
  return FunctionBase::template isEqualImpl<Lambda>(rhs);
}

InvocableResult Lambda::apply(bool evaled, ValuesSpan values,
                              const EnvPtr &evalEnv) const {
  if (evaled) {
    return {body, makeApplyEnv(values, evalEnv)};
  }
  return {body, makeApplyEnv(evalValues(values, evalEnv), evalEnv)};
}

std::string Macro::print(PrintType readable) const {
  if (readable) {
    return std::format("#<macro@{:p} {} {:r}>",
                       static_cast<const void *>(this), params, body);
  }
  return std::format("#<macro@{:p}>", static_cast<const void *>(this));
}

ValuePtr Macro::isEqualTo(ValuePtr rhs) const {
  return FunctionBase::template isEqualImpl<Macro>(rhs);
}

InvocableResult Macro::apply(bool /*evaled*/, ValuesSpan values,
                             const EnvPtr &evalEnv) const {
  assert(!EvalFnStack::empty());
  return {EvalFnStack::top()(body, FunctionBase::makeApplyEnv(values, evalEnv)),
          evalEnv};
}

ValuePtr Eval::isEqualTo(ValuePtr rhs) const {
  if (this == rhs.get()) {
    return Constant::trueValue();
  }
  return Constant::falseValue();
}

InvocableResult Eval::apply(bool evaled, ValuesSpan values,
                            const EnvPtr &evalEnv) const {
  assert(!EvalFnStack::empty());
  checkArgsIs("eval", values, 1);
  return {evaled ? values[0] : EvalFnStack::top()(values[0], evalEnv), env};
}

} // namespace mal
