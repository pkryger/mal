#include "Types.h" // IWYU pragma: associated
#include "Core.h"
#include "FunctionRef.h"
#include "InPlaceAllocator.h"
#include "Mal.h"
#if !defined(__cpp_lib_ranges_chunk)
#include "Ranges.h"
#endif // __cpp_lib_ranges_chunk

#include <algorithm>
#if defined(__aarch64__) || defined(_M_ARM64)
#include <arm_neon.h>
#include <bit>
#endif // defined(__aarch64__) || defined(_M_ARM64)
#include <cassert>
#include <format>
#include <optional>
#include <ranges>
#include <span>
#include <tuple>
#include <utility>

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
         std::views::transform([&](auto &&v) { return evalFn(v, evalEnv); });
}

class WithEvaledValues {
public:
  inline static constexpr std::size_t InPlaceLimit = 32;
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
  return !(this == Constant::nilValue().get() ||
           this == Constant::falseValue().get());
}

bool operator==(const ValuePtr &lhs, const ValuePtr &rhs) {
  return lhs->isEqualTo(rhs)->isTrue();
}

ValuePtr Integer::isEqualTo(ValuePtr rhs) const {
  if (this == rhs.get()) {
    return Constant::trueValue();
  }
  if (auto other = rhs->dyncast<Integer>();
      other && data == other->data) {
    return Constant::trueValue();
  }
  return Constant::falseValue();
}

ValuePtr StringBase::isEqualTo(ValuePtr rhs) const {
  if (this == rhs.get()) {
    return Constant::trueValue();
  }
  if (auto other = rhs->dyncast<StringBase>();
      other && other->loId_ == this->loId_ && data == other->data) {
    return Constant::trueValue();
  }
  return Constant::falseValue();
}

ValuePtr Symbol::eval(const EnvPtr &env) const {
  assert(env);
  if (auto value = env->find(asKey()))
    return value;
  throw EvalException{std::format("'{}' not found", data)};
}

ValuePtr Symbol::isEqualTo(ValuePtr rhs) const {
  if (this == rhs.get()) {
    return Constant::trueValue();
  }
  if (auto other = rhs->dyncast<Symbol>(); other && asKey() == other->asKey()) {
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

} // namespace mal

namespace {

#if defined(__aarch64__) || defined(_M_ARM64)

// This is modified from simdjson library:
// https://github.com/simdjson/simdjson/blob/45caa86/include/simdjson/generic/builder/json_string_builder-inl.h#L224-L260

std::pair<std::size_t, std::optional<char>>
find_next_character_to_unescape(std::string_view view,
                                std::size_t location) noexcept {

  const std::size_t len = view.size();
  const uint8_t *ptr =
      reinterpret_cast<const uint8_t *>(view.data()) + location;
  std::size_t remaining = len - location;
  auto unescaped_after_pos = [&](std::size_t pos) -> std::optional<char> {
    if (++pos < view.size()) {
      switch (view[pos]) {
      case '\\':
        return '\\';
      case 'n':
        return '\n';
      default:
        return view[pos];
      }
    }
    return {};
  };

  {
    // SIMD constants for characters requiring escape
    static const uint8x16_t v92 = vdupq_n_u8(92);  // '\\'

    while (remaining >= 16) {
      const uint8x16_t word = vld1q_u8(ptr);
      const uint8x16_t needs_unescape = vceqq_u8(word, v92);

      const uint8x8_t res = vshrn_n_u16(vreinterpretq_u16_u8(needs_unescape), 4);
      const std::uint64_t mask = vget_lane_u64(vreinterpret_u64_u8(res), 0);
      if (mask != 0) {
        const auto offset = static_cast<std::size_t>(
            ptr - reinterpret_cast<const std::uint8_t *>(view.data()));
        const auto trailing_zeros =
            static_cast<std::size_t>(std::countr_zero(mask));
        const auto pos = offset + (trailing_zeros >> 2);
        auto unescaped = unescaped_after_pos(pos);
        if (unescaped) {
          return {pos, unescaped};
        }
      }
      ptr += 16;
      remaining -= 16;
    }
  }
  {
    // SIMD constants for characters requiring escape
    static const uint8x8_t v92 = vdup_n_u8(92);  // '\\'

    if (remaining >= 8) {
      const uint8x8_t word = vld1_u8(ptr);
      const uint8x8_t needs_unescape = vceq_u8(word, v92);

      const std::uint64_t mask =
          vget_lane_u64(vreinterpret_u64_u8(needs_unescape), 0);
      if (mask != 0) {
        const auto offset = static_cast<std::size_t>(
            ptr - reinterpret_cast<const std::uint8_t *>(view.data()));
        const auto trailing_zeros =
            static_cast<std::size_t>(std::countr_zero(mask));
        const auto pos = offset + (trailing_zeros >> 3);
        auto unescaped = unescaped_after_pos(pos);
        if (unescaped) {
          return {pos, unescaped};
        }
      }
      ptr += 8;
      remaining -= 8;
    }
  }
  {
    // scalar constants for characters requiring escape
    static const std::uint8_t v92 = 92;  // '\\'

    while (remaining > 0) {
      const std::uint8_t word = *ptr;
      const bool needs_unescape = word == v92;

      if (needs_unescape) {
        const auto offset = static_cast<std::size_t>(
            ptr - reinterpret_cast<const std::uint8_t *>(view.data()));
        auto unescaped = unescaped_after_pos(offset);
        if (unescaped) {
          return {offset, unescaped};
        }
      }
      ++ptr;
      --remaining;
    }
  }
  return {std::size_t(view.size()), {}};
}

#endif // defined(__aarch64__) || defined(_M_ARM64)

} // namespace

namespace mal {

#if defined(__aarch64__) || defined(_M_ARM64)

std::string String::unescape(const std::string &in) {
  std::string_view view{in.begin() + 1, in.end() - 1};
  auto [location, unescaped] = find_next_character_to_unescape(view, 0);
  if (location == view.size()) [[likely]] {
    assert(!unescaped);
    return std::string{view};
  }

  std::string out;
  const auto size = view.size();
  out.reserve(size - 1);

  std::size_t pos = 0;
  while (location < size) {
    assert(unescaped);
    out.append(view.data() + pos, location - pos);
    out += *unescaped;
    pos = location + 2;
    std::tie(location, unescaped) = find_next_character_to_unescape(view, pos);
  }
  out.append(view.data() + pos, location - pos);
  out.shrink_to_fit();
  return out;
}

#else

std::string String::unescape(const std::string& in) {
  std::string out;
  out.reserve(in.size() - 2);
  for (auto i = in.begin() + 1, end = in.end() - 1; i != end; ++i) {
    char c = *i;
    if (c == '\\') {
      ++i;
      if (i != end) {
        out += [&]() noexcept {
          switch (*i) {
          case '\\':
            return '\\';
          case 'n':
            return '\n';
          default:
            return *i;
          }
        }();
      }
    } else
      out += c;
  }
  out.shrink_to_fit();
  return out;
}

#endif // defined(__aarch64__) || defined(_M_ARM64)

} // namespace mal

namespace {

void maybe_escape_and_append_character(char c, std::string &out) {
  switch (c) {
  case '\\':
    out += "\\\\";
    break;
  case '\n':
    out += "\\n";
    break;
  case '"':
    out += "\\\"";
    break;
  default:
    out += c;
  }
}

#if defined(__aarch64__) || defined(_M_ARM64)

// This is modified from simdjson library:
// https://github.com/simdjson/simdjson/blob/45caa86/include/simdjson/generic/builder/json_string_builder-inl.h#L224-L260

std::size_t find_next_character_to_escape(std::string_view view,
                                          std::size_t location) noexcept {
  const std::size_t len = view.size();
  const uint8_t *ptr =
      reinterpret_cast<const uint8_t *>(view.data()) + location;
  std::size_t remaining = len - location;
  {
    // SIMD constants for characters requiring escape
    static const uint8x16_t v10 = vdupq_n_u8(10);  // '\n'
    static const uint8x16_t v34 = vdupq_n_u8(34);  // '"'
    static const uint8x16_t v92 = vdupq_n_u8(92);  // '\\'

    while (remaining >= 16) {
      const uint8x16_t word = vld1q_u8(ptr);

      uint8x16_t needs_escape = vceqq_u8(word, v10);
      needs_escape = vorrq_u8(needs_escape, vceqq_u8(word, v34));
      needs_escape = vorrq_u8(needs_escape, vceqq_u8(word, v92));

      const uint8x8_t res = vshrn_n_u16(vreinterpretq_u16_u8(needs_escape), 4);
      const std::uint64_t mask = vget_lane_u64(vreinterpret_u64_u8(res), 0);
      if (mask != 0) {
        const auto offset = static_cast<std::size_t>(
            ptr - reinterpret_cast<const std::uint8_t *>(view.data()));
        const auto trailing_zeros =
            static_cast<std::size_t>(std::countr_zero(mask));
        return offset + (trailing_zeros >> 2);
      }
      ptr += 16;
      remaining -= 16;
    }
  }
  {
    // SIMD constants for characters requiring escape
    static const uint8x8_t v10 = vdup_n_u8(10);  // '\n'
    static const uint8x8_t v34 = vdup_n_u8(34);  // '"'
    static const uint8x8_t v92 = vdup_n_u8(92);  // '\\'

    if (remaining >= 8) {
      const uint8x8_t word = vld1_u8(ptr);

      uint8x8_t needs_escape = vceq_u8(word, v10);
      needs_escape = vorr_u8(needs_escape, vceq_u8(word, v34));
      needs_escape = vorr_u8(needs_escape, vceq_u8(word, v92));

      const std::uint64_t mask =
          vget_lane_u64(vreinterpret_u64_u8(needs_escape), 0);
      if (mask != 0) {
        const auto offset = static_cast<std::size_t>(
            ptr - reinterpret_cast<const std::uint8_t *>(view.data()));
        const auto trailing_zeros =
            static_cast<std::size_t>(std::countr_zero(mask));
        return offset + (trailing_zeros >> 3);
      }
      ptr += 8;
      remaining -= 8;
    }
  }
  {
    // scalar constants for characters requiring escape
    static const std::uint8_t v10 = 10;  // '\n'
    static const std::uint8_t v34 = 34;  // '"'
    static const std::uint8_t v92 = 92;  // '\\'

    while (remaining > 0) {
      const std::uint8_t word = *ptr;

      bool needs_escape = word == v10;
      needs_escape |= word == v34;
      needs_escape |= word == v92;

      if (needs_escape) {
        const auto offset = static_cast<std::size_t>(
            ptr - reinterpret_cast<const std::uint8_t *>(view.data()));
        return offset;
      }
      ++ptr;
      --remaining;
    }
  }

  return std::size_t(view.size());
}
#endif // defined(__aarch64__) || defined(_M_ARM64)

} // namespace

namespace mal {

#if defined(__aarch64__) || defined(_M_ARM64)

std::string String::escape(const std::string &in) {
  std::string out{'"'};
  const auto size = in.size();
  auto location = find_next_character_to_escape(in, 0);
  if (location == size) [[likely]] {
    out.reserve(size + 2);
    out.append(in.data(), size);
    out += '"';
    return out;
  }

  out.reserve(size * 2 + 2);
  std::size_t pos = 0;
  while (location < size) {
    out.append(in.data() + pos, location - pos);
    maybe_escape_and_append_character(in[location], out);
    pos = location + 1;
    location = find_next_character_to_escape(in, pos);
  }
  out.append(in.data() + pos, location - pos);

  out += '"';
  out.shrink_to_fit();
  return out;
}

#else // defined(__aarch64__) || defined(_M_ARM64)

std::string String::escape(const std::string& in) {
  std::string out{'"'};
  out.reserve(in.size() + 2);
  for (auto&& c : in) {
    maybe_escape_and_append_character(c, out);
  }
  out += '"';
  return out;
}

#endif // !(defined(__aarch64__) || defined(_M_ARM64))

std::string String::print(PrintType readably) const {
  return readably ? escape(StringBase::data) : StringBase::data;
}

ValuePtr Atom::isEqualTo(ValuePtr rhs) const {
  if (this == rhs.get()) {
    return Constant::trueValue();
  }
  if (auto o = rhs->dyncast<Atom>()) {
    return data->isEqualTo(o->data);
  }
  return Constant::falseValue();
}

ValuePtr Sequence::isEqualTo(ValuePtr rhs) const {
  if (this == rhs.get()) {
    return Constant::trueValue();
  }
  if (auto other = rhs->dyncast<Sequence>();
      other && data.size() == other->data.size()) {
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
          if (auto symbol = data[0]->dyncast<Symbol>()) {
            return symbol->fromMacro;
          }
        }
        return {};
      }()) {
    return readably ? std::format("{}{:r}", *fromMacro, data[1])
                    : std::format("{}{}", *fromMacro, data[1]);

  }
  return readably ? std::format("({:r})", data) : std::format("({})", data);
}

ValuePtr Vector::eval(const EnvPtr &env) const {
  assert(env);
  assert(!EvalFnStack::empty());
  auto &evalFn = EvalFnStack::top();
  return make<Vector>(data | std::views::transform([&](auto &&v) {
                        return evalFn(v, env);
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
    auto op = evalFn(data[0], env);
    if (auto invocable = op->dyncast<Invocable>()) {
      return invocable->apply(false, ValuesSpan{data}.subspan(1), env);
    }
    throw EvalException{std::format("invalid function '{:r}'", op)};
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
  if (auto other = rhs->dyncast<Hash>()) {
    return data == other->data ? Constant::trueValue() : Constant::falseValue();
  }
  return Constant::falseValue();
}

Hash::Hash(const Hash &other, ValuesSpan values)
    : Value{typeInfo<Hash>.lo}, data{other.data} {
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
    return handler(name, values, evalEnv);
  }
  return WithEvaledValues{values, evalEnv}([&](auto &&values) {
    return handler(name, std::forward<decltype(values)>(values), evalEnv);
  });
}

ValuePtr BuiltIn::isEqualTo(ValuePtr rhs) const {
  if (this == rhs.get()) {
    return Constant::trueValue();
  }
  if (auto other = rhs->dyncast<BuiltIn>();
      other && handler == other->handler) {
    return Constant::trueValue();
  }
  return Constant::falseValue();
}

FunctionBase::FunctionBase(std::uint32_t loId, Params params, ValuePtr body,
                           EnvPtr env)
    : Invocable{loId}, bindSize{[&]() -> std::size_t {
        if (auto i = std::ranges::find_if(
                params, [](auto &&elt) { return elt.name()[0] == '&'; });
            i != params.end()) {
          if ((params.end() - i) != 2) {
            throw EvalException{
                "there must be exactly one parameter after the &"};
          }
          return static_cast<std::size_t>(i - params.begin());
        }
        return params.size();
      }()},
      params{std::move(params)}, body{std::move(body)},
      capturedEnv{std::move(env)} {}

template <typename TYPE>
ValuePtr FunctionBase::isEqualToFunctionBase(const ValuePtr &rhs) const {
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
                       reinterpret_cast<const void *>(this), params, body);

  }
  return std::format("#<λ@{:p}>", reinterpret_cast<const void *>(this));
}

ValuePtr Lambda::isEqualTo(ValuePtr rhs) const {
  return FunctionBase::template isEqualToFunctionBase<Lambda>(rhs);
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
                       reinterpret_cast<const void *>(this), params, body);
  }
  return std::format("#<macro@{:p}>", reinterpret_cast<const void *>(this));
}

ValuePtr Macro::isEqualTo(ValuePtr rhs) const {
  return FunctionBase::template isEqualToFunctionBase<Macro>(rhs);
}

InvocableResult Macro::apply(bool /* evaled */, ValuesSpan values,
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
