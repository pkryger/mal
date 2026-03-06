#ifndef INCLUDE_TYPES_H
#define INCLUDE_TYPES_H

#include "Ranges.h"
#include "Env.h"
#include "Mal.h"

#include <cassert>
#include <concepts>
#include <cstddef>
#include <cstdint>
#include <format>
#include <functional> // IWYU pragma: keep
#include <memory>
#include <optional>
#include <ranges>
#include <stdexcept>
#include <string>
#include <string_view>
#include <tuple>
#include <unordered_map>
#include <utility>
#include <vector> // IWYU pragma: keep
// IWYU pragma: no_include <__vector/vector.h>

namespace mal {

using ValuesContainer = std::vector<ValuePtr>;
using ValuesMap = std::unordered_map<ValuePtr, ValuePtr>;

class Value : public GarbageCollectible, public std::enable_shared_from_this<Value> {
public:
  virtual std::string print(bool readably) const = 0;
  virtual  ValuePtr eval(EnvPtr) const { return shared_from_this(); }

  bool isTrue() const noexcept;

  virtual ValuePtr isEqualTo(ValuePtr rhs) const = 0;

  virtual ~Value() = default;

  Value(const Value &) = delete;
  Value(Value &&) = delete;
  Value &operator=(const Value &) = delete;
  Value &operator=(Value &&) = delete;

protected:
  Value() = default;
};


class Integer : public Value {
public:
  explicit Integer(std::int64_t value) noexcept : data{value} {}

  std::string print(bool /* readably */) const override {
    return std::to_string(data);
  }

  ValuePtr isEqualTo(ValuePtr rhs) const override;

  std::int64_t value() const noexcept { return data; }
private:
  std::int64_t data;
};

namespace detail {

template <typename...> class Intern;

} // namespace detail

class Symbol;

class StringBase : public Value {
public:
  std::string print(bool /* readably */) const override { return data; }
  ValuePtr isEqualTo(ValuePtr rhs) const override;

  friend bool operator==(const StringBase &lhs,
                         const StringBase &rhs) noexcept {
    return lhs.data == rhs.data;
  }

  friend std::hash<ValuePtr>;
  friend detail::Intern<std::string, Symbol>;

protected:
  explicit StringBase(std::string v) noexcept : data{std::move(v)} {}
  std::string data;
};

} // namespace mal

// This block need to appear when the StringBase is already defined, but
// before the first instantiation of ValuesMap, such that hash<ValuePtr>
// can be defined
namespace std {

template <> struct hash<mal::ValuePtr> {
  std::size_t operator()(const mal::ValuePtr &o) const noexcept {
    assert(mal::to<mal::StringBase>(o));
    return std::hash<std::string>{}(
        static_cast<const mal::StringBase &>(*o).data);
  }
};

} // namespace std

namespace mal {

bool operator==(const ValuePtr &lhs, const ValuePtr &rhs);

struct ParseValueMixin {
  constexpr auto parse(std::format_parse_context &ctx) {
    auto it = ctx.begin();
    if (it != ctx.end() && *it == 'r') {
      readably = true;
      ++it;
    }
    return it;
  }
  bool readably{false};
};

template <typename RANGE_FORMATTER>
constexpr auto RangeFormatterParse(RANGE_FORMATTER &rf,
                                   std::format_parse_context &ctx) {

  rf.set_brackets("", "");
  rf.set_separator(" ");
  auto it = ctx.begin();
  if (it != ctx.end() && *it == 'r') {
    rf.underlying().readably = true;
    ++it;
  }
  return it;
}

} // namespace mal

namespace std {

template <> struct formatter<mal::ValuePtr> : mal::ParseValueMixin {
  template<typename FORMAT_CONTEXT>
  auto format(const mal::ValuePtr &val, FORMAT_CONTEXT &ctx) const {
    return format_to(ctx.out(), "{}", val->print(readably));
  }
};

template <>
struct formatter<mal::ValuesMap::value_type> : mal::ParseValueMixin {
  template <typename FORMAT_CONTEXT>
  auto format(const mal::ValuesMap::value_type &val,
              FORMAT_CONTEXT &ctx) const {
    if (readably) {
      return format_to(ctx.out(), "{:r} {:r}", val.first, val.second);
    }
    return format_to(ctx.out(), "{} {}", val.first, val.second);
  }
};

template <typename R>
  requires ranges::range<R> && same_as<ranges::range_value_t<R>, mal::ValuePtr>
struct formatter<R> : range_formatter<mal::ValuePtr> {
  constexpr auto parse(format_parse_context &ctx) {
    return mal::RangeFormatterParse(*this, ctx);
  }
};

template <>
struct formatter<mal::ValuesMap> : range_formatter<mal::ValuesMap::value_type> {
  constexpr auto parse(format_parse_context &ctx) {
    return mal::RangeFormatterParse (*this, ctx);
  }
};

} // namespace std

namespace mal {

namespace detail {

template <typename BASE> class Intern<std::string, BASE> {
public:
  explicit Intern(const std::string & /* name */) noexcept {};

  explicit Intern(const Intern & /* other */) noexcept = default;

  Intern(Intern && /* other */) noexcept = default;

protected:
  template <typename KEY_VIEW> KEY_VIEW asKey() const {
    return static_cast<const BASE *>(this)->data;
  }
};

template <typename UINT, typename BASE>
  requires std::same_as<UINT, std::uint64_t> ||
           std::same_as<UINT, std::uint32_t>
class Intern<UINT, BASE> {
public:
  explicit Intern(const std::string &name)
      : intern{[&]() {
          auto newIntern = ++counter;
          auto it = interns.emplace(name, newIntern);
          if (!it.second) {
            --counter;
            return it.first->second;
          }
          return newIntern;
        }()} {}

  explicit Intern(const Intern &other) noexcept = default;

  Intern(Intern &&other) noexcept = default;

protected:
  template <typename KEY_VIEW>
  KEY_VIEW asKey() const { return intern; }

private:
  std::uint64_t intern{0};

  inline static std::uint64_t counter{0};
  inline static std::unordered_map<std::string, std::uint64_t> interns;
};


} // namespace detail

class Symbol : public StringBase, public detail::Intern<Env::Key, Symbol> {
public:
  explicit Symbol(std::string value, std::optional<std::string> fromMacro = {})
      : StringBase{std::move(value)}, Intern{data},
        fromMacro{std::move(fromMacro)} {}

  explicit Symbol(const Symbol &other)
  : StringBase{other.data}, Intern{other}, fromMacro{other.fromMacro} {}

  Symbol(Symbol &&other) noexcept
      : StringBase{std::move(other.data)}, Intern{std::move(other)},
        fromMacro{std::move(other.fromMacro)} {}

  ValuePtr eval(EnvPtr env) const override;

  Env::KeyView asKey() const { return Intern::asKey<Env::KeyView>(); }

  std::string_view name() const { return data; }

  ValuePtr isEqualTo(ValuePtr) const override;

  friend bool operator==(const Symbol &lhs, const std::string &rhs) {
    return lhs.data == rhs;
  }

  friend bool operator==(const std::string &lhs, const Symbol &rhs) {
    return lhs == rhs.data;
  }

  friend class List;

private:
  std::optional<std::string> fromMacro;
};

inline static Symbol debugEval{"DEBUG-EVAL"};

class Keyword : public StringBase {
public:
  explicit Keyword(std::string value) noexcept : StringBase{std::move(value)} {}
};

class Constant : public StringBase {
public:
  static ValuePtr nilValue() {
    static Constant val{"nil"};
    static ValuePtr ptr{std::addressof(val), [](auto &&) noexcept {}};

    return ptr;
  }

  static ValuePtr trueValue() {
    static Constant val{"true"};
    static ValuePtr ptr{std::addressof(val), [](auto &&) noexcept {}};

    return ptr;
  }

  static ValuePtr falseValue() {
    static Constant val{"false"};
    static ValuePtr ptr{std::addressof(val), [](auto &&) noexcept {}};
    return ptr;
  }

  ValuePtr isEqualTo(ValuePtr rhs) const override;

private:
  explicit Constant(std::string value) noexcept
      : StringBase{std::move(value)} {}

};

class String : public StringBase {
public:
  explicit String(std::string v) noexcept : StringBase{std::move(v)} {}

  std::string print(bool readably) const override;

  const std::string &data() const { return StringBase::data; }

  static std::string unescape(const std::string& in);
  static std::string escape(const std::string &in);
};

class Atom : public Value {
public:
  explicit Atom(ValuePtr value) noexcept : data{std::move(value)} {}

  std::string print(bool readably) const override {
    return readably ? std::format("(atom {:r})", data)
                    : std::format("(atom {})", data);
  }

  ValuePtr isEqualTo(ValuePtr rhs) const override;

  ValuePtr value() const { return data; }

  ValuePtr reset(ValuePtr value) const {
    data = value;
    return value;
  }

private:
  mutable ValuePtr data;
};

class MetaMixIn {
public:
  explicit MetaMixIn(ValuePtr meta = Constant::nilValue())
      : meta_{std::move(meta)} {}

  virtual ~MetaMixIn() = default;

  virtual ValuePtr cloneWithMeta(ValuePtr meta) const = 0;

  ValuePtr meta() const { return meta_; }

private:
  ValuePtr meta_;
};

class Sequence : public Value {
public:
  ValuePtr isEqualTo(ValuePtr rhs) const override;

  ValuesSpan values() const noexcept { return {data.begin(), data.end()}; };

  std::size_t size() const { return data.size(); }

protected:
  explicit Sequence(ValuesContainer data) noexcept
      : data{std::move(data)} {}

  explicit Sequence(ValuesSpan data) noexcept
      : data{data | std::ranges::to<ValuesContainer>()} {}

  template <std::ranges::input_range RANGE>
    requires std::convertible_to<std::ranges::range_reference_t<RANGE>,
                                 ValuePtr>
  explicit Sequence(RANGE &&range)
      : data{std::forward<RANGE>(range) | std::ranges::to<ValuesContainer>()} {}

  ValuesContainer data;
};

class List : public Sequence, public MetaMixIn {
public:
  explicit List(ValuesContainer values) noexcept
      : Sequence{std::move(values)} {}

  explicit List(ValuesSpan values, ValuePtr meta = {}) noexcept
      : Sequence{values}, MetaMixIn{std::move(meta)} {}

  template <std::ranges::input_range RANGE>
    requires std::convertible_to<std::ranges::range_reference_t<RANGE>,
                                 ValuePtr>
  explicit List(RANGE &&range) : Sequence{std::forward<RANGE>(range)} {}

  template <typename... ARGS>
    requires(std::convertible_to<ARGS, ValuePtr> && ...)
  explicit List(ARGS &&...args)
      : Sequence{ValuesContainer{std::forward<ARGS>(args)...}} {}

  std::string print(bool readably) const override;

  InvocableResult invoke(EnvPtr env) const;

  ValuePtr eval(EnvPtr env) const override;

  ValuePtr cloneWithMeta(ValuePtr meta) const override {
    return make<List>(values(), std::move(meta));
  }
};

class Vector : public Sequence, public MetaMixIn {
public:
  explicit Vector(ValuesContainer values) noexcept
      : Sequence{std::move(values)} {}

  explicit Vector(ValuesSpan values, ValuePtr meta = {}) noexcept
      : Sequence{values}, MetaMixIn{std::move(meta)} {}

  template <std::ranges::input_range RANGE>
    requires std::convertible_to<std::ranges::range_reference_t<RANGE>,
                                 ValuePtr>
  explicit Vector(RANGE &&range) : Sequence{std::forward<RANGE>(range)} {}

  std::string print(bool readably) const override {
    return readably ? std::format("[{:r}]", data) : std::format("[{}]", data);
  }

  ValuePtr eval(EnvPtr env) const override;

  ValuePtr cloneWithMeta(ValuePtr meta) const override {
    return make<Vector>(values(), std::move(meta));
  }

};

class Hash : public Value, public MetaMixIn {
public:
  // To ensure that the last element that with a given key takes a precedence
  // over previous elements with the same key, use std::views::reverse to
  // reverse order of elements when creating a map.  Current implementation of
  // libc++ (and I think libstdc++ as well) uses the first encountered element.
  // While this has not been specified in standard [1] we'll take a bet this
  // behaviour won't change.
  //
  // [1] as of March 2026 the C++ standard has not taken a stance on that, see
  //     https://cplusplus.github.io/LWG/issue2844
  template <std::ranges::input_range RANGE>
    requires std::convertible_to<std::ranges::range_reference_t<RANGE>,
                                 ValuePtr>
  explicit Hash(RANGE &&range)
      : data{std::from_range, std::forward<RANGE>(range) |
                                  std::views::chunk(2) | std::views::reverse |
                                  std::views::transform([](auto &&chunk) {
                                    assert(to<StringBase>(chunk[0]));
                                    return std::tie(chunk[0], chunk[1]);
                                  })} {}

  template <std::ranges::input_range RANGE>
    requires std::convertible_to<std::ranges::range_reference_t<RANGE>,
                                 ValuesMap::value_type>
  explicit Hash(RANGE &&range)
      : data{std::from_range, std::forward<RANGE>(range)} {}

  explicit Hash(const Hash &other, ValuesSpan values);

  explicit Hash(const Hash &other, ValuePtr meta)
      : MetaMixIn{std::move(meta)}, data{other.data} {}

  std::string print(bool readably) const override;

  ValuePtr eval(EnvPtr env) const override;

  ValuePtr isEqualTo(ValuePtr rhs) const override;

  ValuePtr find(ValuePtr key) const;

  auto begin() const { return data.begin(); }

  auto end() const { return data.end(); }

  ValuePtr cloneWithMeta(ValuePtr meta) const override {
    return make<Hash>(*this, std::move(meta));
  }

private:
  ValuesMap data;
};

class Invocable : public Value {
public:
  virtual InvocableResult apply(ValuesSpan, EnvPtr) const = 0;
};

class BuiltIn : public Invocable, public MetaMixIn {
public:
  using HandlerFn = InvocableResult(std::string_view name, ValuesSpan value,
                                    EnvPtr Env);

  explicit BuiltIn(std::string name, HandlerFn &handler) noexcept
      : name{std::move(name)}, handler{handler} {}

  explicit BuiltIn(const BuiltIn &other, ValuePtr meta)
      : MetaMixIn{std::move(meta)}, name{other.name}, handler{other.handler} {}

  InvocableResult apply(ValuesSpan value, EnvPtr evalEnv) const override {
    return handler(name, value, evalEnv);
  }

  ValuePtr isEqualTo(ValuePtr rhs) const override;

  std::string print(bool /* readably */) const override {
    return std::format("#<built-in {}@{:p}>", name,
                       reinterpret_cast<const void *>(this));
  }

  Env::Key asKey() const { return Symbol{name}.asKey(); }

  ValuePtr cloneWithMeta(ValuePtr meta) const override {
    return make<BuiltIn>(*this, std::move(meta));
  }

private:
  std::string name;
  HandlerFn &handler;
};

class FunctionBase : public Invocable {
public:
  struct Params : std::vector<Symbol> {
    using std::vector<Symbol>::vector;
  };

  explicit FunctionBase(Params params, ValuePtr body, EnvPtr env);

  explicit FunctionBase(const FunctionBase &other)
      : bindSize{other.bindSize}, params{other.params}, body{other.body},
        capturedEnv{other.capturedEnv} {}

  FunctionBase(FunctionBase &&other) noexcept
      : bindSize{other.bindSize}, params{std::move(other.params)},
        body{std::move(other.body)}, capturedEnv{std::move(other.capturedEnv)} {}

protected:
  template <typename TYPE> ValuePtr isEqualTo(ValuePtr rhs) const;

  EnvPtr makeApplyEnv(ValuesSpan value, EnvPtr evalEnv) const;

  std::size_t bindSize;
  Params params;
  ValuePtr body;
  EnvPtr capturedEnv;
};

} // namespace mal

namespace std {

template <> struct formatter<mal::Symbol> : mal::ParseValueMixin {
  template<typename FORMAT_CONTEXT>
  auto format(const mal::Symbol &val, FORMAT_CONTEXT &ctx) const {
    return format_to(ctx.out(), "{}", val.print(false));
  }
};

template <>
struct std::formatter<mal::FunctionBase::Params>
    : std::range_formatter<mal::Symbol> {
  constexpr formatter() {
    set_brackets("(", ")");
    set_separator(" ");
  }
};

} //namespace std

namespace mal {

class Lambda : public FunctionBase, public MetaMixIn {
public:
  template <std::ranges::input_range RANGE>
    requires std::convertible_to<std::ranges::range_reference_t<RANGE>, Symbol>
  explicit Lambda(RANGE &&params, ValuePtr body, EnvPtr env)
      : FunctionBase{std::forward<RANGE>(params) |
                         std::ranges::to<FunctionBase::Params>(),
                     std::move(body), std::move(env)} {}

  explicit Lambda(const Lambda &other, ValuePtr meta)
      : FunctionBase{other}, MetaMixIn{std::move(meta)} {}

  std::string print(bool readable) const override;

  ValuePtr isEqualTo(ValuePtr rhs) const override;

  InvocableResult apply(ValuesSpan value, EnvPtr evalEnv) const override;

  ValuePtr cloneWithMeta(ValuePtr meta) const override {
    return make<Lambda>(*this, std::move(meta));
  }
};

class Macro : public FunctionBase {
public:
  explicit Macro(const Lambda &other) noexcept : FunctionBase{other} {}

  explicit Macro(Lambda &&other) noexcept : FunctionBase{std::move(other)} {}

  std::string print(bool readable) const override;

  ValuePtr isEqualTo(ValuePtr rhs) const override;

  InvocableResult apply(ValuesSpan value, EnvPtr evalEnv) const override;
};

class Eval : public Invocable, public MetaMixIn {
public:
  explicit Eval(EnvPtr env) : env{std::move(env)} {}

  explicit Eval(const Eval &other, ValuePtr meta)
      : MetaMixIn{std::move(meta)}, env{other.env} {}

  std::string print(bool /* readably */) const override {
    return std::format("#<eval@{:p}>", reinterpret_cast<const void *>(this));
  }

  ValuePtr isEqualTo(ValuePtr rhs) const override;

  InvocableResult apply(ValuesSpan values, EnvPtr evalEnv) const override;

  ValuePtr cloneWithMeta(ValuePtr meta) const override {
    return make<Eval>(*this, std::move(meta));
  }

private:
  EnvPtr env;
};


class EvalException : public std::runtime_error {
public:
  explicit EvalException(const std::string &str) : std::runtime_error{str} {}
};

class MalException : public std::runtime_error {
public:
  MalException(const std::string &str, ValuePtr value)
      : std::runtime_error{str}, value{std::move(value)} {}

  ValuePtr value;
};

} // namespace mal
#endif // INCLUDE_TYPES_H
