#ifndef INCLUDE_TYPES_H
#define INCLUDE_TYPES_H

#include <cassert>
#include <concepts>
#include <cstddef>
#include <cstdint>
#include <format>
#include <functional> // IWYU pragma: keep
#include <memory>
#include <ranges>
#include <span>
#include <stdexcept>
#include <string>
#include <tuple>
#include <type_traits>
#include <unordered_map>
#include <utility>
#include <vector> // IWYU pragma: keep
// IWYU pragma: no_include <__vector/vector.h>

namespace mal {

class Env;
using EnvPtr = std::shared_ptr<Env>;

class Value;
using ValuePtr = std::shared_ptr<const Value>;
using ValuesContainer = std::vector<ValuePtr>;
using ValuesSpan = std::span<const ValuePtr>;
using ValuesMap = std::unordered_map<ValuePtr, ValuePtr>;

using InvocableResult = std::tuple<ValuePtr, EnvPtr, bool>;

template <typename TYPE, typename... ARGS>
std::shared_ptr<std::decay_t<TYPE>> make(ARGS &&...args) {
  return std::make_shared<std::decay_t<TYPE>>(std::forward<ARGS>(args)...);
}

template <typename TYPE> const std::decay_t<TYPE> *to(ValuePtr ptr) noexcept {
  return dynamic_cast<const std::decay_t<TYPE> *>(ptr.get());
}

class Env {
public:
  explicit Env(EnvPtr outer) noexcept : outer{std::move(outer)} {}

  void insert_or_assign(std::string key, ValuePtr value) {
    assert(value);
    map.insert_or_assign(std::move(key), std::move(value));
  }

  ValuePtr find(const std::string& key) const;

private:
  EnvPtr outer;
  std::unordered_map<std::string, ValuePtr> map;
};


class Value : public std::enable_shared_from_this<Value> {
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

class StringBase : public Value {
public:
  std::string print(bool /* readably */) const override { return data; }
  ValuePtr isEqualTo(ValuePtr rhs) const override;

  friend bool operator==(const StringBase &lhs,
                         const StringBase &rhs) noexcept {
    return lhs.data == rhs.data;
  }

  friend std::hash<ValuePtr>;

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

class Symbol : public StringBase {
public:
  explicit Symbol(std::string value, std::optional<std::string> macro = {}) noexcept
      : StringBase{std::move(value)}, macro{std::move(macro)} {}

  ValuePtr eval(EnvPtr env) const override;

  const std::string &asKey() const {
    return data;
  }

  friend bool operator==(const Symbol &lhs, const std::string &rhs) {
    return lhs.data == rhs;
  }

  friend bool operator==(const std::string &lhs, const Symbol &rhs) {
    return lhs == rhs.data;
  }

  friend class List;
private:
  std::optional<std::string> macro;
};

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
  std::string print(bool readably) const override;
  static std::string unescape(const std::string& in);
  static std::string escape(const std::string& in);

  explicit String(std::string v) noexcept : StringBase{std::move(v)} {}
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

class List : public Sequence {
public:
  explicit List(ValuesContainer values) noexcept
      : Sequence{std::move(values)} {}

  explicit List(ValuesSpan values) noexcept : Sequence{values} {}

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
};

class Vector : public Sequence {
public:
  explicit Vector(ValuesContainer values) noexcept
      : Sequence{std::move(values)} {}

  explicit Vector(ValuesSpan values) noexcept : Sequence{values} {}

  template <std::ranges::input_range RANGE>
    requires std::convertible_to<std::ranges::range_reference_t<RANGE>,
                                 ValuePtr>
  explicit Vector(RANGE &&range) : Sequence{std::forward<RANGE>(range)} {}

  std::string print(bool readably) const override {
    return readably ? std::format("[{:r}]", data) : std::format("[{}]", data);
  }

  ValuePtr eval(EnvPtr env) const override;
};

class Hash : public Value {
public:
  explicit Hash(ValuesContainer value = {})
      : data{createMap(std::move(value))} {}

  std::string print(bool readably) const override;

  ValuePtr eval(EnvPtr env) const override;

  ValuePtr isEqualTo(ValuePtr rhs) const override;

  auto& value() const noexcept { return data; }

private:
  static ValuesMap createMap(ValuesContainer value);
  ValuesMap data;
};

class Invocable : public Value {
public:
  virtual InvocableResult apply(ValuesSpan, EnvPtr) const = 0;
};

class BuiltIn : public Invocable {
public:
  using HandlerFn = ValuePtr (std::string name, ValuesSpan value, EnvPtr Env);

  explicit BuiltIn(std::string name, HandlerFn &handler) noexcept
      : name{std::move(name)}, handler{handler} {}

  InvocableResult apply(ValuesSpan value, EnvPtr evalEnv) const override {
    return {handler(name, value, evalEnv), evalEnv, false};
  }

  ValuePtr isEqualTo(ValuePtr rhs) const override;

  std::string print(bool /* readably */) const override {
    return std::format("#<built-in {}@{:p}>", name,
                       reinterpret_cast<const void *>(this));
  }

  const std::string &asKey() const { return name; }

private:
  std::string name;
  HandlerFn &handler;
};

class Lambda : public Invocable {
public:
  explicit Lambda(std::vector<std::string> params, ValuePtr body, EnvPtr env);

  std::string print(bool readably) const override;

  ValuePtr isEqualTo(ValuePtr rhs) const override;

  InvocableResult apply(ValuesSpan value, EnvPtr evalEnv) const override;

private:
  std::size_t bindSize;
  std::vector<std::string> params;
  ValuePtr body;
  EnvPtr captureEnv;
};

class Eval : public Invocable {
public:
  explicit Eval(EnvPtr env) : env{std::move(env)} {}

  std::string print(bool /* readably */) const override {
    return std::format("#<eval@{:p}>", reinterpret_cast<const void *>(this));
  }

  ValuePtr isEqualTo(ValuePtr rhs) const override;

  InvocableResult apply(ValuesSpan values, EnvPtr evalEnv) const override;

private:
  EnvPtr env;
};


class EvalException : public std::runtime_error {
public:
  explicit EvalException(const std::string &str) : std::runtime_error{str} {}
};

} // namespace mal
#endif // INCLUDE_TYPES_H
