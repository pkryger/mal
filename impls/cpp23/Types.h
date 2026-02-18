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
using ValuePtr = std::shared_ptr<Value>;
using ValuesContainer = std::vector<ValuePtr>;
using ValuesSpan = std::span<const ValuePtr>;
using ValuesMap = std::unordered_map<ValuePtr, ValuePtr>;

using InvocableResult = std::tuple<ValuePtr, EnvPtr, bool>;

template <typename TYPE, typename... ARGS>
std::shared_ptr<std::decay_t<TYPE>> make(ARGS &&...args) {
  return std::make_shared<std::decay_t<TYPE>>(std::forward<ARGS>(args)...);
}

template <typename TYPE> std::decay_t<TYPE> *to(ValuePtr ptr) noexcept {
  return dynamic_cast<std::decay_t<TYPE> *>(ptr.get());
}

class Env {
public:
  explicit Env(EnvPtr o) noexcept : outer{std::move(o)} {}

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
  virtual std::string print(bool) const = 0;
  virtual  ValuePtr eval(EnvPtr) { return shared_from_this(); }

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
  explicit Integer(std::int64_t v) noexcept : data{v} {}
  std::string print(bool) const override { return std::to_string(data); }
  ValuePtr isEqualTo(ValuePtr rhs) const override;

  std::int64_t value() const noexcept { return data; }

private:
  std::int64_t data;
};

class StringBase : public Value {
public:
  std::string print(bool) const override { return data; }
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
    return std::hash<std::string>{}(static_cast<mal::StringBase &>(*o).data);
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
  explicit Symbol(std::string v) noexcept : StringBase{std::move(v)} {}
  ValuePtr eval(EnvPtr env) override;

  const std::string &asKey() const {
    return data;
  }

  friend bool operator==(const Symbol &lhs, const std::string &rhs) {
    return lhs.data == rhs;
  }

  friend bool operator==(const std::string &lhs, const Symbol &rhs) {
    return lhs == rhs.data;
  }

};

class Keyword : public StringBase {
public:
  explicit Keyword(std::string v) noexcept : StringBase{std::move(v)} {}
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
  explicit Constant(std::string v) noexcept : StringBase{std::move(v)} {}

};

class String : public StringBase {
public:
  std::string print(bool readably) const override;
  static std::string unescape(const std::string& in);
  static std::string escape(const std::string& in);

  explicit String(std::string v) noexcept : StringBase{std::move(v)} {}
};

class Sequence : public Value {
public:
  ValuePtr isEqualTo(ValuePtr rhs) const override;
  ValuesSpan values() const noexcept { return {data.begin(), data.end()}; };

protected:
  explicit Sequence(ValuesContainer v) noexcept
      : data{std::move(v)} {}

  ValuesContainer data;
};

class List : public Sequence {
public:
  explicit List(ValuesContainer v) noexcept
      : Sequence{std::move(v)} {}
  std::string print(bool readably) const override {
    return readably ? std::format("({:r})", data) : std::format("({})", data);
  }

  InvocableResult invoke(EnvPtr) const;
  ValuePtr eval(EnvPtr env) override;
};

class Vector : public Sequence {
public:
  explicit Vector(ValuesContainer v) noexcept
      : Sequence{std::move(v)} {}
  std::string print(bool readably) const override {
    return readably ? std::format("[{:r}]", data) : std::format("[{}]", data);
  }

   ValuePtr eval(EnvPtr env) override;
};

class Hash : public Value {
public:
  explicit Hash(ValuesContainer v)
      : data{createMap(std::move(v))} {}

  std::string print(bool) const override;

  ValuePtr eval(EnvPtr env) override;

  ValuePtr isEqualTo(ValuePtr rhs) const override;

  auto& values() const noexcept { return data; }

private:
  static ValuesMap createMap(ValuesContainer);
  ValuesMap data;
};

class Invocable : public Value {
public:
  virtual InvocableResult apply(ValuesSpan, EnvPtr) const = 0;
};

class BuiltIn : public Invocable {
public:
  using Handler = ValuePtr (*)(std::string name, ValuesSpan values);

  BuiltIn(std::string n, Handler h) noexcept
      : name{std::move(n)}, handler{h} {}

  InvocableResult apply(ValuesSpan values, EnvPtr evalEnv) const override {
    return {handler(name, values), evalEnv, false};
  }

  ValuePtr isEqualTo(ValuePtr rhs) const override;

  std::string print(bool readable) const override {
    return readable ? "#'" + name : name;
  }

  const std::string &asKey() const { return name; }

private:
  std::string name;
  Handler handler;
};

class Lambda : public Invocable {
public:
  explicit Lambda(std::vector<std::string> p,  ValuePtr b, EnvPtr e)
  : params{std::move(p)}, body{std::move(b)}, env{std::move(e)} {}

  std::string print(bool readable) const override;
  ValuePtr isEqualTo(ValuePtr rhs) const override;
  InvocableResult apply(ValuesSpan values, EnvPtr evalEnv) const override;

private:
  std::vector<std::string> params;
  ValuePtr body;
  EnvPtr env;
};

class EvalException : public std::runtime_error {
public:
  explicit EvalException(const std::string &str) : std::runtime_error{str} {}
};

} // namespace mal
#endif // INCLUDE_TYPES_H
