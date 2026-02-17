#ifndef INCLUDE_TYPES_H
#define INCLUDE_TYPES_H

#include <cassert>
#include <cstddef>
#include <cstdint>
#include <functional>
#include <memory>
#include <stdexcept>
#include <string>
#include <utility>
#include <vector>
#include <format>
#include <ranges>
#include <unordered_map>
#include <span>

namespace mal {

class Env;
using EnvPtr = std::shared_ptr<Env>;

class Value;
using ValuePtr = std::shared_ptr<Value>;
using ValuesContainer = std::vector<ValuePtr>;
using ValuesSpan = std::span<const ValuePtr>;
using ValuesMap = std::unordered_map<ValuePtr, ValuePtr>;

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
  size_t operator()(const mal::ValuePtr &o) const noexcept {
    assert(dynamic_cast<mal::StringBase *>(o.get()));
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
  template<typename FormatContext>
  auto format(const mal::ValuePtr &val, FormatContext &ctx) const {
    return format_to(ctx.out(), "{}", val->print(readably));
  }
};

template <>
struct formatter<mal::ValuesMap::value_type> : mal::ParseValueMixin {
  template <typename FormatContext>
  auto format(const mal::ValuesMap::value_type &val, FormatContext &ctx) const {
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
  static std::string unescape(std::string in);
  static std::string escape(std::string in);

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
  virtual ValuePtr apply(ValuesSpan) const = 0;
};

class BuiltIn : public Invocable {
public:
  using Handler = ValuePtr (*)(std::string name, ValuesSpan values);

  BuiltIn(std::string n, Handler h) noexcept
      : name{std::move(n)}, handler{h} {}

  ValuePtr apply(ValuesSpan values) const override {
    return handler(name, values);
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
  : params{std::move(p)}, body{b}, env{e} {}

  std::string print(bool readable) const override;
  ValuePtr isEqualTo(ValuePtr rhs) const override;
  ValuePtr apply(ValuesSpan values) const override;

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
