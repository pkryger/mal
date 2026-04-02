#ifndef INCLUDE_TYPES_H
#define INCLUDE_TYPES_H

#include "FunctionRef.h"
// NOLINTNEXTLINE(readability-use-concise-preprocessor-directives) - consistent checks
#if !defined(__cpp_lib_ranges_chunk)
#include "Ranges.h"
#endif // __cpp_lib_ranges_chunk
#include "Env.h"
#include "Hierarchy.h"
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
// IWYU pragma: no_include <__bit_reference>

namespace mal {

class EvalException : public std::runtime_error {
public:
  explicit EvalException(const std::string &str) : std::runtime_error{str} {}
};


using ValuesContainer = std::vector<ValuePtr>;
using ValuesMap = std::unordered_map<ValuePtr, ValuePtr>;

class PrintType {
public:
  enum class Type : signed char {
    Simply = 0,
    MalReadably = 1,
    Readably = 2
  };

  // NOLINTNEXTLINE(google-explicit-constructor)
  constexpr PrintType(Type type) noexcept : type_(type) {}

  constexpr explicit operator bool() const noexcept {
    return type_ != Type::Simply;
  }

  [[nodiscard]] constexpr Type type() const noexcept {
    return type_;
  }

  constexpr PrintType() noexcept = default;
  constexpr PrintType(const PrintType &) noexcept = default;
  constexpr PrintType &operator=(const PrintType &) noexcept = default;
  constexpr PrintType(PrintType &&) noexcept = default;
  constexpr PrintType &operator=(PrintType &&) noexcept = default;
  constexpr ~PrintType() noexcept = default;

  constexpr auto operator<=>(const PrintType &) const noexcept = default;
  constexpr bool operator==(const PrintType &) const noexcept = default;

private:
  Type type_{Type::Simply};
};

inline constexpr auto Simply{PrintType::Type::Simply};
inline constexpr auto Readably{PrintType::Type::Readably};
inline constexpr auto MalReadably{PrintType::Type::MalReadably};

class Value : public RttiBase, public GarbageCollectible, public std::enable_shared_from_this<Value> {
public:
  virtual std::string print(PrintType readably) const = 0;

  virtual ValuePtr eval(const EnvPtr &/*env*/) const
  { return shared_from_this(); }

  bool isTrue() const noexcept;

  virtual ValuePtr isEqualTo(ValuePtr rhs) const = 0;

  Value(const Value &) = default;
  Value(Value &&) = default;
  Value &operator=(const Value &) = default;
  Value &operator=(Value &&) = default;
  ~Value() override = default;

protected:
  explicit Value(std::uint32_t lowId) noexcept : RttiBase{lowId} {}

private:
  Value() noexcept : RttiBase{typeInfo<Value>.low_} {}
};


class Integer final : public Value {
public:
  explicit Integer(std::int64_t value) noexcept
      : Value{typeInfo<Integer>.low_}, data_{value} {}

  std::string print(PrintType /*readably*/) const override {
    return std::to_string(data_);
  }

  ValuePtr isEqualTo(ValuePtr rhs) const override;

  std::int64_t value() const noexcept { return data_; }
private:
  std::int64_t data_;
};

namespace detail {

template <typename...> class Intern;

} // namespace detail

class Symbol;

class StringBase : public Value {
public:
  std::string print(PrintType /*readably*/) const override { return data_; }
  ValuePtr isEqualTo(ValuePtr rhs) const override;

  friend bool operator==(const StringBase &lhs,
                         const StringBase &rhs) noexcept {
    return lhs.data_ == rhs.data_;
  }

  friend void swap(StringBase &lhs, StringBase &rhs) noexcept {
    using std::swap;
    swap(lhs.data_, rhs.data_);
  }

  friend std::hash<ValuePtr>;
  friend detail::Intern<std::string, Symbol>;


protected:
  explicit StringBase(std::uint32_t lowId, std::string data) noexcept
      : Value{lowId}, data_{std::move(data)} {}

  explicit StringBase(std::uint32_t lowId, std::string_view data) noexcept
      : Value{lowId}, data_{data} {}

  std::string data_;

private:
  StringBase() noexcept : Value{typeInfo<StringBase>.low_} {}
};

} // namespace mal

// This block need to appear when the StringBase is already defined, but
// before the first instantiation of ValuesMap, such that hash<ValuePtr>
// can be defined
template <> struct std::hash<mal::ValuePtr> {
  std::size_t operator()(const mal::ValuePtr &value) const;
};

namespace mal {

bool operator==(const ValuePtr &lhs, const ValuePtr &rhs);

struct ParseValueMixin {
  constexpr auto parse(std::format_parse_context &ctx) {
    const auto *iter = ctx.begin();
    if (iter != ctx.end()) {
      switch (*iter) {
      case 'r':
        printType_ = Readably;
        ++iter;
        break;
      case 'l':
        printType_ = MalReadably;
        ++iter;
        break;
      }
    }
    return iter;
  }
  PrintType printType_{Simply};
};

template <typename RANGE_FORMATTER>
constexpr auto RangeFormatterParse(RANGE_FORMATTER &formatter,
                                   std::format_parse_context &ctx) {

  formatter.set_brackets("", "");
  formatter.set_separator(" ");
  const auto *iter = ctx.begin();
  if (iter != ctx.end()) {
    switch (*iter) {
    case 'r':
      formatter.underlying().printType_ = Readably;
      ++iter;
      break;
    case 'l':
      formatter.underlying().printType_ = MalReadably;
      ++iter;
      break;
    }
  }
  return iter;
}

} // namespace mal

namespace std {

template <> struct formatter<mal::ValuePtr> : mal::ParseValueMixin {
  template<typename FORMAT_CONTEXT>
  auto format(const mal::ValuePtr &val, FORMAT_CONTEXT &ctx) const {
    return format_to(ctx.out(), "{}", val->print(printType_));
  }
};

template <>
struct formatter<mal::ValuesMap::value_type> : mal::ParseValueMixin {
  template <typename FORMAT_CONTEXT>
  auto format(const mal::ValuesMap::value_type &val,
              FORMAT_CONTEXT &ctx) const {
    switch (printType_.type()) {
    using enum mal::PrintType::Type;
    case Simply:
      return format_to(ctx.out(), "{} {}", val.first, val.second);
      break;
    case Readably:
      return format_to(ctx.out(), "{:r} {:r}", val.first, val.second);
      break;
    case MalReadably:
      return format_to(ctx.out(), "{:l} {:l}", val.first, val.second);
      break;
    }
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
  explicit Intern(const std::string &/*unused*/) noexcept {};

  Intern(const Intern &) noexcept = default;
  Intern& operator=(const Intern &) noexcept = default;
  Intern(Intern &&) noexcept = default;
  Intern& operator=(Intern &&) noexcept = default;
  ~Intern() noexcept = default;

  friend void swap(Intern &/*lhs*/, Intern &/*rhs*/) noexcept {}

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
      : intern_{[&]() {
          auto newIntern = ++counter_;
          auto iter = interns_.emplace(name, newIntern);
          if (!iter.second) {
            --counter_;
            return iter.first->second;
          }
          return newIntern;
        }()} {}

  Intern(const Intern &) noexcept = default;
  Intern& operator=(const Intern &) noexcept = default;
  Intern(Intern &&) noexcept = default;
  Intern& operator=(Intern &&) noexcept = default;
  ~Intern() noexcept = default;

  friend void swap(Intern &lhs, Intern &rhs) noexcept {
    using std::swap;
    swap(lhs.intern_, rhs.intern_);
  }

protected:
  template <typename KEY_VIEW>
  [[nodiscard]] KEY_VIEW asKey() const { return intern_; }

private:
  UINT intern_{0};
  inline static UINT counter_{0};
  inline static std::unordered_map<std::string, UINT> interns_;
};


} // namespace detail

class Symbol final : public StringBase,
                     public detail::Intern<Env::Key, Symbol> {
public:
  explicit Symbol(std::string_view value, std::optional<std::string> fromMacro = {})
      : StringBase{typeInfo<Symbol>.low_, value}, Intern{data_},
        fromMacro_{std::move(fromMacro)} {}

  Symbol(const Symbol &other)
      : StringBase{typeInfo<Symbol>.low_, other.data_}, Intern{other},
        fromMacro_{other.fromMacro_} {}

  Symbol &operator=(const Symbol &other) {
    Symbol temp{other};
    swap(*this, temp);
    return *this;
  }

  Symbol(Symbol &&other) noexcept
      : StringBase{typeInfo<Symbol>.low_, std::move(other.data_)},
        Intern{std::move(other)} {
    swap(*this, other);
  }

  Symbol &operator=(Symbol &&other) noexcept {
    StringBase::operator=(std::move(other));
    Intern::operator=(std::move(other));
    fromMacro_ = std::move(other.fromMacro_);
    return *this;
  }

  ~Symbol() override = default;

  ValuePtr eval(const EnvPtr &env) const override;

  Env::KeyView asKey() const { return Intern::asKey<Env::KeyView>(); }

  std::string_view name() const { return data_; }

  ValuePtr isEqualTo(ValuePtr /*rhs*/) const override;

  friend bool operator==(const Symbol &lhs, const std::string &rhs) {
    return lhs.data_ == rhs;
  }

  friend bool operator==(const std::string &lhs, const Symbol &rhs) {
    return lhs == rhs.data_;
  }

  friend void swap(Symbol &lhs, Symbol &rhs) noexcept {
    using std::swap;
    swap(lhs.fromMacro_, rhs.fromMacro_);
  }

  friend class List;

private:
  std::optional<std::string> fromMacro_;
};

inline static const Symbol debugEval{"DEBUG-EVAL"};

class Keyword final : public StringBase {
public:
  explicit Keyword(std::string_view value) noexcept
      : StringBase{typeInfo<Keyword>.low_, value} {}
};

class Constant : public StringBase {
public:
  static const ValuePtr &nilValue() {
    static const Constant val{"nil"};
    static const ValuePtr ptr{std::addressof(val), [](auto &&) noexcept {}};
    return ptr;
  }

  static const ValuePtr &trueValue() {
    static const Constant val{"true"};
    static const ValuePtr ptr{std::addressof(val), [](auto &&) noexcept {}};
    return ptr;
  }

  static const ValuePtr &falseValue() {
    static const Constant val{"false"};
    static const ValuePtr ptr{std::addressof(val), [](auto &&) noexcept {}};
    return ptr;
  }

  ValuePtr isEqualTo(ValuePtr rhs) const override;

private:
  explicit Constant(std::string value) noexcept
      : StringBase{typeInfo<Constant>.low_, std::move(value)} {}
};

class String final : public StringBase {
public:
  explicit String(std::string data) noexcept
      : StringBase{typeInfo<String>.low_, std::move(data)} {}

  std::string print(PrintType readably) const override;

  const std::string &data() const { return StringBase::data_; }
};

class Atom final : public Value {
public:
  explicit Atom(ValuePtr value) noexcept
      : Value{typeInfo<Atom>.low_}, data_{std::move(value)} {}

  std::string print(PrintType readably) const override {
    return readably ? std::format("(atom {:r})", data_)
                    : std::format("(atom {})", data_);
  }

  ValuePtr isEqualTo(ValuePtr rhs) const override;

  ValuePtr value() const { return data_; }

  ValuePtr reset(ValuePtr value) const {
    data_ = value;
    return value;
  }

private:
  mutable ValuePtr data_;
};

class MetaMixIn {
public:
  explicit MetaMixIn(ValuePtr meta = Constant::nilValue())
      : meta_{std::move(meta)} {}

  MetaMixIn(const MetaMixIn &) noexcept = default;
  MetaMixIn &operator=(const MetaMixIn &) noexcept = default;
  MetaMixIn(MetaMixIn &&) noexcept = default;
  MetaMixIn& operator=(MetaMixIn &&) noexcept = default;
  virtual ~MetaMixIn() = default;

  [[nodiscard]] virtual ValuePtr cloneWithMeta(ValuePtr meta) const = 0;

  [[nodiscard]] ValuePtr meta() const { return meta_; }

private:
  ValuePtr meta_;
};

class Sequence : public Value {
public:
  ValuePtr isEqualTo(ValuePtr rhs) const override;

  ValuesSpan values() const noexcept { return {data.begin(), data.end()}; };

  std::size_t size() const { return data.size(); }

protected:
  explicit Sequence(std::uint32_t lowId, ValuesContainer data) noexcept
      : Value{lowId}, data{std::move(data)} {}

  explicit Sequence(std::uint32_t lowId, ValuesSpan data) noexcept
      : Value{lowId}, data{std::from_range, data} {}

  template <std::ranges::input_range RANGE>
    requires std::convertible_to<std::ranges::range_reference_t<RANGE>,
                                 ValuePtr>
  explicit Sequence(std::uint32_t lowId, RANGE &&range)
      : Value{lowId}, data{std::from_range, std::forward<RANGE>(range)} {}

  ValuesContainer data;

private:
  Sequence() : Value{typeInfo<Sequence>.low_} {}
};

class List final : public Sequence, public MetaMixIn {
public:
  explicit List(ValuesContainer values) noexcept
      : Sequence{typeInfo<List>.low_, std::move(values)} {}

  explicit List(ValuesSpan values, ValuePtr meta = {}) noexcept
      : Sequence{typeInfo<List>.low_, values}, MetaMixIn{std::move(meta)} {}

  template <std::ranges::input_range RANGE>
    requires std::convertible_to<std::ranges::range_reference_t<RANGE>,
                                 ValuePtr>
  explicit List(RANGE &&range)
      : Sequence{typeInfo<List>.low_, std::forward<RANGE>(range)} {}

  template <typename... ARGS>
    requires(std::convertible_to<ARGS, ValuePtr> && ...)
  explicit List(ARGS &&...args)
      : Sequence{typeInfo<List>.low_,
                 ValuesContainer{std::forward<ARGS>(args)...}} {}

  std::string print(PrintType readably) const override;

  ValuePtr eval(const EnvPtr &env) const override;

  ValuePtr cloneWithMeta(ValuePtr meta) const override {
    return make<List>(values(), std::move(meta));
  }
};

class Vector final : public Sequence, public MetaMixIn {
public:
  explicit Vector(ValuesContainer values) noexcept
      : Sequence{typeInfo<Vector>.low_, std::move(values)} {}

  explicit Vector(ValuesSpan values, ValuePtr meta = {}) noexcept
      : Sequence{typeInfo<Vector>.low_, values}, MetaMixIn{std::move(meta)} {}

  template <std::ranges::input_range RANGE>
    requires std::convertible_to<std::ranges::range_reference_t<RANGE>,
                                 ValuePtr>
  explicit Vector(RANGE &&range)
      : Sequence{typeInfo<Vector>.low_, std::forward<RANGE>(range)} {}

  std::string print(PrintType readably) const override {
    return readably ? std::format("[{:r}]", data) : std::format("[{}]", data);
  }

  ValuePtr eval(const EnvPtr &env) const override;

  ValuePtr cloneWithMeta(ValuePtr meta) const override {
    return make<Vector>(values(), std::move(meta));
  }

};

class Hash final : public Value, public MetaMixIn {
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
  template <std::ranges::random_access_range RANGE>
    requires std::convertible_to<std::ranges::range_reference_t<RANGE>,
                                 ValuePtr>
  explicit Hash(RANGE &&range)
      : Value{typeInfo<Hash>.low_},
        data_{std::from_range, std::forward<RANGE>(range) |
#ifdef __cpp_lib_ranges_chunk
                                  std::views::chunk(2)
#else
                                  mal::views::Chunk(2)
#endif //__cpp_lib_ranges_chunk
                                  | std::views::reverse |
                                  std::views::transform([](auto &&chunk) {
                                    assert(chunk[0]->template isa<StringBase>());
                                    return std::tie(chunk[0], chunk[1]);
                                  })} {
  }

  template <std::ranges::input_range RANGE>
    requires std::convertible_to<std::ranges::range_reference_t<RANGE>,
                                 ValuesMap::value_type>
  explicit Hash(RANGE &&range)
      : Value{typeInfo<Hash>.low_},
        data_{std::from_range, std::forward<RANGE>(range)} {}

  explicit Hash(const Hash &other, ValuesSpan values);

  explicit Hash(const Hash &other, ValuePtr meta)
      : Value{typeInfo<Hash>.low_}, MetaMixIn{std::move(meta)}, data_{other.data_} {
  }

  std::string print(PrintType readably) const override;

  ValuePtr eval(const EnvPtr &env) const override;

  ValuePtr isEqualTo(ValuePtr rhs) const override;

  ValuePtr find(const ValuePtr &key) const;

  auto begin() const { return data_.begin(); }

  auto end() const { return data_.end(); }

  ValuePtr cloneWithMeta(ValuePtr meta) const override {
    return make<Hash>(*this, std::move(meta));
  }

private:
  ValuesMap data_;
};

class Invocable : public Value {
public:
  virtual InvocableResult apply(bool evaled, ValuesSpan values,
                                const EnvPtr &evalEnv) const = 0;

protected:
  explicit Invocable(std::uint32_t lowId) : Value{lowId} {}

private:
  Invocable() : Value{typeInfo<Invocable>.low_} {}

};

class BuiltIn final : public Invocable, public MetaMixIn {
public:
  using HandlerFn = InvocableResult(std::string_view name, ValuesSpan value,
                                    const EnvPtr &Env);

  explicit BuiltIn(std::string name, HandlerFn &handler) noexcept
      : Invocable{typeInfo<BuiltIn>.low_}, name_{std::move(name)},
        handler_{handler} {}

  explicit BuiltIn(const BuiltIn &other, ValuePtr meta)
      : Invocable{typeInfo<BuiltIn>.low_}, MetaMixIn{std::move(meta)},
        name_{other.name_}, handler_{other.handler_} {}

  InvocableResult apply(bool evaled, ValuesSpan values,
                        const EnvPtr &evalEnv) const override;

  ValuePtr isEqualTo(ValuePtr rhs) const override;

  std::string print(PrintType /*readably*/) const override {
    return std::format("#<built-in {}@{:p}>", name_,
                       static_cast<const void *>(this));
  }

  Env::Key asKey() const { return Symbol{name_}.asKey(); }

  ValuePtr cloneWithMeta(ValuePtr meta) const override {
    return make<BuiltIn>(*this, std::move(meta));
  }

private:
  std::string name_;
  FunctionRef<HandlerFn> handler_;
};

class FunctionBase : public Invocable {
public:
  struct Params : std::vector<Symbol> {
    using std::vector<Symbol>::vector;
  };

  explicit FunctionBase(std::uint32_t lowId, Params params, ValuePtr body,
                        EnvPtr env);

protected:
  explicit FunctionBase(std::uint32_t lowId, const FunctionBase &other)
      : Invocable{lowId}, bindSize_{other.bindSize_}, params_{other.params_},
        body_{other.body_}, capturedEnv_{other.capturedEnv_} {}

  // NOLINTNEXTLINE(cppcoreguidelines-rvalue-reference-param-not-moved)
  explicit FunctionBase(std::uint32_t lowId, FunctionBase &&other) noexcept
      : Invocable{lowId}, bindSize_{other.bindSize_},
        params_{std::move(other.params_)}, body_{std::move(other.body_)},
        capturedEnv_{std::move(other.capturedEnv_)} {}

  template <typename TYPE>
  ValuePtr isEqualImpl(const ValuePtr &rhs) const;

  template <typename VALUES>
  EnvPtr makeApplyEnv(VALUES &&values, const EnvPtr &evalEnv) const;

  std::size_t bindSize_;
  Params params_;
  ValuePtr body_;
  EnvPtr capturedEnv_;
};

} // namespace mal

namespace std {

template <> struct formatter<mal::Symbol> : mal::ParseValueMixin {
  template<typename FORMAT_CONTEXT>
  auto format(const mal::Symbol &val, FORMAT_CONTEXT &ctx) const {
    return format_to(ctx.out(), "{}", val.print(mal::Simply));
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

class Lambda final : public FunctionBase, public MetaMixIn {
public:
  template <std::ranges::input_range RANGE>
    requires std::convertible_to<std::ranges::range_reference_t<RANGE>, Symbol>
  explicit Lambda(RANGE &&params, ValuePtr body, EnvPtr env)
      : FunctionBase{typeInfo<Lambda>.low_,
                     {std::from_range, std::forward<RANGE>(params)},
                     std::move(body),
                     std::move(env)} {}

  explicit Lambda(const Lambda &other, ValuePtr meta)
      : FunctionBase{typeInfo<Lambda>.low_, other}, MetaMixIn{std::move(meta)} {}

  std::string print(PrintType readable) const override;

  ValuePtr isEqualTo(ValuePtr rhs) const override;

  InvocableResult apply(bool evaled, ValuesSpan values,
                        const EnvPtr &evalEnv) const override;

  ValuePtr cloneWithMeta(ValuePtr meta) const override {
    return make<Lambda>(*this, std::move(meta));
  }
};

class Macro final : public FunctionBase {
public:
  explicit Macro(const Lambda &other)
      : FunctionBase{typeInfo<Macro>.low_, other} {}

  explicit Macro(Lambda &&other) noexcept
      : FunctionBase{typeInfo<Macro>.low_, std::move(other)} {}

  std::string print(PrintType readable) const override;

  ValuePtr isEqualTo(ValuePtr rhs) const override;

  InvocableResult apply(bool /*evaled*/, ValuesSpan values,
                        const EnvPtr &evalEnv) const override;

};

class Eval final : public Invocable, public MetaMixIn {
public:
  explicit Eval(EnvPtr env)
      : Invocable{typeInfo<Eval>.low_}, env_{std::move(env)} {}

  explicit Eval(const Eval &other, ValuePtr meta)
      : Invocable{typeInfo<Eval>.low_}, MetaMixIn{std::move(meta)},
        env_{other.env_} {}

  std::string print(PrintType /*readably*/) const override {
    return std::format("#<eval@{:p}>", static_cast<const void *>(this));
  }

  ValuePtr isEqualTo(ValuePtr rhs) const override;

  InvocableResult apply(bool evaled, ValuesSpan values,
                        const EnvPtr &evalEnv) const override;

  ValuePtr cloneWithMeta(ValuePtr meta) const override {
    return make<Eval>(*this, std::move(meta));
  }

private:
  EnvPtr env_;
};

class MalException : public std::runtime_error {
public:
  MalException(const std::string &str, ValuePtr value)
      : std::runtime_error{str}, value_{std::move(value)} {}

  ValuePtr value_;
};

} // namespace mal
#endif // INCLUDE_TYPES_H
