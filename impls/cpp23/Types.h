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

class MalEnv;
using MalEnvPtr = std::shared_ptr<MalEnv>;

class MalValue;
using MalValuePtr = std::shared_ptr<MalValue>;
using MalValueVec = std::vector<MalValuePtr>;
using MalValues = std::span<const MalValuePtr>;
using MalValueIter =  MalValueVec::iterator;
using MalValueCIter = MalValueVec::const_iterator;
using MalValueMap = std::unordered_map<MalValuePtr, MalValuePtr>;

class MalEnv {
public:
  explicit MalEnv(MalEnvPtr o) noexcept : outer{std::move(o)} {}

  void insert_or_assign(std::string key, MalValuePtr value) {
    assert(value);
    map.insert_or_assign(std::move(key), std::move(value));
  }

  MalValuePtr find(const std::string& key) const;

private:
  MalEnvPtr outer;
  std::unordered_map<std::string, MalValuePtr> map;
};


class MalValue : public std::enable_shared_from_this<MalValue> {
public:
  virtual std::string print(bool) const = 0;
  virtual MalValuePtr eval(MalEnvPtr) { return shared_from_this(); }

  bool isTrue() const noexcept;
  virtual MalValuePtr isEqualTo(MalValuePtr rhs) const = 0;

  virtual ~MalValue() = default;

  MalValue(const MalValue &) = delete;
  MalValue(MalValue &&) = delete;
  MalValue &operator=(const MalValue &) = delete;
  MalValue &operator=(MalValue &&) = delete;

protected:
  MalValue() = default;
};

class MalInteger : public MalValue {
public:
  explicit MalInteger(std::int64_t v) noexcept : data{v} {}
  std::string print(bool) const override { return std::to_string(data); }
  MalValuePtr isEqualTo(MalValuePtr rhs) const override;

  std::int64_t value() const noexcept { return data; }

private:
  std::int64_t data;
};

class MalStringBase : public MalValue {
public:
  std::string print(bool) const override { return data; }
  MalValuePtr isEqualTo(MalValuePtr rhs) const override;

  friend bool operator==(const MalStringBase &lhs,
                         const MalStringBase &rhs) noexcept {
    return lhs.data == rhs.data;
  }

  friend std::hash<MalValuePtr>;

protected:
  explicit MalStringBase(std::string v) noexcept : data{std::move(v)} {}
  std::string data;
};

// This block need to appear when the MalStringBase is already defined, but
// before the first instantiation of MalValueMap, such that hash<MalValuePtr>
// can be defined
namespace std {

template <> struct hash<MalValuePtr> {
  size_t operator()(const MalValuePtr &o) const noexcept {
    assert(dynamic_cast<MalStringBase *>(o.get()));
    return std::hash<std::string>{}(static_cast<MalStringBase &>(*o).data);
  }
};

struct mal_parse_value_mixin {
  constexpr auto parse(format_parse_context &ctx) {
    auto it = ctx.begin();
    if (it != ctx.end() && *it == 'r') {
      readably = true;
      ++it;
    }
    return it;
  }
  bool readably{false};
};

template <> struct formatter<MalValuePtr> : mal_parse_value_mixin {
  template<typename FormatContext>
  auto format(const MalValuePtr &val, FormatContext &ctx) const {
    return format_to(ctx.out(), "{}", val->print(readably));
  }
};

template <>
struct formatter<MalValueMap::value_type> : mal_parse_value_mixin {
  template <typename FormatContext>
  auto format(const MalValueMap::value_type &val, FormatContext &ctx) const {
    if (readably) {
      return format_to(ctx.out(), "{:r} {:r}", val.first, val.second);
    }
    return format_to(ctx.out(), "{} {}", val.first, val.second);
  }
};

template <typename RANGE_FORMATTER>
constexpr auto mal_range_formatter_parse(RANGE_FORMATTER &rf,
                                         format_parse_context &ctx) {
  rf.set_brackets("", "");
  rf.set_separator(" ");
  auto it = ctx.begin();
  if (it != ctx.end() && *it == 'r') {
    rf.underlying().readably = true;
    ++it;
  }
  return it;
}

template <typename R>
  requires ranges::range<R> &&
           same_as<ranges::range_value_t<R>, MalValuePtr>
struct formatter<R> : range_formatter<MalValuePtr> {
  constexpr auto parse(format_parse_context &ctx) {
    return mal_range_formatter_parse(*this, ctx);
  }
};

template <>
struct formatter<MalValueMap> : range_formatter<MalValueMap::value_type> {
  constexpr auto parse(format_parse_context &ctx) {
    return mal_range_formatter_parse (*this, ctx);
  }
};

} // namespace std


class MalSymbol : public MalStringBase {
public:
  explicit MalSymbol(std::string v) noexcept : MalStringBase{std::move(v)} {}
  MalValuePtr eval(MalEnvPtr env) override;

  const std::string &asKey() const {
    return data;
  }

  friend bool operator==(const MalSymbol &lhs, const std::string &rhs) {
    return lhs.data == rhs;
  }

  friend bool operator==(const std::string &lhs, const MalSymbol &rhs) {
    return lhs == rhs.data;
  }

};

class MalKeyword : public MalStringBase {
public:
  explicit MalKeyword(std::string v) noexcept : MalStringBase{std::move(v)} {}
};

class MalConstant : public MalStringBase {
public:
  static MalValuePtr nilValue() {
    static MalConstant val{"nil"};
    static MalValuePtr ptr{std::addressof(val), [](auto &&) noexcept {}};

    return ptr;
  }

  static MalValuePtr trueValue() {
    static MalConstant val{"true"};
    static MalValuePtr ptr{std::addressof(val), [](auto &&) noexcept {}};

    return ptr;
  }

  static MalValuePtr falseValue() {
    static MalConstant val{"false"};
    static MalValuePtr ptr{std::addressof(val), [](auto &&) noexcept {}};
    return ptr;
  }

  MalValuePtr isEqualTo(MalValuePtr rhs) const override;

private:
  explicit MalConstant(std::string v) noexcept : MalStringBase{std::move(v)} {}

};

class MalString : public MalStringBase {
public:
  std::string print(bool readably) const override;
  static std::string unescape(std::string in);
  static std::string escape(std::string in);

  explicit MalString(std::string v) noexcept : MalStringBase{std::move(v)} {}
};

class MalSequence : public MalValue {
public:
  MalValuePtr isEqualTo(MalValuePtr rhs) const override;
  MalValues values() const noexcept { return {data.begin(), data.end()}; };

protected:
  explicit MalSequence(MalValueVec v) noexcept
      : data{std::move(v)} {}

  MalValueVec data;
};

class MalList : public MalSequence {
public:
  explicit MalList(MalValueVec v) noexcept
      : MalSequence{std::move(v)} {}
  std::string print(bool readably) const override {
    return readably ? std::format("({:r})", data) : std::format("({})", data);
  }

  MalValuePtr eval(MalEnvPtr env) override;
};

class MalVector : public MalSequence {
public:
  explicit MalVector(MalValueVec v) noexcept
      : MalSequence{std::move(v)} {}
  std::string print(bool readably) const override {
    return readably ? std::format("[{:r}]", data) : std::format("[{}]", data);
  }

  MalValuePtr eval(MalEnvPtr env) override;
};

class MalHash : public MalValue {
public:
  explicit MalHash(MalValueVec v)
      : data{createMap(std::move(v))} {}

  std::string print(bool) const override;

  MalValuePtr eval(MalEnvPtr env) override;

  MalValuePtr isEqualTo(MalValuePtr rhs) const override;

  auto& values() const noexcept { return data; }

private:
  static MalValueMap createMap(MalValueVec);
  MalValueMap data;
};

class MalFunction : public MalValue {
public:
  virtual MalValuePtr apply(MalValues) const = 0;
};

class MalBuiltIn : public MalFunction {
public:
  using Handler = MalValuePtr (*)(std::string name, MalValues values);

  MalBuiltIn(std::string n, Handler h) noexcept
      : name{std::move(n)}, handler{h} {}

  MalValuePtr apply(MalValues values) const override {
    return handler(name, values);
  }

  MalValuePtr isEqualTo(MalValuePtr rhs) const override;

  std::string print(bool readable) const override {
    return readable ? "#'" + name : name;
  }

  const std::string &asKey() const { return name; }

private:
  std::string name;
  Handler handler;
};

class MalLambda : public MalFunction {
public:
  explicit MalLambda(std::vector<std::string> p, MalValuePtr b, MalEnvPtr e)
  : params{std::move(p)}, body{b}, env{e} {}

  std::string print(bool readable) const override;
  MalValuePtr isEqualTo(MalValuePtr rhs) const override;
  MalValuePtr apply(MalValues values) const override;

private:
  std::vector<std::string> params;
  MalValuePtr body;
  MalEnvPtr env;
};

class EvalException : public std::runtime_error {
public:
  explicit EvalException(const std::string &str) : std::runtime_error{str} {}
};

#endif // INCLUDE_TYPES_H
