#ifndef INCLUDE_TYPES_H
#define INCLUDE_TYPES_H

#include <cassert>
#include <cstddef>
#include <cstdint>
#include <functional>
#include <memory>
#include <stdexcept>
#include <string>
#include <vector>
#include <unordered_map>

class MalEnv;
using MalEnvPtr = std::shared_ptr<MalEnv>;

class MalValue;
using MalValuePtr = std::shared_ptr<MalValue>;
using MalValueVec = std::vector<MalValuePtr>;
using MalValueIter =  MalValueVec::iterator;
using MalValueCIter =  MalValueVec::const_iterator;

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

  std::int64_t value() const noexcept { return data; }

private:
  std::int64_t data;
};

class MalStringBase : public MalValue {
public:
  std::string print(bool) const override { return data; }

  friend bool operator==(const MalStringBase &lhs,
                         const MalStringBase &rhs) noexcept {
    return lhs.data == rhs.data;
  }

  friend std::hash<MalValuePtr>;

protected:
  explicit MalStringBase(std::string v) noexcept : data{std::move(v)} {}
  std::string data;
};

namespace std {

template <> struct hash<MalValuePtr> {
  size_t operator()(const MalValuePtr &o) const noexcept {
    assert(dynamic_cast<MalStringBase *>(o.get()));
    return std::hash<std::string>{}(static_cast<MalStringBase &>(*o).data);
  }
};

} // namespace std

class MalSymbol : public MalStringBase {
public:
  explicit MalSymbol(std::string v) noexcept : MalStringBase{std::move(v)} {}
  MalValuePtr eval(MalEnvPtr env) override;
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
  auto &values() const noexcept{ return data; };

protected:
  explicit MalSequence(MalValueVec v) noexcept
      : data{std::move(v)} {}
  std::string doPrint(bool readably, char open, char close) const;

  MalValueVec data;
};

class MalList : public MalSequence {
public:
  explicit MalList(MalValueVec v) noexcept
      : MalSequence{std::move(v)} {}
  std::string print(bool readably) const override {
    return doPrint(readably, '(', ')');
  }

  MalValuePtr eval(MalEnvPtr env) override;
};

class MalVector : public MalSequence {
public:
  explicit MalVector(MalValueVec v) noexcept
      : MalSequence{std::move(v)} {}
  std::string print(bool readably) const override {
    return doPrint(readably, '[', ']');
  }

  MalValuePtr eval(MalEnvPtr env) override;
};

class MalHash : public MalValue {
public:
  explicit MalHash(MalValueVec v)
      : data{createMap(std::move(v))} {}

  std::string print(bool) const override;

  MalValuePtr eval(MalEnvPtr env) override;

  auto& values() const noexcept { return data; }

private:
  static std::unordered_map<MalValuePtr, MalValuePtr>
      createMap(MalValueVec);
  std::unordered_map<MalValuePtr, MalValuePtr> data;
};

class MalFunction : public MalValue {
public:
  virtual MalValuePtr apply(MalValueIter first, MalValueIter last) const = 0;
};

class MalBuiltIn : public MalFunction {
public:
  typedef MalValuePtr(Func)(MalValueIter first,
                            MalValueIter last);

  MalBuiltIn(std::string n, Func *h) noexcept
      : name{std::move(n)}, handler{h} {}

  MalValuePtr apply(MalValueIter first,
                    MalValueIter last) const override {
    return handler(first, last);
  }

  std::string print(bool readable) const override {
    return readable ? "#'" + name : name;
  }

private:
  std::string name;
  Func* handler;
};

class EvalException : public std::runtime_error {
public:
  explicit EvalException(const std::string &str) : std::runtime_error{str} {}
};

#endif // INCLUDE_TYPES_H
