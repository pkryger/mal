#ifndef INCLUDE_TYPES_H
#define INCLUDE_TYPES_H

#include <cassert>
#include <cstddef>
#include <cstdint>
#include <memory>
#include <stdexcept>
#include <string>
#include <vector>
#include <unordered_map>

class MalValue {
public:
  virtual std::string print(bool) const = 0;
  virtual ~MalValue() = default;

  MalValue(const MalValue &) = delete;
  MalValue(MalValue &&) = delete;
  MalValue &operator=(const MalValue &) = delete;
  MalValue &operator=(MalValue &&) = delete;

protected:
  MalValue() = default;
};

typedef std::shared_ptr<MalValue> MalValuePtr;
typedef std::vector<MalValuePtr> MalValueVec;

class MalInteger : public MalValue {
public:
  explicit MalInteger(std::int64_t v) noexcept : data{v} {}
  std::string print(bool) const override { return std::to_string(data); }

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

private:
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
protected:
  explicit MalSequence(std::vector<MalValuePtr> v) noexcept
      : data{std::move(v)} {}
  std::string doPrint(bool readably, char open, char close) const;

private:
  std::vector<MalValuePtr> data;

};

class MalList : public MalSequence {
public:
  explicit MalList(std::vector<MalValuePtr> v) noexcept
      : MalSequence{std::move(v)} {}
  std::string print(bool readably) const override {
    return doPrint(readably, '(', ')');
  }
};

class MalVector : public MalSequence {
public:
  explicit MalVector(std::vector<MalValuePtr> v) noexcept
      : MalSequence{std::move(v)} {}
  std::string print(bool readably) const override {
    return doPrint(readably, '[', ']');
  }
};

class MalHash : public MalValue {
public:
  explicit MalHash(std::vector<MalValuePtr> v)
      : data{createMap(std::move(v))} {}

  std::string print(bool) const override;

private:
  static std::unordered_map<MalValuePtr, MalValuePtr>
      createMap(std::vector<MalValuePtr>);
  std::unordered_map<MalValuePtr, MalValuePtr> data;
};

class ParserException : public std::runtime_error {
public:
  explicit ParserException(const std::string &str) : std::runtime_error{str} {}
};

#endif // INCLUDE_TYPES_H
