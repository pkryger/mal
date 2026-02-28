#include "Types.h" // IWYU pragma: associated
#include "Core.h"
#include "Ranges.h"

#include <algorithm>
#include <cassert>
#include <format>
#include <ranges>
#include <tuple>
#include <typeinfo>
#include <utility>

namespace mal {

extern ValuePtr EVAL(ValuePtr, EnvPtr);

ValuePtr EnvBase::find(KeyView key) const {
  auto phk = PreHashedKey{key, Hash{}(key)};
  for (auto &&env : *this) {
    if (auto value = env.findLocal(phk)) {
      return value;
    }
  }
  return nullptr;
}

ValuePtr Env::findLocal(PreHashedKey phk) const {
  if (auto item = map->find(phk); item != map->end()) {
    return item->second;
  }
  return nullptr;
}

CapturedEnv::CapturedEnv(EnvCPtr captureEnv)
    : EnvBase{nullptr},
      maps{*captureEnv | std::views::transform([](auto &&envBase) {
        if (auto env = dynamic_cast<const Env *>(std::addressof(envBase))) {
          return MapsVec{env->mapCPtr()};
        }
        if (auto capturedEnv =
                dynamic_cast<const CapturedEnv *>(std::addressof(envBase))) {
          return capturedEnv->maps;
        }
        throw EvalException{"invalid env"};
      }) | std::views::join |
           std::ranges::to<MapsVec>()} {}

ValuePtr CapturedEnv::findLocal(PreHashedKey phk) const {
  for (auto &&map : maps) {
    if (auto item = map->find(phk); item != map->end()) {
      return item->second;
    }
  }
  return nullptr;
}

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
  if (auto other = to<Integer>(rhs);
      other && data == other->data) {
    return Constant::trueValue();
  }
  return Constant::falseValue();
}

ValuePtr StringBase::isEqualTo(ValuePtr rhs) const {
  if (this == rhs.get()) {
    return Constant::trueValue();
  }
  if (auto other = [&]() noexcept -> const StringBase * {
        auto&& o = *rhs; // suppress -Wpotentially-evaluated-expression
        if (typeid(*this) == typeid(o)) {
          return to<StringBase>(rhs);
        }
        return nullptr;
      }();
      other && data == other->data) {
    return Constant::trueValue();
  }
  return Constant::falseValue();
}

ValuePtr Symbol::eval(EnvPtr env) const {
  assert(env);
  if (auto &&i = env->find(asKey()))
    return i;
  throw EvalException{std::format("'{}' not found", data)};
}

ValuePtr Symbol::isEqualTo(ValuePtr rhs) const {
  if (this == rhs.get()) {
    return Constant::trueValue();
  }
  if (auto other = to<Symbol>(rhs); other && asKey() == other->asKey()) {
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

std::string String::escape(const std::string& in) {
  std::string out{'"'};
  out.reserve(in.size() + 2);
  for (auto&& c : in) {
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
  out += '"';
  return out;
}

std::string String::print(bool readably) const {
  return readably ? escape(StringBase::data) : StringBase::data;
}

ValuePtr Atom::isEqualTo(ValuePtr rhs) const {
  if (this == rhs.get()) {
    return Constant::trueValue();
  }
  if (auto o = to<Atom>(rhs)) {
    return data->isEqualTo(o->data);
  }
  return Constant::falseValue();
}

ValuePtr Sequence::isEqualTo(ValuePtr rhs) const {
  if (this == rhs.get()) {
    return Constant::trueValue();
  }
  if (auto other = to<Sequence>(rhs);
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

std::string List::print(bool readably) const {
  if (auto macro = [&]() -> std::optional<std::string> {
        if (data.size() == 2) {
          if (auto symbol = to<Symbol>(data[0])) {
            return symbol->macro;
          }
        }
        return {};
      }()) {
    return readably ? std::format("{}{:r}", *macro, data[1])
                    : std::format("{}{}", *macro, data[1]);

  }
  return readably ? std::format("({:r})", data) : std::format("({})", data);
}

ValuePtr Vector::eval(EnvPtr env) const {
  assert(env);
  return make<Vector>(
      data | std::views::transform([&](auto &&v) { return EVAL(v, env); }));
}

InvocableResult List::invoke(EnvPtr env) const {
  assert(env);
  assert(!data.empty());
  auto op = EVAL(data[0], env);
  if (auto function = to<Macro>(op)) {
    return function->apply(ValuesSpan{data}.subspan(1), env);
  }
  if (auto function = to<Invocable>(op)) {
      auto args = data | std::views::drop(1) |
               std::views::transform([&](auto &&v) { return EVAL(v, env); }) |
        std::ranges::to<ValuesContainer>();
      return function->apply({args}, env);
  }
  throw EvalException{std::format("invalid function '{:r}'", op)};
}

ValuePtr List::eval(EnvPtr env) const {
  assert(env);
  if (data.empty()) {
    return shared_from_this();
  }
  auto [ast, evalEnv, needsEval] = invoke(env);
  return needsEval ? EVAL(ast, evalEnv) : ast;
}

std::string Hash::print(bool readably) const {
  return readably ? std::format("{{{:r}}}", data) : std::format("{{{}}}", data);
}

ValuePtr Hash::eval(EnvPtr env) const {
  assert(env);
  return make<Hash>(data | std::views::transform([&](auto &&elt) {
                      return std::pair{elt.first, EVAL(elt.second, env)};
                    }));
}

ValuePtr Hash::isEqualTo(ValuePtr rhs) const {
  if (this == rhs.get()) {
    return Constant::trueValue();
  }
  if (auto other = to<Hash>(rhs)) {
    return data == other->data ? Constant::trueValue() : Constant::falseValue();
  }
  return Constant::falseValue();
}

Hash::Hash(const Hash &other, ValuesSpan values) : data{other.data} {
  for (auto [key, value] :
       values | std::views::chunk(2) | std::views::transform([](auto &&chunk) {
         assert(to<StringBase>(chunk[0]));
         return std::tie(chunk[0], chunk[1]);
       })) {
    data.insert_or_assign(key, value);
  }}

ValuePtr Hash::find(ValuePtr key) const {
  if (auto res = data.find(key); res != data.end()) {
    return res->second;
  }
  return nullptr;
}

ValuePtr BuiltIn::isEqualTo(ValuePtr rhs) const {
  if (this == rhs.get()) {
    return Constant::trueValue();
  }
  if (auto other = to<BuiltIn>(rhs);
      other && handler == other->handler) {
    return Constant::trueValue();
  }
  return Constant::falseValue();
}

FunctionBase::FunctionBase(Params params, ValuePtr body, EnvPtr env)
    : bindSize{[&]() -> std::size_t {
        if (auto i = std::ranges::find_if(
                params, [](auto &&elt) { return elt.name()[0] == '&'; });
            i != params.end()) {
          if ((params.end() - i) != 2) {
            throw EvalException{
                "there must be exactly one parameter after the &"};
          }
          return i - params.begin();
        }
        return params.size();
      }()},
      params{std::move(params)}, body{std::move(body)},
      capturedEnv{std::move(env)} {}

template <typename TYPE>
ValuePtr FunctionBase::isEqualTo(ValuePtr rhs) const {
  if (this == rhs.get()) {
    return Constant::trueValue();
  }
  if (auto other = to<TYPE>(rhs); other && bindSize == other->bindSize &&
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

EnvPtr FunctionBase::makeApplyEnv(ValuesSpan values, EnvPtr evalEnv) const {
  auto applyEnv = make<Env>(make<CapturedEnv>(capturedEnv, std::move(evalEnv)));
  if (bindSize == params.size()) {
    checkArgsIs(print(false), values, bindSize);
  } else {
    checkArgsAtLeast(print(false), values, bindSize);
    applyEnv->insert_or_assign(params.back().asKey(),
                               make<List>(values | std::views::drop(bindSize)));
  }
  for (auto &&[key, value] :
       std::views::zip(params | std::views::take(bindSize) |
                           std::views::transform([](auto &&param) {
                             return param.asKey();
                           }),
                       values)) {
    applyEnv->insert_or_assign(key, value);
  }
  return applyEnv;
}

std::string Lambda::print(bool readable) const {
  if (readable) {
    return std::format("#<λ@{:p} {} {:r}>",
                       reinterpret_cast<const void *>(this), params, body);

  }
  return std::format("#<λ@{:p}>", reinterpret_cast<const void *>(this));
}

ValuePtr Lambda::isEqualTo(ValuePtr rhs) const {
  return FunctionBase::template isEqualTo<Lambda>(rhs);
}

InvocableResult Lambda::apply(ValuesSpan values, EnvPtr evalEnv) const {
  return {body, makeApplyEnv(values, std::move(evalEnv)), true};
}

std::string Macro::print(bool readable) const {
  if (readable) {
    return std::format("#<macro@{:p} {} {:r}>",
                       reinterpret_cast<const void *>(this), params, body);
  }
  return std::format("#<macro@{:p}>", reinterpret_cast<const void *>(this));
}

ValuePtr Macro::isEqualTo(ValuePtr rhs) const {
  return FunctionBase::template isEqualTo<Macro>(rhs);
}

InvocableResult Macro::apply(ValuesSpan values, EnvPtr evalEnv) const {
  auto applyEnv =  FunctionBase::makeApplyEnv(values, std::move(evalEnv));
  return {EVAL(body, applyEnv), applyEnv, true};
}

ValuePtr Eval::isEqualTo(ValuePtr rhs) const {
  if (this == rhs.get()) {
    return Constant::trueValue();
  }
  return Constant::falseValue();
}

InvocableResult Eval::apply(ValuesSpan values, EnvPtr evalEnv) const {
  checkArgsIs("eval", values, 1);
  return{values[0], env, true};
}

} // namespace mal
