#include "Types.h" // IWYU pragma: associated
#include "Core.h"
#include "FunctionRef.h"
#include "Mal.h"
#include "Ranges.h"

#include <algorithm>
#include <cassert>
#include <format>
#include <ranges>
#include <tuple>
#include <typeinfo>
#include <utility>

namespace {

using mal::EnvPtr;
using mal::EvalFnStack;
using mal::ValuesContainer;
using mal::ValuesSpan;

template <typename VALUES>
auto evalValues(VALUES &&values, EnvPtr evalEnv)  {
  assert(!EvalFnStack::empty());
  auto &evalFn = EvalFnStack::top();
  return std::forward<VALUES>(values) |
         std::views::transform([&](auto &&v) { return evalFn(v, evalEnv); });
}

}

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
  if (auto value = env->find(asKey()))
    return value;
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

std::string String::print(PrintType readably) const {
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

std::string List::print(PrintType readably) const {
  if (readably == MalReadably) {
    return std::format("({:l})", data);
  }
  if (auto fromMacro = [&]() -> std::optional<std::string> {
        if (data.size() == 2) {
          if (auto symbol = to<Symbol>(data[0])) {
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

ValuePtr Vector::eval(EnvPtr env) const {
  assert(env);
  assert(!EvalFnStack::empty());
  auto &evalFn = EvalFnStack::top();
  return make<Vector>(data | std::views::transform([&](auto &&v) {
                        return evalFn(v, env);
                      }));
}

ValuePtr List::eval(EnvPtr env) const {
  assert(env);
    assert(!EvalFnStack::empty());
  if (data.empty()) {
    return shared_from_this();
  }
  auto &evalFn = EvalFnStack::top();
  auto [ast, evalEnv, needsEval] = [&]() {
    auto op = evalFn(data[0], env);
    if (auto invocable = to<Invocable>(op)) {
      return invocable->apply(false, ValuesSpan{data}.subspan(1), env);
    }
    throw EvalException{std::format("invalid function '{:r}'", op)};
  }();

  return needsEval ? EvalFnStack::top()(ast, evalEnv) : ast;
}

std::string Hash::print(PrintType readably) const {
  return readably ? std::format("{{{:r}}}", data) : std::format("{{{}}}", data);
}

ValuePtr Hash::eval(EnvPtr env) const {
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

InvocableResult BuiltIn::apply(bool evaled, ValuesSpan values,
                               EnvPtr evalEnv) const {
  if (evaled) {
    return handler(name, values, std::move(evalEnv));
  }
  return handler(
      name, evalValues(values, evalEnv) | std::ranges::to<ValuesContainer>(),
      std::move(evalEnv));
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
          return static_cast<std::size_t>(i - params.begin());
        }
        return params.size();
      }()},
      params{std::move(params)}, body{std::move(body)},
      capturedEnv{std::move(env)} {
}

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

template <typename VALUES>
EnvPtr FunctionBase::makeApplyEnv(VALUES &&values, EnvPtr evalEnv) const {
  auto applyEnv = make<ApplyEnv>(std::move(evalEnv), capturedEnv);
  if (bindSize == params.size()) {
    checkArgsIs(print(Simply), values.size(), bindSize);
  } else {
    checkArgsAtLeast(print(Simply), values.size(), bindSize);
    applyEnv->insert_or_assign(
        params.back().asKey(),
        make<List>(std::forward<VALUES>(values) | std::views::drop(bindSize)));
  }
  for (auto &&[key, value] :
       std::views::zip(params | std::views::take(bindSize) |
                           std::views::transform([](auto &&param) {
                             return param.asKey();
                           }),
                       std::forward<VALUES>(values))) {
    applyEnv->insert_or_assign(key, std::forward<decltype(value)>(value));
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
  return FunctionBase::template isEqualTo<Lambda>(rhs);
}

InvocableResult Lambda::apply(bool evaled, ValuesSpan values,
                              EnvPtr evalEnv) const {
  if (evaled) {
    return {body,
            makeApplyEnv(values, std::move(evalEnv)), true};
  }
  return {body,
          makeApplyEnv(evalValues(values, evalEnv),
                       std::move(evalEnv)),
          true};
}

std::string Macro::print(PrintType readable) const {
  if (readable) {
    return std::format("#<macro@{:p} {} {:r}>",
                       reinterpret_cast<const void *>(this), params, body);
  }
  return std::format("#<macro@{:p}>", reinterpret_cast<const void *>(this));
}

ValuePtr Macro::isEqualTo(ValuePtr rhs) const {
  return FunctionBase::template isEqualTo<Macro>(rhs);
}

InvocableResult Macro::apply(bool /* evaled */, ValuesSpan values,
                             EnvPtr evalEnv) const {
  assert(!EvalFnStack::empty());
  auto applyEnv = FunctionBase::makeApplyEnv(values, std::move(evalEnv));
  auto res = EvalFnStack::top()(body, applyEnv);
  return {std::move(res), std::move(applyEnv), true};
}

ValuePtr Eval::isEqualTo(ValuePtr rhs) const {
  if (this == rhs.get()) {
    return Constant::trueValue();
  }
  return Constant::falseValue();
}

InvocableResult Eval::apply(bool evaled, ValuesSpan values,
                            EnvPtr evalEnv) const {
  assert(!EvalFnStack::empty());
  checkArgsIs("eval", values, 1);
  return {evaled ? values[0] : EvalFnStack::top()(values[0], evalEnv), env,
          true};
}

} // namespace mal
