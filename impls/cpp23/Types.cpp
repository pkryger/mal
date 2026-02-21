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

ValuePtr Env::find(const std::string &key) const {
  for (auto env = this; env; env = [&]() noexcept -> const Env * {
         if (env->outer)
           return env->outer.get();
         return nullptr;
       }()) {
    if (auto item = env->map.find(key); item != env->map.end()) {
      return item->second;
    }
  }
  return nullptr;
}

bool Value::isTrue() const noexcept {
  return !(this == Constant::nilValue().get() ||
           this == Constant::falseValue().get());
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
  if (auto &&i = env->find(data))
    return i;
  throw EvalException{std::format("'{}' not found", data)};
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
  return readably ? escape(data) : data;
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
  auto evaled =
      data | std::views::transform([&](auto &&v) { return EVAL(v, env); });
  auto op = evaled[0];
  if (auto function = to<Invocable>(op)) {
    auto args =
        evaled | std::views::drop(1) | std::ranges::to<ValuesContainer>();
    return function->apply({args.begin(), args.end()}, env);
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
  auto evaled = data | std::views::transform([&](auto &&v) {
                  return std::pair{v.first, EVAL(v.second, env)};
                }) |
                std::ranges::to<ValuesMap>();
  auto res = make<Hash>();
  res->data = std::move(evaled);
  return res;
}

ValuePtr Hash::isEqualTo(ValuePtr rhs) const {
  if (this == rhs.get()) {
    return Constant::trueValue();
  }
  if (auto other = [&]() noexcept -> const Hash * {
        auto&& o = *rhs; // suppress -Wpotentially-evaluated-expression
        if (typeid(*this) == typeid(o)) {
          return to<Hash>(rhs);
        }
        return nullptr;
      }();
      other && data.size() == other->data.size()) {
    auto res =
        std::ranges::mismatch(data, other->data, [](auto &&lhs, auto &&rhs) {
          return lhs.first->isEqualTo(rhs.first)->isTrue() &&
                 lhs.second->isEqualTo(rhs.second)->isTrue();
        });
    return res.in1 == data.end() && res.in2 == other->data.end()
               ? Constant::trueValue()
               : Constant::falseValue();
  }
  return Constant::falseValue();
}

ValuesMap Hash::createMap(ValuesContainer v) {
  ValuesMap res;
  res.reserve(v.size() / 2);
  for (auto &&[key, value] :
       v | std::views::chunk(2) | std::views::transform([](auto &&elt) {
         return std::tie(elt[0], elt[1]);
       })) {
    assert(to<StringBase>(key));
    res.emplace(std::move(key), std::move(value));
  }
  return res;
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

Lambda::Lambda(std::vector<std::string> params, ValuePtr body, EnvPtr env)
    : bindSize{[&]() -> std::size_t {
        if (auto i = std::ranges::find_if(
                params, [](auto &&elt) { return elt[0] == '&'; });
            i != params.end()) {
          if ((params.end() - i) != 2) {
            throw EvalException{
                "there must be exactly one parameter after the &"};
          }
          return i - params.begin();
        }
        return params.size();
      }()},
      params{std::move(params)}, body{std::move(body)}, env{std::move(env)} {}

std::string Lambda::print(bool readable) const {
  if (readable) {
    return std::format("#(λ-{:p} ({}) {:r}", reinterpret_cast<const void *>(this),
                       params, body);

  }
  return std::format("(λ-{:p})", reinterpret_cast<const void *>(this));
}

ValuePtr Lambda::isEqualTo(ValuePtr rhs) const {
  if (this == rhs.get()) {
    return Constant::trueValue();
  }
  if (auto other = to<Lambda>(rhs); other && bindSize == other->bindSize &&
                                    params.size() == other->params.size()) {
    auto res =
        std::ranges::mismatch(params, other->params, [](auto &&lhs, auto &&rhs) noexcept {
          return lhs == rhs;
        });
    if (res.in1 != params.end() || res.in2 != other->params.end()) {
      return Constant::falseValue();
    }
    return body->isEqualTo(other->body);
  }
  return Constant::falseValue();
}

InvocableResult Lambda::apply(ValuesSpan values, EnvPtr /* evalEnv */) const {
  auto applyEnv = make<Env>(env);
  if (bindSize == params.size()) {
    checkArgsIs(print(false), values, bindSize);
  } else {
    checkArgsAtLeast(print(false), values, bindSize);
    applyEnv->insert_or_assign(params.back(),
                               make<List>(values | std::views::drop(bindSize)));
  }
  for (auto &&[key, value] :
       std::views::zip(params | std::views::take(bindSize),
                       values)) {
    applyEnv->insert_or_assign(key, value);
  }
  return {body, applyEnv, true};
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
