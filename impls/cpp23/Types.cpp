#include "Types.h"
#include "Core.h"
#include <algorithm>
#include <cassert>
#include <format>
#include <utility>
#include <ranges>


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
  if (auto other = [&]() noexcept -> StringBase * {
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

ValuePtr Symbol::eval(EnvPtr env) {
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

std::string String::unescape(std::string in) {
  std::string out;
  out.reserve(in.size());
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

std::string String::escape(std::string in) {
  std::string out{'"'};
  out.reserve(in.size() + 2);
  for (auto i = in.begin(); i != in.end(); ++i) {
    char c = *i;
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

ValuePtr Vector::eval(EnvPtr env) {
  assert(env);
  auto evaled = data |
                std::views::transform([&](auto &&v) { return EVAL(v, env); }) |
                std::ranges::to<ValuesContainer>();
  return make<Vector>(std::move(evaled));
}

ValuePtr List::eval(EnvPtr env) {
  assert(env);
  if (data.empty())
    return shared_from_this();
  auto evaled = data |
                std::views::transform([&](auto &&v) { return EVAL(v, env); }) |
                std::ranges::to<ValuesContainer>();
  auto op = evaled[0];
  if (auto function = to<Invocable>(op)) {
    return function->apply({evaled.begin() + 1, evaled.end()});
  }
  throw EvalException{std::format("invalid function '{:r}'", op)};
}

std::string Hash::print(bool readably) const {
  return readably ? std::format("{{{:r}}}", data) : std::format("{{{}}}", data);
}

ValuePtr Hash::eval(EnvPtr env) {
  assert(env);
  auto evaled = data | std::views::transform([&](auto &&v) {
                  return std::pair{v.first, EVAL(v.second, env)};
                }) |
                std::ranges::to<ValuesMap>();
  auto res = make<Hash>(ValuesContainer{});
  res->data = std::move(evaled);
  return res;
}

ValuePtr Hash::isEqualTo(ValuePtr rhs) const {
  if (this == rhs.get()) {
    return Constant::trueValue();
  }
  if (auto other = [&]() noexcept -> Hash * {
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
  for (auto &&i = v.begin(); i != v.end(); i += 2) {
    assert(to<StringBase>(*i));
    res.emplace(std::move(*i), std::move(*(i + 1)));
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
  if (auto other = to<Lambda>(rhs);
      other && params.size() == other->params.size() &&
      env.get() == other->env.get()) {
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

ValuePtr Lambda::apply(ValuesSpan values) const {
  auto applyEnv = make<Env>(env);
  auto bindSize = params.size();
  if (auto i = std::find_if(params.begin(), params.end(),
                            [](auto &&elt) { return elt[0] == '&'; });
      i != params.end()) {
    if ((params.end() - i) != 2) {
      throw EvalException{"there must be exactly one parameter after the &"};
    }
    bindSize = i - params.begin();
    checkArgsAtLeast(print(false), values, bindSize);
    applyEnv->insert_or_assign(params.back(),
                               make<List>(ValuesContainer(
                                   values.begin() + bindSize, values.end())));
  } else {
    checkArgsIs(print(false), values, params.size());
  }
  for (auto &&[key, value] :
       std::views::zip(params | std::views::take(bindSize),
                       values)) {
    applyEnv->insert_or_assign(key, value);
  }
  return EVAL(body, applyEnv);
}

} // namespace mal
