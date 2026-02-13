#include "Types.h"
#include "Core.h"
#include <algorithm>
#include <cassert>
#include <format>
#include <utility>
#include <ranges>

extern MalValuePtr EVAL(MalValuePtr, MalEnvPtr);

MalValuePtr MalEnv::find(const std::string &key) const {
  for (auto env = this; env; env = [&]() noexcept -> const MalEnv * {
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

bool MalValue::isTrue() const noexcept {
  return !(this == MalConstant::nilValue().get() ||
           this == MalConstant::falseValue().get());
}

MalValuePtr MalInteger::isEqualTo(MalValuePtr rhs) const {
  if (this == rhs.get()) {
    return MalConstant::trueValue();
  }
  if (auto other = dynamic_cast<MalInteger *>(rhs.get());
      other && data == other->data) {
    return MalConstant::trueValue();
  }
  return MalConstant::falseValue();
}

MalValuePtr MalStringBase::isEqualTo(MalValuePtr rhs) const {
  if (this == rhs.get()) {
    return MalConstant::trueValue();
  }
  if (auto other = [&]() noexcept -> MalStringBase * {
        auto&& o = *rhs; // suppress -Wpotentially-evaluated-expression
        if (typeid(*this) == typeid(o)) {
          return dynamic_cast<MalStringBase *>(rhs.get());
        }
        return nullptr;
      }();
      other && data == other->data) {
    return MalConstant::trueValue();
  }
  return MalConstant::falseValue();
}

MalValuePtr MalSymbol::eval(MalEnvPtr env) {
  assert(env);
  if (auto &&i = env->find(data))
    return i;
  throw EvalException{std::format("'{}' not found", data)};
}

MalValuePtr MalConstant::isEqualTo(MalValuePtr rhs) const {
  if (this == rhs.get()) {
    return MalConstant::trueValue();
  }
  return MalConstant::falseValue();
}

std::string MalString::unescape(std::string in) {
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

std::string MalString::escape(std::string in) {
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

std::string MalString::print(bool readably) const {
  return readably ? escape(data) : data;
}

MalValuePtr MalSequence::isEqualTo(MalValuePtr rhs) const {
  if (this == rhs.get()) {
    return MalConstant::trueValue();
  }
  if (auto other = dynamic_cast<MalSequence *>(rhs.get());
      other && data.size() == other->data.size()) {
    auto res =
        std::ranges::mismatch(data, other->data, [](auto &&lhs, auto &&rhs) {
          return lhs->isEqualTo(rhs)->isTrue();
        });
    return res.in1 == data.end() && res.in2 == other->data.end()
               ? MalConstant::trueValue()
               : MalConstant::falseValue();
  }
  return MalConstant::falseValue();
}

MalValuePtr MalVector::eval(MalEnvPtr env) {
  assert(env);
  auto evaled = data |
                std::views::transform([&](auto &&v) { return EVAL(v, env); }) |
                std::ranges::to<MalValueVec>();
  return std::make_shared<MalVector>(std::move(evaled));
}

MalValuePtr MalList::eval(MalEnvPtr env) {
  assert(env);
  if (data.empty())
    return shared_from_this();
  auto evaled = data |
                std::views::transform([&](auto &&v) { return EVAL(v, env); }) |
                std::ranges::to<MalValueVec>();
  auto op = evaled[0];
  if (auto function = dynamic_cast<MalFunction *>(op.get())) {
    return function->apply({evaled.begin() + 1, evaled.end()});
  }
  throw EvalException{std::format("invalid function '{:r}'", op)};
}

std::string MalHash::print(bool readably) const {
  return readably ? std::format("{{{:r}}}", data) : std::format("{{{}}}", data);
}

MalValuePtr MalHash::eval(MalEnvPtr env) {
  assert(env);
  auto evaled = data | std::views::transform([&](auto &&v) {
                  return std::pair{v.first, EVAL(v.second, env)};
                }) |
                std::ranges::to<MalValueMap>();
  auto res = std::make_shared<MalHash>(MalValueVec{});
  res->data = std::move(evaled);
  return res;
}

MalValuePtr MalHash::isEqualTo(MalValuePtr rhs) const {
  if (this == rhs.get()) {
    return MalConstant::trueValue();
  }
  if (auto other = [&]() noexcept -> MalHash * {
        auto&& o = *rhs; // suppress -Wpotentially-evaluated-expression
        if (typeid(*this) == typeid(o)) {
          return dynamic_cast<MalHash *>(rhs.get());
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
               ? MalConstant::trueValue()
               : MalConstant::falseValue();
  }
  return MalConstant::falseValue();
}

MalValueMap MalHash::createMap(MalValueVec v) {
  MalValueMap res;
  res.reserve(v.size() / 2);
  for (auto &&i = v.begin(); i != v.end(); i += 2) {
    assert(dynamic_cast<MalStringBase *>(i->get()));
    res.emplace(std::move(*i), std::move(*(i + 1)));
  }
  return res;
}

MalValuePtr MalBuiltIn::isEqualTo(MalValuePtr rhs) const {
  if (this == rhs.get()) {
    return MalConstant::trueValue();
  }
  if (auto other = dynamic_cast<MalBuiltIn *>(rhs.get());
      other && handler == other->handler) {
    return MalConstant::trueValue();
  }
  return MalConstant::falseValue();
}

std::string MalLambda::print(bool readable) const {
  if (readable) {
    return std::format("#(λ-{:p} ({}) {:r}", reinterpret_cast<const void *>(this),
                       params, body);

  }
  return std::format("(λ-{:p})", reinterpret_cast<const void *>(this));
}

MalValuePtr MalLambda::isEqualTo(MalValuePtr rhs) const {
  if (this == rhs.get()) {
    return MalConstant::trueValue();
  }
  if (auto other = dynamic_cast<MalLambda *>(rhs.get());
      other && params.size() == other->params.size() &&
      env.get() == other->env.get()) {
    auto res =
        std::ranges::mismatch(params, other->params, [](auto &&lhs, auto &&rhs) noexcept {
          return lhs == rhs;
        });
    if (res.in1 != params.end() || res.in2 != other->params.end()) {
      return MalConstant::falseValue();
    }
    return body->isEqualTo(other->body);
  }
  return MalConstant::falseValue();
}

MalValuePtr MalLambda::apply(MalValues values) const {
  auto applyEnv = std::make_shared<MalEnv>(env);
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
                               std::make_shared<MalList>(MalValueVec(
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
