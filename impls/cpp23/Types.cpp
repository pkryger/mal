#include "Types.h"
#include <cassert>
#include <utility>
#include <ranges>

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

MalValuePtr MalSymbol::eval(MalEnvPtr env) {
  assert(env);
  if (auto &&i = env->find(data))
    return i;
  throw EvalException{"'" + data + "' not found"};
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
  out.reserve(in.size());
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
  std::string out{MalStringBase::print(readably)};
  return readably ? escape(out) : out;
}

namespace {
  // No support for std::views::join_with on Apple Clang 17.0, but for a
  // MalValueVec it should be something like:
  //
  // return std::format("({})", data | std::views::transform(&MalValue::print)
  //   | std::views::join_with(' ') | std::ranges::to<std::string>());


template <typename DATA, typename FORMAT_DATUM>
std::string collectionPrint(char open, char close, DATA&& data,
                            FORMAT_DATUM &&format_datum) {
  std::string res{open};
  if (data.empty())
    return res + close;
  auto i = data.begin();
  res += format_datum(*i);
  for (++i; i != data.end(); ++i)
    res += " " + format_datum(*i);
  res += close;
  return res;
}

} // namespace

std::string MalSequence::doPrint(bool readably, char open, char close) const {
  return collectionPrint(open, close, data,
                         [&](auto &&v) { return v->print(readably); });
}


MalValuePtr MalVector::eval(MalEnvPtr env) {
  assert(env);
  auto evaled = data |
                std::views::transform([&](auto &&v) { return v->eval(env); }) |
                std::ranges::to<MalValueVec>();
  return std::make_shared<MalVector>(std::move(evaled));
}

MalValuePtr MalList::eval(MalEnvPtr env) {
  assert(env);
  if (data.empty())
    return shared_from_this();
  auto evaled = data |
                std::views::transform([&](auto &&v) { return v->eval(env); }) |
                std::ranges::to<MalValueVec>();
  auto op = evaled.at(0);
  if (auto func = dynamic_cast<MalFunction *>(op.get())) {
    return func->apply(evaled.begin() + 1, evaled.end());
  }
  throw EvalException{"invalid function " + op->print(true)};
}

std::string MalHash::print(bool readably) const {
  return collectionPrint('{', '}', data, [&](auto &&v) {
    return v.first->print(readably) + " " + v.second->print(readably);
  });
}

MalValuePtr MalHash::eval(MalEnvPtr env) {
  assert(env);
  auto evaled = data | std::views::transform([&](auto &&v) {
                  return std::pair{v.first, v.second->eval(env)};
                }) |
                std::ranges::to<std::unordered_map>();
  auto res = std::make_shared<MalHash>(MalValueVec{});
  res->data = std::move(evaled);
  return res;
}

std::unordered_map<MalValuePtr, MalValuePtr>
MalHash::createMap(MalValueVec v) {
  std::unordered_map<MalValuePtr, MalValuePtr> res;
  res.reserve(v.size() / 2);
  for (auto &&i = v.begin(); i != v.end(); i += 2) {
    assert(dynamic_cast<MalStringBase *>(i->get()));
    res.emplace(std::move(*i), std::move(*(i + 1)));
  }
  return res;
}
