#include "Types.h"
#include <cassert>
#include <utility>

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
  // std::vector<MalValuePtr> it should be something like:
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

std::string MalHash::print(bool readably) const {
  return collectionPrint('{', '}', data, [&](auto &&v) {
    return v.first->print(readably) + " " + v.second->print(readably);
  });
}

std::unordered_map<MalValuePtr, MalValuePtr>
MalHash::createMap(std::vector<MalValuePtr> v) {
  std::unordered_map<MalValuePtr, MalValuePtr> res;
  res.reserve(v.size() / 2);
  for (auto &&i = v.begin(); i != v.end(); i += 2) {
    assert(dynamic_cast<MalStringBase *>(i->get()));
    res.emplace(std::move(*i), std::move(*(i + 1)));
  }
  return res;
}
