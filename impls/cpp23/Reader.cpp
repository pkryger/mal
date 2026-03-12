#include "Reader.h" // IWYU pragma: associated
#include "Mal.h"
#if !defined(__cpp_lib_ranges_stride)
#include "Ranges.h"
#endif // __cpp_lib_ranges_stride
#include "Types.h"

#include <algorithm>
#include <array>
#include <cassert>
#include <format>
#include <limits>
#if defined(__cpp_lib_ranges_stride)
#include <ranges>
#endif // __cpp_lib_ranges_stride
#include <regex>
#include <string>
#include <utility>
// IWYU pragma: no_include <ranges>
// IWYU pragma: no_include <memory>

namespace {
using mal::Constant;
using mal::Hash;
using mal::Integer;
using mal::Keyword;
using mal::List;
using mal::make;
using mal::ReaderException;
using mal::String;
using mal::Symbol;
using mal::ValuePtr;
using mal::ValuesContainer;
using mal::Vector;

static const std::array tokenRegexes = {
  std::regex("~@"),
  std::regex("[\\[\\]{}()'`~^@]"),
  std::regex("\"(?:\\\\.|[^\\\\\"])*\""),
  std::regex("[^\\s\\[\\]{}('\"`,;)]+"),
};

static const std::array constants = {
  std::pair{"nil", Constant::nilValue()},
  std::pair{"true", Constant::trueValue()},
  std::pair{"false", Constant::falseValue()},
};

static const std::array macros = {
  std::pair{"@", "deref"},
  std::pair{"`", "quasiquote"},
  std::pair{"'", "quote"},
  std::pair{"~@", "splice-unquote"},
  std::pair{"~", "unquote"},
};

class Tokeniser {
public:
  explicit Tokeniser(const std::string &s) : str{s}, pos{s.begin()} {
    nextToken();
  }

  bool eoi() const noexcept { return pos == str.end(); }
  void nextToken();
  const std::string &peek() const noexcept {
    assert(!eoi());
    return token;
  }

private:
  bool match(const std::regex &regex);

  auto tokenSize() const {
    auto size = token.size();
    if (size > static_cast<decltype(size)>(
                   std::numeric_limits<std::string::difference_type>::max())) {
      throw ReaderException(
          std::format("token at pos {} too big {}", pos - str.begin(), size));
    }
    return static_cast<std::string::difference_type>(size);
  }

  const std::string &str;
  std::string::const_iterator pos;
  std::string token;
};

bool Tokeniser::match(const std::regex &regex) {
  if (eoi())
    return false;

  std::smatch match;
  if (!std::regex_search(pos, str.end(), match, regex,
                         std::regex_constants::match_continuous))
    return false;

  assert(match.size() == 1);
  assert(match.position(0) == 0);
  assert(match.length(0) > 0);
  token = match.str(0);
  return true;
}

void Tokeniser::nextToken() {
  pos += tokenSize();
  // Skip white spaces and comments
  while (match(std::regex{"[\\s,]+|;.*"})) {
    pos += tokenSize();
  }

  if (eoi()) {
    return;
  }

  for (const auto &regex : tokenRegexes) {
    if (match(regex)) {
      return;
    }
  }

  std::string mismatch{pos, str.end()};
  if (mismatch[0] == '"') {
    throw ReaderException{"unbalanced \""};
  }
  throw ReaderException{std::format("mismatch from: {}", std::move(mismatch))};
}

ValuePtr readForm(Tokeniser & /* tokeniser */);

ValuesContainer readSequence(Tokeniser &tokeniser, const std::string &end) {
  assert(!tokeniser.eoi());
  ValuesContainer items;
  while ([&]() {
    if (tokeniser.eoi()) {
      throw ReaderException{"unbalanced " + end};
    }
    return true; }()
    && tokeniser.peek() != end) {
    items.emplace_back(readForm(tokeniser));
  }
  tokeniser.nextToken();
  return items;
}

ValuePtr readAtom(Tokeniser &tokeniser) {
  assert(!tokeniser.eoi());
  auto token = tokeniser.peek();
  tokeniser.nextToken();

  if (token[0] == '"') {
    return make<String>(String::unescape(token));
  }

  if (token[0] == ':') {
    return make<Keyword>(token);
  }

  if (token == "^") {
    auto meta{readForm(tokeniser)};
    return make<List>(make<Symbol>("with-meta"), readForm(tokeniser),
                      std::move(meta));
  }

  if (std::regex_match(token, std::regex{"^[-+]?\\d+$"})) {
    return make<Integer>(std::stol(token));
  }

  auto first = [](auto &&elt) noexcept { return elt.first; };
  auto isToken = [&](auto &&elt) noexcept { return elt == token; };
  if (auto constant = std::ranges::find_if(constants, isToken, first);
      constant != constants.end()) {
    return constant->second;
  }

  if (auto macro = std::ranges::find_if(macros, isToken, first);
      macro != macros.end()) {
    return make<List>(make<Symbol>(macro->second, macro->first),
                      readForm(tokeniser));
  }

  return make<Symbol>(std::move(token));
}

ValuePtr readForm(Tokeniser &tokeniser) {
  assert(!tokeniser.eoi());
  auto &&token = tokeniser.peek();
  if (token == "(") {
    tokeniser.nextToken();
    return make<List>(readSequence(tokeniser, ")"));
  }
  if (token == "[") {
    tokeniser.nextToken();
    return make<Vector>(readSequence(tokeniser, "]"));
  }
  if (token == "{") {
    tokeniser.nextToken();
    auto items = readSequence(tokeniser, "}");
    if (items.size() % 2 != 0) {
      throw ReaderException{"odd number of items: " +
                                 std::to_string(items.size())};
    }
    for (auto key : items | std::views::stride(2)) {
      if (!(key->isa<String>() ||
            key->isa<Symbol>() ||
            key->isa<Keyword>())) {
        throw ReaderException{std::format("unexpected key '{:r}'", key)};
      }
    }
    return make<Hash>(std::move(items));
  }

  return readAtom(tokeniser);
}

} // namespace

namespace mal {

ValuePtr readStr(const std::string &str) {
  Tokeniser tokeniser(str);
  if (tokeniser.eoi()) {
    throw ReaderException{"EOI"};
  }
  return readForm(tokeniser);
}

} // namespace mal
