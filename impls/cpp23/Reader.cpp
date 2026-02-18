#include "Reader.h"
#include "Ranges.h"
#include "Types.h"
#include <algorithm>
#include <cassert>
#include <ranges>
#include <regex>
#include <string>
#include <utility>

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

static const std::regex tokenRegexes[] = {
  std::regex("~@"),
  std::regex("[\\[\\]{}()'`~^@]"),
  std::regex("\"(?:\\\\.|[^\\\\\"])*\""),
  std::regex("[^\\s\\[\\]{}('\"`,;)]+"),
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
  bool match(const std::regex& regex);

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
  pos += token.size();
  // Skip white spaces and comments
  while (match(std::regex{"[\\s,]+|;.*"})) {
    pos += token.size();
  }

  if (eoi())
    return;

  for (const auto &regex : tokenRegexes) {
    if (match(regex))
      return;
  }

  std::string mismatch{pos, str.end()};
  if (mismatch[0] == '"')
    throw ReaderException{"unbalanced \""};
  throw ReaderException{std::format("mismatch from: {}", std::move(mismatch))};
}

ValuePtr readForm(Tokeniser&);

ValuesContainer readSequence(Tokeniser &tokeniser, const std::string &end) {
  assert(!tokeniser.eoi());
  ValuesContainer items;
  while ([&]() {
    if (tokeniser.eoi())
      throw ReaderException{"unbalanced " + end};
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
    return make<String>(String::unescape(std::move(token)));
  }

  if (token[0] == ':') {
    return make<Keyword>(token);
  }

  if (token == "^") {
    auto meta{readForm(tokeniser)};
    return make<List>(ValuesContainer{
      make<Symbol>("with-meta"),
      readForm(tokeniser),
      std::move(meta),
    });
  }

  if (std::regex_match(token, std::regex{"^[-+]?\\d+$"})) {
    return make<Integer>(std::stol(std::move(token)));
  }

  auto firstIsToken = [&](auto &&elt) noexcept { return elt.first == token; };
  static const std::pair<std::string, ValuePtr> constants[] = {
    {"nil", Constant::nilValue()},
    {"true", Constant::trueValue()},
    {"false", Constant::falseValue()},
  };
  if (auto constant = std::find_if(std::begin(constants), std::end(constants),
                                   firstIsToken);
      constant != std::end(constants)) {
    return constant->second;
  }

  static const std::pair<std::string, std::string> macros[] = {
    {"@", "deref"},
    {"`", "quasiquote"},
    {"'", "quote"},
    {"~@", "splice-unquote"},
    {"~", "unquote"},
  };
  if (auto macro = std::find_if(std::begin(macros), std::end(macros),
                                firstIsToken);
      macro != std::end(macros)) {
    return make<List>(ValuesContainer{
        make<Symbol>(macro->second),
        readForm(tokeniser),
    });
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
      if (!(to<String>(key) ||
            to<Symbol>(key) ||
            to<Keyword>(key))) {
        throw ReaderException{std::format("unexpected key '{:r}'", key)};
      }
    }
    return make<Hash>(std::move(items));
  }

  return readAtom(tokeniser);
}

} // namespace

namespace mal {

ValuePtr readStr(std::string str) {
  Tokeniser tokeniser(str);
  if (tokeniser.eoi()) {
    throw ReaderException{"EOI"};
  }
  return readForm(tokeniser);
}

} // namespace mal
