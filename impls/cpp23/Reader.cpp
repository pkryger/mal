#include "Reader.h"
#include "Types.h"
#include <algorithm>
#include <cassert>
#include <memory>
#include <regex>
#include <string>
#include <utility>

namespace {

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

MalValuePtr readForm(Tokeniser&);

MalValueVec readSequence(Tokeniser &tokeniser, const std::string &end) {
  assert(!tokeniser.eoi());
  MalValueVec items;
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

MalValuePtr readAtom(Tokeniser &tokeniser) {
  assert(!tokeniser.eoi());
  auto token = tokeniser.peek();
  tokeniser.nextToken();

  if (token[0] == '"') {
    return std::make_shared<MalString>(MalString::unescape(std::move(token)));
  }

  if (token[0] == ':') {
    return std::make_shared<MalKeyword>(token);
  }

  if (token == "^") {
    auto meta{readForm(tokeniser)};
    return std::make_shared<MalList>(MalValueVec{
      std::make_shared<MalSymbol>("with-meta"),
      readForm(tokeniser),
      std::move(meta),
    });
  }

  if (std::regex_match(token, std::regex{"^[-+]?\\d+$"})) {
    return std::make_shared<MalInteger>(std::stol(std::move(token)));
  }

  auto firstIsToken = [&](auto &&elt) noexcept { return elt.first == token; };
  static const std::pair<std::string, MalValuePtr> constants[] = {
    {"nil", MalConstant::nilValue()},
    {"true", MalConstant::trueValue()},
    {"false", MalConstant::falseValue()},
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
    return std::make_shared<MalList>(MalValueVec{
        std::make_shared<MalSymbol>(macro->second),
        readForm(tokeniser),
    });
  }

  return std::make_shared<MalSymbol>(std::move(token));
}

MalValuePtr readForm(Tokeniser &tokeniser) {
  assert(!tokeniser.eoi());
  auto &&token = tokeniser.peek();
  if (token == "(") {
    tokeniser.nextToken();
    return std::make_shared<MalList>(readSequence(tokeniser, ")"));
  }
  if (token == "[") {
    tokeniser.nextToken();
    return std::make_shared<MalVector>(readSequence(tokeniser, "]"));
  }
  if (token == "{") {
    tokeniser.nextToken();
    auto items = readSequence(tokeniser, "}");
    if (items.size() % 2 != 0) {
      throw ReaderException{"odd number of items: " +
                            std::to_string(items.size())};
    }
    for (auto i = items.begin(); i != items.end(); i+=2) {
      if (!(dynamic_cast<MalString *>(i->get()) ||
            dynamic_cast<MalSymbol *>(i->get()) ||
            dynamic_cast<MalKeyword *>(i->get()))) {
        throw ReaderException{std::format("unexpected key '{:r}'", *i)};
    }}
    return std::make_shared<MalHash>(std::move(items));
  }

  return readAtom(tokeniser);
}

} // namespace

MalValuePtr readStr(std::string str) {
  Tokeniser tokeniser(str);
  if (tokeniser.eoi()) {
    throw ReaderException{"EOI"};
  }
  return readForm(tokeniser);
}
