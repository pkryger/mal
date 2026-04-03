#include "Reader.h"
#include "Mal.h"
#include "StringEscaping.h"

#include <charconv>
#include <iterator>
// NOLINTNEXTLINE(readability-use-concise-preprocessor-directives) - consistent checks
#if !defined(__cpp_lib_ranges_stride)
#include "Ranges.h"
#endif // __cpp_lib_ranges_stride
#include "Types.h"

#include <algorithm>
#include <array>
#include <cassert>
#include <cstdint>
#include <format>
#include <limits>
// NOLINTNEXTLINE(readability-use-concise-preprocessor-directives) - consistent checks
#if defined(__cpp_lib_ranges_stride)
#include <ranges>
#endif // __cpp_lib_ranges_stride
#include <regex>
#include <string>
#include <string_view>
#include <system_error>
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

const std::array tokenRegexes = {
  std::regex("~@"),
  std::regex(R"([\[\]{}()'`~^@])"),
  std::regex(R"("(?:\\.|[^\\"])*")"),
  std::regex(R"([^\s\[\]{}('"`,;)]+)"),
};

const std::array constants = {
  std::pair{"nil", Constant::nilValue()},
  std::pair{"true", Constant::trueValue()},
  std::pair{"false", Constant::falseValue()},
};

const std::array macros = {
  std::pair{"@", "deref"},
  std::pair{"`", "quasiquote"},
  std::pair{"'", "quote"},
  std::pair{"~@", "splice-unquote"},
  std::pair{"~", "unquote"},
};

class Tokeniser {
public:
  explicit Tokeniser(const std::string &str) : str_{str}, pos_{str_.begin()} {
    nextToken();
  }

  [[nodiscard]] bool eoi() const noexcept { return pos_ == str_.end(); }

  void nextToken();

  [[nodiscard]] std::string_view peek() const noexcept {
    assert(!eoi());
    return token_;
  }

private:
  bool match(const std::regex &regex);

  [[nodiscard]] auto tokenSize() const {
    auto size = token_.size();
    if (size > static_cast<decltype(size)>(
                   std::numeric_limits<std::string::difference_type>::max()))
        [[unlikely]] {
      throw ReaderException(
          std::format("token at pos {} too big {}", pos_ - str_.begin(), size));
    }
    return static_cast<std::string::difference_type>(size);
  }

  std::string_view str_;
  std::string_view::const_iterator pos_;
  std::string_view token_;
};

bool Tokeniser::match(const std::regex &regex) {
  if (eoi()) {
    return false;
  }

  std::cmatch match;
  if (!std::regex_search(pos_, str_.end(), match, regex,
                         std::regex_constants::match_continuous)) {
    return false;
  }

  assert(match.size() == 1);
  assert(match.position(0) == 0);
  assert(match.length(0) > 0);
  token_ =
    {match[0].first, match[0].second};
  return true;
}

void Tokeniser::nextToken() {
  // NOLINTNEXTLINE(cppcoreguidelines-pro-bounds-pointer-arithmetic) - this is iterator
  pos_ += tokenSize();
  // Skip white spaces and comments
  while (match(std::regex{"[\\s,]+|;.*"})) {
    // NOLINTNEXTLINE(cppcoreguidelines-pro-bounds-pointer-arithmetic) - this is iterator
    pos_ += tokenSize();
  }

  if (eoi()) {
    return;
  }

  for (const auto &regex : tokenRegexes) {
    if (match(regex)) {
      return;
    }
  }

  if (*pos_ == '"') {
    throw ReaderException{"unbalanced \""};
  }
  throw ReaderException{
      std::format("mismatch from: {}", std::string{pos_, str_.end()})};
}

ValuePtr readForm(Tokeniser &/*tokeniser*/);

// NOLINTNEXTLINE(misc-no-recursion)
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

// NOLINTNEXTLINE(misc-no-recursion)
ValuePtr readAtom(Tokeniser &tokeniser) {
  assert(!tokeniser.eoi());
  auto token = tokeniser.peek();
  tokeniser.nextToken();

  if (token.front() == '"') {
    return make<String>(mal::unescapeString(token));
  }

  if (token.front() == ':') {
    return make<Keyword>(token);
  }

  if (token == "^") {
    auto meta{readForm(tokeniser)};
    return make<List>(make<Symbol>("with-meta"), readForm(tokeniser),
                      std::move(meta));
  }

  if (std::regex_match(token.begin(), token.end(), std::regex{"^[-+]?\\d+$"})) {
    // NOLINTNEXTLINE(cppcoreguidelines-init-variables) - use as an output
    std::int64_t value;
    auto [_, ec] = std::from_chars(
        [&]() {
          if (token.front() == '+') {
            return std::next(token.begin());
          }
          return token.begin();
        }(),
        token.end(), value);

    if (ec != std::errc{}) [[unlikely]] {
      throw ReaderException{std::format("cannot read number '{}'", token)};
    }
    return make<Integer>(value);
  }

  auto first = [](auto &&elt) noexcept { return elt.first; };
  auto isToken = [&](auto &&elt) noexcept { return elt == token; };
  if (const auto *constant = std::ranges::find_if(constants, isToken, first);
      constant != constants.end()) {
    return constant->second;
  }

  if (const auto *macro = std::ranges::find_if(macros, isToken, first);
      macro != macros.end()) {
    return make<List>(make<Symbol>(macro->second, macro->first),
                      readForm(tokeniser));
  }

  return make<Symbol>(token);
}

// NOLINTNEXTLINE(misc-no-recursion)
ValuePtr readForm(Tokeniser &tokeniser) {
  assert(!tokeniser.eoi());
  auto token = tokeniser.peek();
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
    for (auto key : items |
#ifdef __cpp_lib_ranges_stride
                        std::views::stride(2)
#else
                        mal::views::Stride(2)
#endif // __cpp_lib_ranges_stride
    ) {
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
