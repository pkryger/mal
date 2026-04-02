#include "StringEscaping.h" // IWYU pragma: associated

#include <string>
#include <string_view>
#ifdef MAL_HAVE_NEON

#include <arm_neon.h>
#include <bit>
#include <cassert>
#include <cstddef>
#include <cstdint>
#include <optional>
#include <tuple>
#include <utility>
#endif // MAL_HAVE_NEON



// NOLINTBEGIN(cppcoreguidelines-pro-type-reinterpret-cast, cppcoreguidelines-pro-bounds-pointer-arithmetic) - escape and unescape is about pointer arithmetic and ARM NEON intrinsics

namespace {

void maybe_escape_and_append_character(char c, std::string &out) {
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

#ifdef MAL_HAVE_NEON

// This is modified from simdjson library:
// https://github.com/simdjson/simdjson/blob/45caa86/include/simdjson/generic/builder/json_string_builder-inl.h#L224-L260

std::pair<std::size_t, std::optional<char>>
find_next_character_to_unescape(std::string_view view,
                                std::size_t location) noexcept {

  const std::size_t len = view.size();
  const std::uint8_t *ptr =
      reinterpret_cast<const std::uint8_t *>(view.data()) + location;
  std::size_t remaining = len - location;
  auto unescaped_after_pos = [&](std::size_t pos) -> std::optional<char> {
    if (++pos < view.size()) {
      switch (view[pos]) {
      case '\\':
        return '\\';
      case 'n':
        return '\n';
      default:
        return view[pos];
      }
    }
    return {};
  };

  {
    // SIMD constants for characters requiring escape
    static const uint8x16_t v92 = vdupq_n_u8(92);  // '\\'

    static constexpr std::size_t chunk_size{sizeof(uint8x16_t)};
    while (remaining >= chunk_size) {
      const uint8x16_t word = vld1q_u8(ptr);
      const uint8x16_t needs_unescape = vceqq_u8(word, v92);

      const uint8x8_t res = vshrn_n_u16(vreinterpretq_u16_u8(needs_unescape), 4);
      const std::uint64_t mask = vget_lane_u64(vreinterpret_u64_u8(res), 0);
      if (mask != 0) {
        const auto offset = static_cast<std::size_t>(
            ptr - reinterpret_cast<const std::uint8_t *>(view.data()));
        const auto trailing_zeros =
            static_cast<std::size_t>(std::countr_zero(mask));
        const auto pos = offset + (trailing_zeros >> 2);
        auto unescaped = unescaped_after_pos(pos);
        if (unescaped) {
          return {pos, unescaped};
        }
      }
      ptr += chunk_size;
      remaining -= chunk_size;
    }
  }
  {
    // SIMD constants for characters requiring escape
    static const uint8x8_t v92 = vdup_n_u8(92);  // '\\'

    static constexpr std::size_t chunk_size{sizeof(uint8x8_t)};
    if (remaining >= chunk_size) {
      const uint8x8_t word = vld1_u8(ptr);
      const uint8x8_t needs_unescape = vceq_u8(word, v92);

      const std::uint64_t mask =
          vget_lane_u64(vreinterpret_u64_u8(needs_unescape), 0);
      if (mask != 0) {
        const auto offset = static_cast<std::size_t>(
            ptr - reinterpret_cast<const std::uint8_t *>(view.data()));
        const auto trailing_zeros =
            static_cast<std::size_t>(std::countr_zero(mask));
        const auto pos = offset + (trailing_zeros >> 3);
        auto unescaped = unescaped_after_pos(pos);
        if (unescaped) {
          return {pos, unescaped};
        }
      }
      ptr += chunk_size;
      remaining -= chunk_size;
    }
  }
  {
    // scalar constants for characters requiring escape
    static constexpr std::uint8_t v92 = 92;  // '\\'

    while (remaining > 0) {
      const std::uint8_t word = *ptr;
      const bool needs_unescape = word == v92;

      if (needs_unescape) {
        const auto offset = static_cast<std::size_t>(
            ptr - reinterpret_cast<const std::uint8_t *>(view.data()));
        auto unescaped = unescaped_after_pos(offset);
        if (unescaped) {
          return {offset, unescaped};
        }
      }
      ++ptr;
      --remaining;
    }
  }
  return {static_cast<std::size_t>(view.size()), {}};
}

std::size_t find_next_character_to_escape(std::string_view view,
                                          std::size_t location) noexcept {
  const std::size_t len = view.size();
  const std::uint8_t *ptr =
      reinterpret_cast<const std::uint8_t *>(view.data()) + location;
  std::size_t remaining = len - location;
  {
    // SIMD constants for characters requiring escape
    static const uint8x16_t v10 = vdupq_n_u8(10);  // '\n'
    static const uint8x16_t v34 = vdupq_n_u8(34);  // '"'
    static const uint8x16_t v92 = vdupq_n_u8(92);  // '\\'

    static constexpr std::size_t chunk_size{sizeof(uint8x16_t)};
    while (remaining >= chunk_size) {
      const uint8x16_t word = vld1q_u8(ptr);

      uint8x16_t needs_escape = vceqq_u8(word, v10);
      needs_escape = vorrq_u8(needs_escape, vceqq_u8(word, v34));
      needs_escape = vorrq_u8(needs_escape, vceqq_u8(word, v92));

      const uint8x8_t res = vshrn_n_u16(vreinterpretq_u16_u8(needs_escape), 4);
      const std::uint64_t mask = vget_lane_u64(vreinterpret_u64_u8(res), 0);
      if (mask != 0) {
        const auto offset = static_cast<std::size_t>(
            ptr - reinterpret_cast<const std::uint8_t *>(view.data()));
        const auto trailing_zeros =
            static_cast<std::size_t>(std::countr_zero(mask));
        return offset + (trailing_zeros >> 2);
      }
      ptr += chunk_size;
      remaining -= chunk_size;
    }
  }
  {
    // SIMD constants for characters requiring escape
    static const uint8x8_t v10 = vdup_n_u8(10);  // '\n'
    static const uint8x8_t v34 = vdup_n_u8(34);  // '"'
    static const uint8x8_t v92 = vdup_n_u8(92);  // '\\'

    static constexpr std::size_t chunk_size{sizeof(uint8x8_t)};
    if (remaining >= chunk_size) {
      const uint8x8_t word = vld1_u8(ptr);

      uint8x8_t needs_escape = vceq_u8(word, v10);
      needs_escape = vorr_u8(needs_escape, vceq_u8(word, v34));
      needs_escape = vorr_u8(needs_escape, vceq_u8(word, v92));

      const std::uint64_t mask =
          vget_lane_u64(vreinterpret_u64_u8(needs_escape), 0);
      if (mask != 0) {
        const auto offset = static_cast<std::size_t>(
            ptr - reinterpret_cast<const std::uint8_t *>(view.data()));
        const auto trailing_zeros =
            static_cast<std::size_t>(std::countr_zero(mask));
        return offset + (trailing_zeros >> 3);
      }
      ptr += chunk_size;
      remaining -= chunk_size;
    }
  }
  {
    // scalar constants for characters requiring escape
    static constexpr std::uint8_t v10 = 10;  // '\n'
    static constexpr std::uint8_t v34 = 34;  // '"'
    static constexpr std::uint8_t v92 = 92;  // '\\'

    while (remaining > 0) {
      const std::uint8_t word = *ptr;

      bool needs_escape = word == v10;
      needs_escape |= word == v34;
      needs_escape |= word == v92;

      if (needs_escape) {
        const auto offset = static_cast<std::size_t>(
            ptr - reinterpret_cast<const std::uint8_t *>(view.data()));
        return offset;
      }
      ++ptr;
      --remaining;
    }
  }

  return static_cast<std::size_t>(view.size());
}
#endif // MAL_HAVE_NEON

} // namespace

namespace mal {

#ifdef MAL_HAVE_NEON

std::string unescapeString(std::string_view input) {
  const std::string_view view{input.begin() + 1, input.end() - 1};
  auto [location, unescaped] = find_next_character_to_unescape(view, 0);
  if (location == view.size()) [[likely]] {
    assert(!unescaped);
    return std::string{view};
  }

  std::string out;
  const auto size = view.size();
  out.reserve(size - 1);

  std::size_t pos = 0;
  while (location < size) {
    assert(unescaped);
    out.append(view.data() + pos, location - pos);
    out += *unescaped;
    pos = location + 2;
    std::tie(location, unescaped) = find_next_character_to_unescape(view, pos);
  }
  out.append(view.data() + pos, location - pos);
  out.shrink_to_fit();
  return out;
}

#else

std::string unescapeString(std::string_view input) {
  std::string out;
  out.reserve(input.size() - 2);
  for (auto i = input.begin() + 1, end = input.end() - 1; i != end; ++i) {
    const char chr = *i;
    if (chr == '\\') {
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
    } else {
      out += chr;
    }
  }
  out.shrink_to_fit();
  return out;
}

#endif // MAL_HAVE_NEON


#ifdef MAL_HAVE_NEON

std::string escapeString(std::string_view input) {
  std::string out{'"'};
  const auto size = input.size();
  auto location = find_next_character_to_escape(input, 0);
  if (location == size) [[likely]] {
    out.reserve(size + 2);
    out.append(input.data(), size);
    out += '"';
    return out;
  }

  out.reserve((size * 2) + 2);
  std::size_t pos = 0;
  while (location < size) {
    out.append(input.data() + pos, location - pos);
    maybe_escape_and_append_character(input[location], out);
    pos = location + 1;
    location = find_next_character_to_escape(input, pos);
  }
  out.append(input.data() + pos, location - pos);

  out += '"';
  out.shrink_to_fit();
  return out;
}

#else

std::string escapeString(std::string_view input) {
  std::string out{'"'};
  out.reserve(input.size() + 2);
  for (auto&& chr : input) {
    maybe_escape_and_append_character(chr, out);
  }
  out += '"';
  return out;
}

#endif // MAL_HAVE_NEON

} // namespace mal

// NOLINTEND(cppcoreguidelines-pro-type-reinterpret-cast, cppcoreguidelines-pro-bounds-pointer-arithmetic) - escape and unescape is about pointer arithmetic and ARM NEON intrinsics
