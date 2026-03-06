#ifndef INCLUDE_BLOOMFILTER_H
#define INCLUDE_BLOOMFILTER_H

#include <bitset>
#include <concepts>
#include <cstddef>
#include <cstdint>
#include <functional>
#include <type_traits>
#include <utility>
// IWYU pragma: no_include <memory>

namespace mal {

template <typename KEY, std::size_t SIZE, std::size_t ELEMENTS,
          typename HASH = std::hash<KEY>,
          std::uint32_t ITERATIONS =
              (SIZE / ELEMENTS) * 693 / 1000, // approximation of ln2
          std::size_t MULTIPLIER = 0x9e3779b9 // alternative: 0xff51afd7ed558ccd
                                              // (MurmurHash3/CityHash)
          >
class BloomFilter {
private:
  static std::size_t bitNumber(std::size_t hash, std::uint32_t seed) {
    if constexpr ((SIZE & (SIZE - 1)) == 0) {
      return (hash ^ (seed * MULTIPLIER)) & (SIZE - 1);
    } else {
      return (hash ^ (seed * MULTIPLIER)) % SIZE;
    }
  }

  template <std::uint32_t... Is>
  void setBits(std::size_t hash, std::integer_sequence<std::uint32_t, Is...>) {
    (bits.set(bitNumber(hash, Is + 1)), ...);
  }

  template <std::uint32_t... Is>
  bool testBits(std::size_t hash,
                std::integer_sequence<std::uint32_t, Is...>) const {
    return (bits.test(bitNumber(hash, Is + 1)) && ...);
  }

public:
  void insert(const KEY &key) {
    auto hash = HASH{}(key);
    setBits(hash, std::make_integer_sequence<std::uint32_t, ITERATIONS>{});
  }

  bool possiblyContains(const KEY &key) const {
    auto hash = HASH{}(key);
    return testBits(hash,
                    std::make_integer_sequence<std::uint32_t, ITERATIONS>{});
  }

  template <typename PHK>
    requires (!std::same_as<std::decay_t<KEY>, PHK>)
  bool possiblyContains(PHK &&phk) const {
    auto hash = HASH{}(std::forward<PHK>(phk));
    return testBits(hash,
                    std::make_integer_sequence<std::uint32_t, ITERATIONS>{});
  }

private:
  std::bitset<SIZE> bits;
};


} // namespace mal

#endif // INCLUDE_BLOOMFILTER_H
