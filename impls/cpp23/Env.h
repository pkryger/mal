#ifndef INCLUDE_ENV
#define INCLUDE_ENV

#include "Mal.h"
#include "BloomFilter.h"

#include <concepts>
#include <cstddef>
#include <functional> // IWYU pragma: keep
#include <iterator>
#include <ranges>
#include <string>
#include <string_view>
#include <type_traits>
#include <unordered_map>
#include <utility>
#include <vector> // IWYU pragma: keep
// IWYU pragma: no_include <__vector/vector.h>

namespace mal {

namespace detail {

template <typename T>
concept IsHashContainer = requires(T t) {
  typename T::hasher;
  typename T::key_type;
  typename T::mapped_type;
};

} // namespace detail

class EnvBase : public GarbageCollectible {
public:
  // using Key = std::uint32_t;
  // xor
  // using Key = std::uint64_t;
  // xor
  using Key = std::string;
  using KeyView = std::conditional_t<std::is_trivially_copyable_v<Key> &&
                                         sizeof(Key) <= 2 * sizeof(void *),
                                     Key, const Key &>;
  using HashKeyView = std::conditional_t<std::is_same_v<Key, std::string>,
                                         std::string_view, Key>;

  struct PreHashedKey {
    HashKeyView key;
    std::size_t hash;
  };

  struct Hash {
    using is_transparent = void;
    using hash_type = std::hash<Key>;

    template <typename T>
      requires std::same_as<Key, std::string> &&
               (std::same_as<T, const char *> ||
                std::same_as<T, std::string_view>)
    std::size_t operator()(T&& key) const
    {
      return hash_type{}(std::forward<T>(key));
    }

    std::size_t operator()(KeyView key) const {
      return hash_type{}(key);
    }

    std::size_t operator()(const PreHashedKey &phk) const {
      return phk.hash;
    }
  };

  struct Equal {
    using is_transparent = void;

    template <typename T>
      requires std::same_as<Key, std::string> &&
               (std::same_as<T, const char *> ||
                std::same_as<T, std::string_view>)
    bool operator()(KeyView lhs, T&& rhs) const
    {
      return lhs == rhs;
    }

    bool operator()(KeyView lhs, KeyView rhs) const {
      return lhs == rhs;
    }

    bool operator()(KeyView lhs, const PreHashedKey &rhs) const {
      return lhs == rhs.key;
    }
  };
  using Map = std::unordered_map<Key, ValuePtr, Hash, Equal>;

  using FindLocalKey = std::conditional_t<detail::IsHashContainer<Map>,
                                          const PreHashedKey &, KeyView>;

  template <bool CONST> class Iterator {

  public:
    using value_type = std::conditional_t<CONST, const EnvBase, EnvBase>;
    using difference_type = std::ptrdiff_t;
    using iterator_category = std::input_iterator_tag;
    using iterator_concept = std::forward_iterator_tag;

    Iterator() = default;
    explicit Iterator(const Iterator&) = default;
    Iterator& operator=(const Iterator&) = default;
    Iterator(Iterator &&) = default;
    Iterator& operator=(Iterator &&) = default;
    ~Iterator() = default;
    explicit Iterator(value_type *current) noexcept : current{current} {}


    value_type &operator*() noexcept { return *current; }
    value_type &operator*() const noexcept { return *current; }

    value_type *operator->() noexcept { return current; }
    value_type *operator->() const noexcept { return current; }

    Iterator &operator++() noexcept {
      current = current->outer_.get();
      return *this;
    }

    void operator++(int) noexcept {
      ++*this;
    }

    friend constexpr bool operator==(const Iterator &lhs,
                                     const Iterator &rhs) noexcept {
      return lhs.current == rhs.current;
    }

    friend constexpr bool operator==(const Iterator &lhs,
                                     std::default_sentinel_t) noexcept {
      return !lhs.current;
    }

  private:
    value_type *current{nullptr};
  };

  explicit EnvBase(EnvPtr outer) noexcept
      : debugEval_{outer ? outer->debugEval_ : nullptr},
        outer_{std::move(outer)},
        size_{outer_ ? outer_->size() + 1 : 1} {}

  EnvBase(EnvBase &&other) noexcept
      : debugEval_{std::move(other.debugEval_)},
        outer_{std::move(other.outer_)},
        size_{std::exchange(other.size_, 0)} {}

  virtual ~EnvBase() = default;

  ValuePtr find(KeyView key) const;

  ValuePtr find(const PreHashedKey& key) const;

  virtual ValuePtr findLocal(FindLocalKey phk) const = 0;

  virtual void insert_or_assign(Key key, ValuePtr value) = 0;

  virtual std::size_t mapsSize() const = 0;

  virtual std::vector<Key> keys() const = 0;

  auto begin() noexcept {
    return Iterator<false>{this};
  }

  auto begin() const noexcept {
    return Iterator<true>{this};
  }

  auto end() noexcept {
    return std::default_sentinel;
  }

  auto end() const noexcept {
    return std::default_sentinel;
  }

  std::size_t size() const noexcept {
    return size_;
  }

protected:
  void registerDebugEval(KeyView key, const ValuePtr& value);

private:
  ValuePtr debugEval_;
  EnvPtr outer_;
  std::size_t size_;
};

class Env : public EnvBase {
public:
  Env(EnvPtr outer) : EnvBase{std::move(outer)} {}

  void insert_or_assign(Key key, ValuePtr value) override;

  ValuePtr findLocal(FindLocalKey phk) const override;

  std::vector<Key> keys() const override {
    return std::views::keys(map_) | std::ranges::to<std::vector>();
  }

  std::size_t mapsSize() const override { return map_.size(); }

private:
  Map map_;
};


class ApplyEnv : public EnvBase {
  auto capturedEnvKeys() const {
    return *capturedEnv_ |
           std::views::transform([&](auto &&env) { return env.keys(); }) |
           std::views::join;
  }
  std::vector<Key> keys() const override;

public:
  explicit ApplyEnv(EnvPtr evalEnv, EnvPtr capturedEnv) noexcept;

  void insert_or_assign(Key key, ValuePtr value) override;

  ValuePtr findLocal(FindLocalKey phk) const override;

  std::size_t mapsSize() const override;

private:

  BloomFilter<Key, std::size_t{1} << 13, // 8192 bits = 1KiB
              std::size_t{1} << 10,      // 1024 keys -> 5 iterations for 1% of
                                         // false positive probability
              Hash>
      filter_;
  Map map_;
  EnvPtr capturedEnv_;
};


} // namespace mal

#endif // INCLUDE_ENV
