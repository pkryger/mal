#ifndef INCLUDE_LRUCACHE_H
#define INCLUDE_LRUCACHE_H

#include <cstddef>
#include <flat_map>
#include <list>
#include <optional>
#include <type_traits>
#include <utility>

namespace mal {
template <typename KEY, typename VALUE> class LruCache {
public:
  using List = std::list<std::pair<KEY, VALUE>>;
  using Map = std::flat_map<KEY, typename List::iterator>;
  using KeyView = std::conditional_t<std::is_trivially_copyable_v<KEY> &&
    sizeof(KEY) <= 2 * sizeof(void *),
    KEY, const KEY &>;

  explicit LruCache(std::size_t size) noexcept : size{size} {}
  LruCache() = default;
  explicit LruCache(const LruCache &) = default;
  LruCache& operator=(const LruCache &) = default;
  LruCache(LruCache &&) = default;
  LruCache& operator=(LruCache &&) = default;

  std::optional<VALUE> find(KeyView key) {
    if (auto it = map.find(key); it != map.end()) {
      list.splice(list.begin(), list, it->second);
      return it->second->second;
    }
    return {};
  }

  void put(KeyView key, VALUE value) {
    auto it = map.find(key);
    if (it != map.end()) {
      list.erase(it->second);
    }
    list.emplace_front(key, std::move(value));
    map.insert_or_assign(it, key, list.begin());
    if (map.size() > size) {
      map.erase(list.back().first);
      list.pop_back();
    }
  };

private:
  std::size_t size{16};
  Map map;
  List list;
};

} // namespace mal

#endif // INCLUDE_LRUCACHE_H
