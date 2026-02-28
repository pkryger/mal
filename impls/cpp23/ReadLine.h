#ifndef INCLUDE_READLINE_H
#define INCLUDE_READLINE_H

#include <cassert>
#include <cstddef>
#include <memory>
#include <optional>
#include <string>
#include <type_traits>
#include <vector> // IWYU pragma: keep
#include <concepts>
#include <iterator>
#include <utility>
// IWYU pragma: no_include <__vector/vector.h>

namespace mal {

namespace detail {

class ReadLineHistoryInit {
public:
  explicit ReadLineHistoryInit(std::size_t size) noexcept;
};

template <typename VALUE>
class CyclicBuffer {
public:
  template <bool CONST> class Iterator {
  public:
    using value_type = std::conditional_t<CONST, const VALUE, VALUE>;
    using difference_type = std::ptrdiff_t;
    using iterator_category = std::input_iterator_tag;
    using iterator_concept = std::forward_iterator_tag;

    explicit Iterator(CyclicBuffer *buffer, std::size_t current)
        : buffer{buffer}, current{current}, end{buffer->head} {
      assert(buffer);
    }

    value_type &operator*() noexcept { return buffer->data[current]; }
    value_type &operator*() const noexcept { return buffer->data[current]; }

    value_type *operator->() noexcept {
      return std::addressof(buffer->data[current]);
    }
    value_type *operator->() const noexcept {
      return std::addressof(buffer->data[current]);
    }

    Iterator &operator++() {
      if (buffer->empty()) {
        return *this;
      }
      if (buffer->full && current < buffer->capacity) {
        current = (current + 1) % buffer->capacity;
      } else if (current < end) {
        current++;
      }
      return *this;
    }

    Iterator operator++(int) {
      auto it = *this;
      ++*this;
      return it;
    }

    friend bool operator==(const Iterator &lhs, const Iterator &rhs) {
      return lhs.buffer == rhs.buffer && lhs.current == rhs.current &&
             lhs.end == rhs.end;
    }

  private:
    CyclicBuffer *buffer;
    std::size_t current;
    const std::size_t end;
  };

  auto begin() noexcept { return Iterator<false>{this, tail}; }
  auto begin() const noexcept { return Iterator<true>{this, tail}; }

  auto end() noexcept { return Iterator<false>{this, head}; }
  auto end() const noexcept { return Iterator<true>{this, head}; }

  explicit CyclicBuffer(std::size_t capacity)
      : data(capacity), capacity{capacity} {}

  template <typename T>
    requires std::convertible_to<T, VALUE>
  void push(T &&elt) {
    data[head] = std::forward<T>(elt);

    if (full) {
      tail = (tail + 1) % capacity;
    }
    head = (head + 1) % capacity;
    full = head == tail;
  }

  bool empty() const noexcept { return !full && (head == tail); }

  size_t size() const noexcept {
    if (full) return capacity;
    return head - tail;
  }

private:
  std::vector<VALUE> data;
  std::size_t head{0};
  std::size_t tail{0};
  const std::size_t capacity;
  bool full{false};
};



} // namespace detail

class ReadLine : detail::ReadLineHistoryInit{
public:
  explicit ReadLine() noexcept;
  explicit ReadLine(const std::string &file);

  std::optional<std::string> get(const std::string &prompt);

private:
  inline static std::size_t counter{};
  inline static std::size_t last{};
  detail::CyclicBuffer<std::string> lines;
  const std::shared_ptr<const char> historyFile;
  std::size_t id{++counter};
};

} // namespace mal

#endif // INCLUDE_READLINE_H
