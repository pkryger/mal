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
  explicit ReadLineHistoryInit(int size) noexcept;
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
        : buffer_{buffer}, current_{current}, end_{buffer->head_} {
      assert(buffer_);
      assert(current_ < buffer_->data_.size());
    }

    value_type &operator*() noexcept
      requires(!CONST)
    {
      assert(current_ < buffer_->data_.size());
      return buffer_->data_[current_];
    }

    value_type &operator*() const noexcept {
      assert(current_ < buffer_->data_.size());
      return buffer_->data_[current_];
    }

    value_type *operator->() noexcept
      requires(!CONST)
    {
      assert(current_ < buffer_->data_.size());
      return std::addressof(buffer_->data_[current_]);
    }

    value_type *operator->() const noexcept {
      assert(current_ < buffer_->data_.size());
      return std::addressof(buffer_->data_[current_]);
    }

    Iterator &operator++() {
      if (buffer_->empty()) {
        return *this;
      }
      if (buffer_->full_ && current_ < buffer_->capacity_) {
        current_ = (current_ + 1) % buffer_->capacity_;
      } else if (current_ < end_) {
        current_++;
      }
      assert(current_ < buffer_->data_.size());
      return *this;
    }

    Iterator operator++(int) {
      auto it = *this;
      ++*this;
      return it;
    }

    friend bool operator==(const Iterator &lhs, const Iterator &rhs) {
      return lhs.buffer_ == rhs.buffer_ && lhs.current_ == rhs.current_ &&
             lhs.end_ == rhs.end_;
    }

  private:
    CyclicBuffer *buffer_;
    std::size_t current_;
    std::size_t end_;
  };

  auto begin() noexcept { return Iterator<false>{this, tail_}; }
  auto begin() const noexcept { return Iterator<true>{this, tail_}; }

  auto end() noexcept { return Iterator<false>{this, head_}; }
  auto end() const noexcept { return Iterator<true>{this, head_}; }

  explicit CyclicBuffer(std::size_t capacity)
      : data_(capacity), capacity_{capacity} {}

  template <typename T>
    requires std::convertible_to<T, VALUE>
  void push(T &&elt) {
    data_.at(head_) = std::forward<T>(elt);

    if (full_) {
      tail_ = (tail_ + 1) % capacity_;
    }
    head_ = (head_ + 1) % capacity_;
    full_ = head_ == tail_;
  }

  bool empty() const noexcept { return !full_ && (head_ == tail_); }

  size_t size() const noexcept {
    if (full_) {
      return capacity_;
    }
    assert(head_ >= tail_);
    return head_ - tail_;
  }

private:
  std::vector<VALUE> data_;
  std::size_t head_{0};
  std::size_t tail_{0};
  std::size_t capacity_;
  bool full_{false};
};



} // namespace detail

class ReadLine : detail::ReadLineHistoryInit{
public:
  explicit ReadLine();
  explicit ReadLine(const std::string &file);

  std::optional<std::string> get(const std::string &prompt);

private:
  inline static std::size_t counter_{};
  inline static std::size_t last_{};
  detail::CyclicBuffer<std::string> lines_;
  std::shared_ptr<const char> historyFile_;
  std::size_t id{++counter_};
};

} // namespace mal

#endif // INCLUDE_READLINE_H
