#ifndef INCLUDE_GARBAGE_COLLECTOR
#define INCLUDE_GARBAGE_COLLECTOR

#include <atomic>
#include <chrono>
#include <condition_variable>
#include <cstddef>
#include <iterator>
#include <memory>
#include <mutex>
#include <stop_token>
#include <thread>
#include <type_traits>
#include <utility>

namespace mal {
template <typename T>
  requires std::is_same_v<T, std::shared_ptr<typename T::element_type>>
class SPSCList {
public:
  class Node {
    friend class SPSCList;
    explicit Node(Node *next, T value) noexcept
        : next_{next}, value_{std::move(value)} {}
    Node *next_;
    T value_;
  };

  template <bool CONST> class Iterator {
  public:
    using value_type = std::conditional_t<CONST, const T, T>;
    using difference_type = std::ptrdiff_t;
    using iterator_category = std::input_iterator_tag;
    using iterator_concept = std::forward_iterator_tag;

    value_type &operator*()
      requires(!CONST)
    {
      return current_->value_;
    }

    const value_type &operator*() const { return current_->value_; }

    value_type *operator->()
      requires(!CONST)
    {
      return std::addressof(current_->value_);
    }

    const value_type *operator->() const {
      return std::addressof(current_->value_);
    }

    Iterator &operator++() {
      current_ = current_->next_;
      return *this;
    }

    void operator++(int) { ++*this; }

    friend bool operator==(const Iterator &lhs, const Iterator &rhs) {
      return lhs.current_ = rhs.current_;
    }

    friend bool operator==(const Iterator &lhs, std::default_sentinel_t) {
      return lhs.current_ != nullptr;
    }

  private:
    friend class SPSCList;
    Iterator(Node *current) : current_{current} {}
    Node *current_;
  };

  auto begin() { return Iterator<false>{head_}; }

  auto begin() const { return Iterator<true>{head_}; }

  auto end() { return std::default_sentinel; }

  auto end() const { return std::default_sentinel; }

  void push_front(T value) {
    Node *node = new Node{head_, std::move(value)};
    head_ = node;
  }

  explicit SPSCList() : head_{new Node{nullptr, nullptr}} {}

  void erase_next(Iterator<false> it) {
    assert(it.current_);
    assert(it.current_->next_);
    delete std::exchange(it.current_->next_, it.current_->next_->next_);
  }

  ~SPSCList() {
    Node *current = head_;
    while (current) {
      delete std::exchange(current, current->next_);
    }
  }

private:
  std::atomic<Node *> head_;
};

inline static std::size_t ChunkSize = 16;

template <typename T>
  requires std::is_same_v<T, std::shared_ptr<typename T::element_type>>
class GarbageCollector {
private:
  template <typename ITERATOR, typename FUNC>
  [[nodiscard]]
  bool resetUnusedPointers(ITERATOR it, FUNC &&yieldAndStopRequested) {
    std::size_t counter = 0;
    for (; it != list_.end(); ++it) {
      if (it->use_count() == 1) {
        it->reset();
      }
      if (yieldAndStopRequested(&counter)) {
        return true;
      }
    }
    return false;
  }

  template <typename ITERATOR, typename FUNC>
  [[nodiscard]]
  bool eraseEmptyNodes(ITERATOR it, FUNC &&yieldAndStopRequested) {
    std::size_t counter = 0;
    for (; it != list_.end(); ++it) {
      // skipping the head_ [sic!]
      auto next = [&]() {
        auto res = it;
        return ++res;
      }();
      if (next != list_.end() && !*next) {
        list_.erase_next(it);
      }
      if (yieldAndStopRequested(&counter)) {
        return true;
      }
    }
    return false;
  }

public:
  GarbageCollector()
      : list_{}, thread_{[&](std::stop_token stoken) {
          auto yieldAndStopRequested = [&](std::size_t *counter = nullptr) {
            if ((++*counter % ChunkSize) == 0) {
              std::this_thread::yield();
            }
            return stoken.stop_requested();
          };
          while (!stoken.stop_requested()) {
            auto it = list_.begin();
            if (resetUnusedPointers(it, yieldAndStopRequested)) {
              return;
            }
            if (eraseEmptyNodes(it, yieldAndStopRequested)) {
              return;
            }
            std::unique_lock guard{mtx_};
            bool stopRequested =
                cv_.wait_for(guard, stoken, std::chrono::seconds(1),
                             [&]() { return stoken.stop_requested(); });
            if (stopRequested) {
              return;
            }
          }
        }} {}

  ~GarbageCollector() {
    thread_.request_stop();
    cv_.notify_all();
  }

  SPSCList<T> list_;

private:
  std::condition_variable_any cv_;
  std::mutex mtx_;
  std::jthread thread_;
};

} // namespace mal

#endif // INCLUDE_GARBAGE_COLLECTOR
