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

// NOLINTBEGIN(cppcoreguidelines-pro-type-union-access) - union for uninitialised storage
template <typename T, typename ALLOCATOR = std::allocator<T>>
  requires std::is_same_v<T, std::shared_ptr<typename T::element_type>>
class SPSCList {
public:
  class Node {
  public:
    explicit Node(Node *next): next_{next} {};
  private:
    friend class SPSCList;
    Node *next_;
    union {
      alignas(T) T value_;
    };
  };

  using NodeAllocator =
      std::allocator_traits<ALLOCATOR>::template rebind_alloc<Node>;

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

    friend bool operator==(const Iterator &lhs,
                           std::default_sentinel_t /*rhs*/) {
      return lhs.current_ == nullptr;
    }

  private:
    friend class SPSCList;
    explicit Iterator(Node *current) : current_{current} {}
    Node *current_;
  };

  auto begin() {
    return Iterator<false>{head_.get_sync()};
  }

  auto begin() const {
    return Iterator<true>{head_.get_sync()};
  }

  auto end() { return std::default_sentinel; }

  auto end() const { return std::default_sentinel; }

  // NOLINTNEXTLINE(performance-unnecessary-value-param) - move to storage in Node
  void push_front(T value) {
    head_.set_sync(new_node(head_.get(), std::move(value)));
  }

  explicit SPSCList(const ALLOCATOR &allocator = ALLOCATOR())
      : nodeAllocator_(allocator), head_{new_node(nullptr, nullptr)} {}

  void erase_next(Iterator<false> iter) {
    assert(iter.current_);
    assert(iter.current_->next_);
    delete_node(
        std::exchange(iter.current_->next_, iter.current_->next_->next_));
  }

  SPSCList(const SPSCList &) = delete;
  SPSCList& operator=(const SPSCList &) = delete;
  SPSCList(SPSCList &&) = delete;
  SPSCList& operator=(SPSCList &&) = delete;

  ~SPSCList() noexcept {
    Node *current = head_.get();
    while (current) {
      delete_node(std::exchange(current, current->next_));
    }
  }

private:
  template <typename... ARGS>
  [[nodiscard]]
  Node *new_node(Node *head, ARGS &&...args) {
    Node *node =
        std::allocator_traits<NodeAllocator>::allocate(nodeAllocator_, 1);
    try {
      std::construct_at(node, head);
      std::uninitialized_construct_using_allocator(std::addressof(node->value_),
                                                   nodeAllocator_,
                                                   std::forward<ARGS>(args)...);
      return node;
    } catch (...) {
      std::allocator_traits<NodeAllocator>::deallocate(nodeAllocator_, node,
                                                       1);
      throw;
    }
  }

  void delete_node(Node *node) noexcept {
    assert(node);
    std::allocator_traits<NodeAllocator>::destroy(nodeAllocator_,
                                                  std::addressof(node->value_));
    std::allocator_traits<NodeAllocator>::deallocate(nodeAllocator_, node, 1);
  }

  [[no_unique_address]]
  NodeAllocator nodeAllocator_;

  class Head {
  public:
    explicit Head(Node *ptr) noexcept : async_{ptr}, sync_{ptr} {}

    Node *get_sync() const noexcept {
      return sync_.load(std::memory_order::acquire);
    }

    Node *get() const noexcept {
      return async_;
    }

    void set_sync(Node *ptr) noexcept {
      async_ = ptr;
      sync_.store(ptr, std::memory_order::release);
    }

  private:
    Node *async_;
    std::atomic<Node *> sync_;
  } head_;
};
// NOLINTEND(cppcoreguidelines-pro-type-union-access)

inline static constexpr std::size_t ChunkSize = 16;

template <typename T>
  requires std::is_same_v<T, std::shared_ptr<typename T::element_type>>
class GarbageCollector {
private:
  template <typename ITERATOR, typename FUNC>
  [[nodiscard]]
  bool resetUnusedPointers(ITERATOR iter, FUNC &&yieldAndStopRequested) {
    std::size_t counter = 0;
    for (; iter != list_.end(); ++iter) {
      if (iter->use_count() == 1) {
        iter->reset();
      }
      if (std::forward<FUNC>(yieldAndStopRequested)(&counter)) {
        return true;
      }
    }
    return false;
  }

  template <typename ITERATOR, typename FUNC>
  [[nodiscard]]
  bool eraseEmptyNodes(ITERATOR iter, FUNC &&yieldAndStopRequested) {
    std::size_t counter = 0;
    while (iter != list_.end()) {
      // skipping the head_ [sic!]
      auto next = [&]() {
        auto res = iter;
        return ++res;
      }();
      if (next != list_.end() && !*next) {
        list_.erase_next(iter);
      } else {
        ++iter;
      }
      if (std::forward<FUNC>(yieldAndStopRequested)(&counter)) {
        return true;
      }
    }
    return false;
  }

public:
  GarbageCollector()
      : thread_{[&](std::stop_token stoken) {
          auto yieldAndStopRequested = [&](std::size_t *counter = nullptr) {
            if ((++*counter % ChunkSize) == 0) {
              std::this_thread::yield();
            }
            return stoken.stop_requested();
          };
          while (!stoken.stop_requested()) {
            auto iter = list_.begin();
            if (resetUnusedPointers(iter, yieldAndStopRequested)) {
              return;
            }
            if (eraseEmptyNodes(iter, yieldAndStopRequested)) {
              return;
            }
            std::unique_lock guard{mtx_};
            if (cv_.wait_for(guard, stoken, std::chrono::seconds(1),
                             [&]() { return stoken.stop_requested(); })) {
              return;
            }
          }
        }} {}

  GarbageCollector(const GarbageCollector &) = delete;
  GarbageCollector& operator=(const GarbageCollector &) = delete;
  GarbageCollector(GarbageCollector &&) = delete;
  GarbageCollector& operator=(GarbageCollector &&) = delete;

  ~GarbageCollector() noexcept {
    thread_.request_stop();
    cv_.notify_all();
  }

  void registerValue(T value) {
    list_.push_front(std::move(value));
  }

private:
  SPSCList<T> list_{};
  std::condition_variable_any cv_;
  std::mutex mtx_;
  std::jthread thread_;
};

} // namespace mal

#endif // INCLUDE_GARBAGE_COLLECTOR
