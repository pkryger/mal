#ifndef INCLUDE_GARBAGE_COLLECTOR
#define INCLUDE_GARBAGE_COLLECTOR

#include <atomic>
#include <algorithm>
#include <cassert>
#include <chrono>
#include <condition_variable>
#include <cstddef>
#include <cstdlib>
#include <iterator>
#include <memory>
#include <mutex>
#include <stop_token>
#include <thread>
#include <type_traits>
#include <utility>
// IWYU pragma: no_include <__cstddef/byte.h>
// IWYU pragma: no_include <malloc/_malloc.h>

namespace mal {

class GarbageCollectible {
public:
  virtual ~GarbageCollectible() = default;
};

namespace detail {

struct Node {
  Node *next_;
    std::shared_ptr<GarbageCollectible> value_;
    bool owned_;
    explicit Node(Node *next, const std::shared_ptr<GarbageCollectible> &value) noexcept
        : next_{next}, value_{value}, owned_{false} {}
};

struct NodeBufferMixin {
  std::byte *buffer_;
};

std::size_t sharedPtrControlSize();
std::size_t sharedPtrControlAlign();

constexpr std::size_t align_up(std::size_t value, std::size_t alignment) {
    return (value + alignment - 1) & ~(alignment - 1);
}

struct Layout {
  constexpr Layout(std::size_t nodeSize, std::size_t nodeAlign,
                   std::size_t controlSize, std::size_t controlAlign,
                   std::size_t tSize, std::size_t tAlign) noexcept
      : maxAlign{std::max({nodeAlign, controlAlign, tAlign})}, offsetNode{0},
        offsetControl{align_up(offsetNode + nodeSize, controlAlign)},
        offsetT{align_up(offsetControl + controlSize, tAlign)},
        totalSize{align_up(offsetT + tSize, maxAlign)} {}

  constexpr Layout(const Layout &) noexcept = default;
  constexpr Layout(Layout &&) noexcept = default;
  constexpr Layout& operator=(const Layout &) noexcept = default;
  constexpr Layout& operator=(Layout &&) noexcept = default;

  std::size_t maxAlign;
  std::size_t offsetNode;
  std::size_t offsetControl;
  std::size_t offsetT;
  std::size_t totalSize;
};

template <typename T> static Layout layout_of() {
  static const Layout layout_{sizeof(Node),
                              alignof(Node),
                              sharedPtrControlSize(),
                              sharedPtrControlAlign(),
                              sizeof(T),
                              alignof(T)};

  return layout_;
}


template <typename T, typename ORIG_T = T>
struct NodeValueAllocator : public NodeBufferMixin {
  using value_type = T;

  explicit NodeValueAllocator(std::byte *buffer) : NodeBufferMixin{buffer} {}

  template <typename U> struct rebind {
    using other = NodeValueAllocator<U, ORIG_T>;
  };

  template <typename U>
  explicit NodeValueAllocator(
      const NodeValueAllocator<U, ORIG_T> &other) noexcept
      : NodeBufferMixin{other.buffer_} {}

  T *allocate(std::size_t n) noexcept {
    assert(n == 1);
    return reinterpret_cast<T *>(buffer_  + layout_of<ORIG_T>().offsetControl);
  }

  void deallocate(T * ptr, std::size_t n) noexcept {
    assert(n == 1);
    assert(reinterpret_cast<void *>(ptr) ==
           reinterpret_cast<void *>(buffer_ + layout_of<ORIG_T>().offsetControl));
    auto node = reinterpret_cast<Node *>(buffer_);
    if (node ->owned_) {
      std::free(buffer_);
    }
  }
};

} // namespace detail

struct NodeBuffer {
  std::unique_ptr<std::byte, void(*)(void *)> buffer_;

  detail::Node *node() {
    assert(buffer_);
    return reinterpret_cast<detail::Node *>(buffer_.get());
  }

  template <typename T, typename... ARGS>
  explicit NodeBuffer(std::shared_ptr<T> &out, ARGS &&...args)
      : buffer_{static_cast<std::byte *>(
                    std::aligned_alloc(detail::layout_of<T>().maxAlign,
                                       detail::layout_of<T>().totalSize)),
                &std::free} {
    assert(detail::sharedPtrControlSize() > 0);
    assert(detail::sharedPtrControlAlign() > 0);
    std::construct_at(
        node(), nullptr,
        std::allocate_shared<T>(detail::NodeValueAllocator<T>{buffer_.get()},
                                std::forward<ARGS>(args)...));
    out = std::static_pointer_cast<T>(node()->value_);
  }

  ~NodeBuffer() {
    if (buffer_) {
      auto n = node();
      if (n->value_.use_count() == 1) {
        std::destroy_at(n);
      } else {
        n->value_.reset();
        std::destroy_at(std::addressof(n->value_));
        n->owned_ = true;
        auto _ = buffer_.release();
        (void)_;
      }
    }
  }

  [[nodiscard]]
  detail::Node *release(detail::Node *head) {
    auto n = node();
    n->next_ = head;
    return reinterpret_cast<detail::Node *>(buffer_.release());
  }
};

namespace deatil {

class SPSCList {
public:

  template <bool CONST> class Iterator {
  public:
    using value_type =
        std::shared_ptr<std::conditional_t<CONST, const GarbageCollectible,
                                           GarbageCollectible>>;
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
      return lhs.current_ == rhs.current_;
    }

    friend bool operator==(const Iterator &lhs, std::default_sentinel_t) {
      return lhs.current_ == nullptr;
    }

  private:
    friend class SPSCList;
    Iterator(detail::Node *current) : current_{current} {}
    detail::Node *current_;
  };

  auto begin() {
    return Iterator<false>{head_.get_sync()};
  }

  auto begin() const {
    return Iterator<true>{head_.get_sync()};
  }

  auto end() { return std::default_sentinel; }

  auto end() const { return std::default_sentinel; }

  void push_front(NodeBuffer &nodeBuffer) {
    head_.set_sync(nodeBuffer.release(head_.get()));
  }

  explicit SPSCList() : head_ {
    []() {
      std::shared_ptr<GarbageCollectible> res;
      NodeBuffer nodeBuffer(res);
      return nodeBuffer.release(nullptr);
    }()} {}

  void erase_next(Iterator<false> it) {
    assert(it.current_);
    assert(it.current_->next_);
    std::free(std::exchange(it.current_->next_, it.current_->next_->next_));
  }

  ~SPSCList() {
    detail::Node *current = head_.get();
    while (current) {
      const bool last = current->value_.use_count() == 1;
      std::destroy_at(std::addressof(current->value_));
      auto n = std::exchange(current, current->next_);
      if (last) {
        std::free(n);
      }
    }
  }

private:

  class Head {
  public:
    explicit Head(detail::Node *ptr) noexcept : async_{ptr}, sync_{ptr} {}

    detail::Node *get_sync() const noexcept {
      return sync_.load(std::memory_order::acquire);
    }

    detail::Node *get() const noexcept {
      return async_;
    }

    void set_sync(detail::Node *ptr) noexcept {
      async_ = ptr;
      sync_.store(ptr, std::memory_order::release);
    }

  private:
    detail::Node *async_;
    std::atomic<detail::Node *> sync_;
  } head_;
};

} // namespace detail

inline static std::size_t ChunkSize = 16;

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
    while (it != list_.end()) {
      // skipping the head_ [sic!]
      auto next = [&]() {
        auto res = it;
        return ++res;
      }();
      if (next != list_.end() && !*next) {
        list_.erase_next(it);
      } else {
        ++it;
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
            if (cv_.wait_for(guard, stoken, std::chrono::seconds(1),
                             [&]() { return stoken.stop_requested(); })) {
              return;
            }
          }
        }} {}

  ~GarbageCollector() {
    thread_.request_stop();
    cv_.notify_all();
  }

  void registerValue(NodeBuffer &nodeBuffer) {
    list_.push_front(nodeBuffer);
  }

private:
  deatil::SPSCList list_;
  std::condition_variable_any cv_;
  std::mutex mtx_;
  std::jthread thread_;
};

} // namespace mal

#endif // INCLUDE_GARBAGE_COLLECTOR
