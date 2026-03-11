#ifndef INCLUDE_INPLACEALLOCATOR
#define INCLUDE_INPLACEALLOCATOR

#include <cstddef>
#include <memory>
#include <new> // IWYU pragma: keep
#include <utility>
// IWYU pragma: no_include <__cstddef/max_align_t.h>
// IWYU pragma: no_include <__new/exceptions.h>

namespace mal {

template <std::size_t SIZE> class InPlaceStorage {
public:
  InPlaceStorage() : current_{buffer_} {}

  InPlaceStorage(const InPlaceStorage &) = delete;
  InPlaceStorage& operator=(const InPlaceStorage &) = delete;
  InPlaceStorage(InPlaceStorage &&) = delete;
  InPlaceStorage &operator=(InPlaceStorage &&) = delete;

  void *allocate(std::size_t n) {
    if (n > static_cast<std::size_t>((buffer_ + SIZE) - current_))
        [[unlikely]] {
      throw std::bad_alloc();
    }
    return std::exchange(current_, current_ + n);
  }

private:
  alignas(std::max_align_t) char buffer_[SIZE];
  char *current_;
};

template <typename T, std::size_t SIZE> class InPlaceAllocator {
public:
  using Storage = InPlaceStorage<SIZE * sizeof(T)>;

  using value_type = T;
  template <typename U> struct rebind {
    using other = InPlaceAllocator<U, SIZE>;
  };

  explicit InPlaceAllocator(Storage &storage) noexcept
      : storage_{std::addressof(storage)} {}

  template <typename U>
  InPlaceAllocator(InPlaceAllocator<U, SIZE> &other) noexcept
      : storage_{other.storage_} {}

  T *allocate(std::size_t n) {
    return static_cast<T *>(storage_->allocate(sizeof(T) * n));
  }

  void deallocate(T * /*object*/, std::size_t /* n */) noexcept {}

  friend bool operator==(const InPlaceAllocator &lhs,
                         const InPlaceAllocator &rhs) {
    return lhs.storage_ == rhs.storage_;
  }

private:
  Storage *storage_;
};

} // namespace mal

#endif // INCLUDE_INPLACEALLOCATOR
