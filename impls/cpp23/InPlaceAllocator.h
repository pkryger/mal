#ifndef INCLUDE_INPLACEALLOCATOR
#define INCLUDE_INPLACEALLOCATOR

#include <array>
#include <cstddef>
#include <memory>
#include <new> // IWYU pragma: keep
#include <utility>
// IWYU pragma: no_include <__cstddef/max_align_t.h>
// IWYU pragma: no_include <__new/exceptions.h>
// IWYU pragma: no_include <__cstddef/byte.h>

namespace mal {

template <std::size_t SIZE> class InPlaceStorage {
public:
  // NOLINTNEXTLINE(cppcoreguidelines-pro-type-member-init) - use default initialisation for buffer_
  InPlaceStorage() noexcept = default;
  InPlaceStorage(const InPlaceStorage &) = delete;
  InPlaceStorage& operator=(const InPlaceStorage &) = delete;
  InPlaceStorage(InPlaceStorage &&) = delete;
  InPlaceStorage &operator=(InPlaceStorage &&) = delete;
  ~InPlaceStorage() = default;

  void *allocate(std::size_t n) {
    assert(current_ <= buffer_.size());
    assert(n <= buffer_.size() - current_);
    if (n > buffer_.size() - current_) [[unlikely]] {
      throw std::bad_alloc();
    }
    // NOLINTNEXTLINE(cppcoreguidelines-pro-bounds-pointer-arithmetic) - raw memory
    return buffer_.data() + std::exchange(current_, current_ + n);
  }

private:
  using Buffer = std::array<std::byte, SIZE>;
  alignas(std::max_align_t) Buffer buffer_;
  Buffer::size_type current_{0};
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
