#include "GarbageCollector.h"

#include <memory>
namespace mal {

namespace {
static std::size_t probedSize{0};
static std::size_t probedAlign{0};

template <typename T> struct ProbeAllocator : private detail::NodeBufferMixin {
  using value_type = T;

  explicit ProbeAllocator() = default;

  template <typename U> struct rebind {
    using other = ProbeAllocator<U>;
  };

  template <typename U>
  explicit ProbeAllocator(const ProbeAllocator<U> &) noexcept {}

  T *allocate(std::size_t n) {
    assert(n == 1);
    assert(probedSize == 0 || probedSize == sizeof(T));
    assert(probedAlign == 0 || probedAlign == alignof(T));
    probedSize = sizeof(T);
    probedAlign = alignof(T);
    return static_cast<T *>(::operator new(n * sizeof(T)));
  }

  void deallocate(T *ptr, std::size_t /* n */) {
    ::operator delete(ptr);
  }
};

} // namespace

namespace detail {

std::size_t sharedPtrControlSize() {
  static auto size = []() {
    std::allocate_shared<GarbageCollectible>(
        ProbeAllocator<GarbageCollectible>{});
    assert(probedSize != 0);
    assert(probedSize > sizeof(GarbageCollectible));
    return probedSize - sizeof(GarbageCollectible);
  }();
  return size;
}

std::size_t sharedPtrControlAlign() {
  static auto align = []() {
    std::allocate_shared<GarbageCollectible>(
        ProbeAllocator<GarbageCollectible>{});
    assert(probedAlign != 0);
    return probedAlign;
  }();
  return align;
}

} // namespace detail

} // namespace mal
