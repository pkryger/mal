#ifndef INCLUDE_FUNCTIONREF
#define INCLUDE_FUNCTIONREF

#include <functional>
#include <memory>
#include <concepts>
#include <type_traits>
#include <utility>

namespace mal {

template <typename...> class FunctionRef;

template <typename RET, typename... ARGS> class FunctionRef<RET(ARGS...)> {
public:
  FunctionRef() = delete;
  constexpr FunctionRef(const FunctionRef &rhs) noexcept = default;
  constexpr FunctionRef(FunctionRef &&rhs) noexcept = default;
  constexpr FunctionRef &operator=(const FunctionRef &rhs) noexcept = default;
  constexpr FunctionRef &operator=(FunctionRef &&rhs) noexcept = default;


  template <typename FUNC>
    requires std::invocable<FUNC, ARGS...> &&
             (!std::is_same_v<std::decay_t<FUNC>, FunctionRef>)
  constexpr explicit FunctionRef(FUNC &&func) noexcept
      : obj_{const_cast<void *>(
            reinterpret_cast<const void *>(std::addressof(func)))},
        callback_{[](void *obj, ARGS ...args) noexcept(noexcept(std::invoke(
                      func, std::forward<ARGS>(args)...))) -> RET {
          return std::invoke(
              *reinterpret_cast<std::add_pointer_t<std::decay_t<FUNC>>>(obj),
              std::forward<ARGS>(args)...);
        }} {}

  template <typename FUNC>
    requires std::invocable<FUNC, ARGS...> &&
             (!std::is_same_v<std::decay_t<FUNC>, FunctionRef>)
  constexpr FunctionRef &operator=(FUNC &&func) noexcept {
    obj_ = const_cast<void *>(reinterpret_cast<const void *>(std::addressof(func)));
    callback_ = [](void *obj, ARGS...args) noexcept(noexcept(std::invoke(
                    func, std::forward<ARGS>(args)...))) -> RET {
      return std::invoke(
          *reinterpret_cast<std::add_pointer_t<std::decay_t<FUNC>>>(obj),
          std::forward<ARGS>(args)...);
    };
    return *this;
  }

  template <typename... CALL_ARGS>
    requires(std::convertible_to<CALL_ARGS, ARGS> && ...)
  RET operator()(CALL_ARGS &&...args) const noexcept(
    noexcept(std::invoke(callback_, obj_, std::forward<CALL_ARGS>(args)...))) {
    return std::invoke(callback_, obj_, std::forward<CALL_ARGS>(args)...);
  }

  constexpr void swap(FunctionRef &other) {
    std::swap(obj_, other.obj_);
    std::swap(callback_, other.callback_);
  }

private:
  void *obj_{nullptr};
  RET (*callback_)(void *, ARGS...){nullptr};
};

template <typename RET, typename... ARGS>
constexpr void swap(FunctionRef<RET(ARGS...)> &lhs,
                    FunctionRef<RET(ARGS...)> &rhs) noexcept {
  lhs.swap(rhs);
}

} // namespace mal

#ifndef __cpp_lib_function_ref
namespace std {

template <typename... Ts> using function_ref = ::mal::FunctionRef<Ts...>;

} // namespace std
#endif

#endif // INCLUDE_FUNCTIONREF
