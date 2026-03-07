#ifndef INCLUDE_FUNCTIONREF
#define INCLUDE_FUNCTIONREF
/*
  C++23 lacks of the std::function_ref implementation.  Using
  https://github.com/llvm/llvm-project/pull/94894 as an inspiration. Limited
  the implementation to only support function pointer and an object.
  Explicitly left out support for member function, and for std::nontype, as
  they were not needed.  Also left out template deduction guides, and const
  support may be sketchy.

  Main inspiration for last template parameter (bool NOEX) comes from libstd++.
*/
#include <cassert>
#include <memory>
#include <type_traits>
#include <utility>

namespace mal {

template <typename...> class FunctionRef;

template <typename> inline constexpr bool IsFunctionRef{false};
template <typename RET, typename... ARGS>
inline constexpr bool IsFunctionRef<FunctionRef<RET, ARGS...>>{true};


template <typename RET, typename... ARGS, bool NOEX>
class FunctionRef<RET(ARGS...) noexcept(NOEX)> {
public:
  FunctionRef() = delete;
  constexpr FunctionRef(const FunctionRef &rhs) noexcept = default;
  constexpr FunctionRef(FunctionRef &&rhs) noexcept = default;
  constexpr FunctionRef &operator=(const FunctionRef &rhs) noexcept = default;
  constexpr FunctionRef &operator=(FunctionRef &&rhs) noexcept = default;

  template <typename T>
    requires(!IsFunctionRef<T>) && (!std::is_pointer_v<T>)
  FunctionRef &operator&(T) = delete;

  template <typename... TS>
  inline static constexpr bool IsInvocableUsing =
      std::conditional_t<NOEX, std::is_nothrow_invocable_r<RET, TS..., ARGS...>,
        std::is_invocable_r<RET, TS..., ARGS...>>::value;


  template <typename FUNC>
    requires std::is_function_v<FUNC> && IsInvocableUsing<FUNC>
  constexpr FunctionRef(FUNC *func_ptr) noexcept
      : storage_{func_ptr},
        callback_{
            [](Storage storage, ARGS &&...args) static noexcept(NOEX) -> RET {
              return get<FUNC>(storage)(std::forward<ARGS>(args)...);
            }} {
    assert(func_ptr != nullptr);
  }

  template <typename FUNC>
    requires(
        !IsFunctionRef<std::remove_cv_t<FUNC>> &&
        !std::is_member_function_pointer_v<std::remove_reference_t<FUNC>> &&
        IsInvocableUsing<FUNC>)
  constexpr FunctionRef(FUNC &&obj) noexcept
      : storage_(std::addressof(obj)),
        callback_{
            [](Storage storage, ARGS &&...args) static noexcept(NOEX) -> RET {
              auto &obj = *get<std::remove_reference_t<FUNC>>(storage);
              return obj(std::forward<ARGS>(args)...);
            }} {}

  RET operator()(ARGS ...args) const noexcept(NOEX) {
    return callback_(storage_, std::forward<ARGS>(args)...);
  }

private:
  union Storage {
    void *obj_;
    void const *const_obj_;
    RET (*fn_)(ARGS...) noexcept(NOEX);

    constexpr explicit Storage() noexcept : obj_{nullptr} {}

    template <typename T> constexpr explicit Storage(T *ptr) noexcept {
      if constexpr (std::is_object_v<T>) {
        if constexpr (std::is_const_v<T>) {
          const_obj_ = ptr;
        } else {
          obj_ = ptr;
        }
      } else {
        static_assert(std::is_function_v<T>);
        fn_ = ptr;
      }
    }
  } storage_;

  template <typename T> static constexpr auto get(Storage storage) noexcept {
      if constexpr (std::is_object_v<T>) {
        if constexpr (std::is_const_v<T>) {
          return static_cast<T *>(storage.const_obj_);
        } else {
          return static_cast<T *>(storage.obj_);
        }
      } else {
        static_assert(std::is_function_v<T>);
        return reinterpret_cast<T *>(storage.fn_);
      }
  }
  RET (*callback_)(Storage, ARGS &&...) noexcept(NOEX);
};

} // namespace mal

#ifndef __cpp_lib_function_ref
namespace std {

template <typename... Ts> using function_ref = ::mal::FunctionRef<Ts...>;

} // namespace std
#endif

#endif // INCLUDE_FUNCTIONREF
