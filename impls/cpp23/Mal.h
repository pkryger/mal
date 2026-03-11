#ifndef INCLUDE_MAL
#define INCLUDE_MAL

#include "FunctionRef.h"

#include <cassert>
#include <memory>
#include <span>
#include <stack>
#include <tuple>
#include <type_traits>
#include <utility>

namespace mal {

namespace detail {

template <typename FN> class FnStack {
public:
  using StackType = std::stack<std::function_ref<FN>>;

  static StackType::value_type &top() { return stack.top(); }

  static bool empty() { return stack.empty(); };

  class Guard {
  public:
    template <typename FUNC>
    explicit Guard(FUNC &&func) {
      stack.emplace(std::forward<FUNC>(func));
    }

    Guard(const Guard &) = delete;
    Guard &operator=(const Guard &) = delete;

    ~Guard() {
      stack.pop();
    }
  };

private:
  inline static StackType stack;
};

} // namespace detail

class GarbageCollectible {
public:
  virtual ~GarbageCollectible() = default;
};
using GarbageCollectiblePtr = std::shared_ptr<GarbageCollectible>;

using GarbageCollectFn = void(GarbageCollectiblePtr);
using GarbageCollectStack = detail::FnStack<GarbageCollectFn>;

class EnvBase;
using EnvPtr = std::shared_ptr<EnvBase>;

class Value;
using ValuePtr = std::shared_ptr<const Value>;

using ValuesSpan = std::span<const ValuePtr>;

template <typename TYPE, typename... ARGS>
[[nodiscard]] std::shared_ptr<std::decay_t<TYPE>> make(ARGS &&...args) {
  auto res = std::make_shared<std::decay_t<TYPE>>(std::forward<ARGS>(args)...);
  assert(!GarbageCollectStack::empty());
  GarbageCollectStack::top()(res);
  return res;
}

template <typename TYPE>
[[nodiscard]] const std::decay_t<TYPE> *to(const ValuePtr &ptr) noexcept {
  return dynamic_cast<const std::decay_t<TYPE> *>(ptr.get());
}

using EvalFn = ValuePtr(ValuePtr, EnvPtr);
using EvalFnStack = detail::FnStack<EvalFn>;

using InvocableResult = std::tuple<ValuePtr, EnvPtr, bool>;

} // namespace mal

#endif // INCLUDE_MAL
