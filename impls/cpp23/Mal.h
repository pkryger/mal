#ifndef INCLUDE_MAL
#define INCLUDE_MAL

#include "FunctionRef.h"

#include <cassert>
#ifndef NDEBUG
#include <cstddef>
#endif
#include <memory>
#include <span>
#include <stack>
#include <tuple>
#include <type_traits>
#include <utility>

namespace mal {

class GarbageCollectible {
public:
  virtual ~GarbageCollectible() = default;
};
using GarbageCollectiblePtr = std::shared_ptr<GarbageCollectible>;

using GarbageCollectRegister = void(GarbageCollectiblePtr);
using GarbageCollectRegisterStack =
    std::stack<std::function_ref<GarbageCollectRegister>>;

inline GarbageCollectRegisterStack& gcStack() {
  static GarbageCollectRegisterStack stack;
  return stack;
}

class GarbageCollectGuard {
public:
  template <typename FUNC>
  explicit GarbageCollectGuard(FUNC &&func) {
    gcStack().emplace(std::forward<FUNC>(func));
#ifndef NDEBUG
    stackSize = gcStack().size();
#endif
  }

  ~GarbageCollectGuard() {
#ifndef NDEBUG
    assert(gcStack().size() == stackSize);
#endif
    gcStack().pop();
  }

#ifndef NDEBUG
private:
  std::size_t stackSize;
#endif
};

class EnvBase;
using EnvPtr = std::shared_ptr<EnvBase>;

class Value;
using ValuePtr = std::shared_ptr<const Value>;

using ValuesSpan = std::span<const ValuePtr>;

template <typename TYPE, typename... ARGS>
[[nodiscard]] std::shared_ptr<std::decay_t<TYPE>> make(ARGS &&...args) {
  assert(!gcStack().empty());
  auto res = std::make_shared<std::decay_t<TYPE>>(std::forward<ARGS>(args)...);
  gcStack().top()(res);
  return res;
}

template <typename TYPE>
[[nodiscard]] const std::decay_t<TYPE> *to(ValuePtr ptr) noexcept {
  return dynamic_cast<const std::decay_t<TYPE> *>(ptr.get());
}

using EvalFn = ValuePtr(ValuePtr, EnvPtr);
using InvocableResult = std::tuple<ValuePtr, EnvPtr, bool>;


} // namespace mal

#endif // INCLUDE_MAL
