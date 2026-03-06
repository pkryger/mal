#ifndef INCLUDE_MAL
#define INCLUDE_MAL

#include <memory>
#include <span>
#include <tuple>
#include <type_traits>
#include <utility>

namespace mal {

class EnvBase;
using EnvPtr = std::shared_ptr<EnvBase>;

class Value;
using ValuePtr = std::shared_ptr<const Value>;

using ValuesSpan = std::span<const ValuePtr>;

template <typename TYPE, typename... ARGS>
[[nodiscard]] std::shared_ptr<std::decay_t<TYPE>> make(ARGS &&...args) {
  return std::make_shared<std::decay_t<TYPE>>(std::forward<ARGS>(args)...);
}

template <typename TYPE>
[[nodiscard]] const std::decay_t<TYPE> *to(ValuePtr ptr) noexcept {
  return dynamic_cast<const std::decay_t<TYPE> *>(ptr.get());
}

using EvalFn = ValuePtr(ValuePtr, EnvPtr);
using InvocableResult = std::tuple<ValuePtr, EnvPtr, bool>;


} // namespace mal

#endif // INCLUDE_MAL
