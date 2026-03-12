#ifndef INCLUDE_HIERARCHY
#define INCLUDE_HIERARCHY

#include <cstdint>
#include <tuple>
#include <type_traits>
#include <utility>

namespace mal {

namespace detail {

template <typename T, typename... CHILDREN> struct DFSNode {
  using type = T;
  using children = std::tuple<CHILDREN...>;
};

template <typename TREE> struct DFS;

template <typename T> struct DFS<DFSNode<T>> {
  static constexpr auto compute(std::uint32_t counter)
      -> std::tuple<std::uint32_t, std::uint32_t, std::uint32_t> {
    return {counter, counter, counter + 1};
  }
};

template <typename T, typename CHILD, typename... CHILDREN>
struct DFS<DFSNode<T, CHILD, CHILDREN...>> {
private:
  static constexpr auto foldChildren(std::uint32_t counter)
      -> std::tuple<std::uint32_t, std::uint32_t, std::uint32_t> {
    std::uint32_t _sink, hi, running = counter;
    auto step = [&]<typename C>() {
      std::tie(_sink, hi, running) = DFS<C>::compute(running);
    };
    (step.template operator()<CHILD>(), ...,
     step.template operator()<CHILDREN>());
    return {counter, hi, running};
  }

public:
  static constexpr auto compute(std::uint32_t counter)
      -> std::tuple<std::uint32_t, std::uint32_t, std::uint32_t> {
    auto [_, hi, next] = foldChildren(counter + 1);
    return {counter, hi, next};
  }
};

} // namespace detail

struct TypeInfo {
  uint32_t lo, hi;
  constexpr bool contains(uint32_t id) const {
    return lo <= id && id <= hi;
  }
};

namespace detail {

template <typename, typename> struct FindInfo;

template <typename T, typename... CHILDREN, typename TARGET>
struct FindInfo<DFSNode<T, CHILDREN...>, TARGET> {
private:
  template <typename C, typename... Cs>
  static constexpr std::pair<bool, TypeInfo>
  searchChildren(std::uint32_t counter) {
    auto result = FindInfo<C, TARGET>::find(counter);
    if (result.first) {
      return result;
    }
    if constexpr (sizeof...(Cs) > 0) {
      auto [_lo, _hi, next] = DFS<C>::compute(counter);
      return searchChildren<Cs...>(next);
    } else {
      return {false, {}};
    }
  }

public:
  static constexpr std::pair<bool, TypeInfo> find(std::uint32_t counter) {
    auto [lo, hi, next] = DFS<DFSNode<T, CHILDREN...>>::compute(counter);

    if constexpr (std::is_same_v<T, TARGET>) {
      return {true, TypeInfo{lo, hi}};
    } else if constexpr (sizeof...(CHILDREN) > 0) {
      return searchChildren<CHILDREN...>(counter + 1);
    } else {
      return {false, {}};
    }
  }
};

} // namespace detail

class Value;
class Integer;
class StringBase;
class Symbol;
class Keyword;
class Constant;
class String;
class Atom;
class Sequence;
class List;
class Vector;
class Hash;
class Invocable;
class BuiltIn;
class FunctionBase;
class Lambda;
class Macro;
class Eval;

using ValueHierarchy =
  detail::DFSNode<Value,
    detail::DFSNode<Integer>,
    detail::DFSNode<StringBase,
      detail::DFSNode<Symbol>,
      detail::DFSNode<Keyword>,
      detail::DFSNode<Constant>,
      detail::DFSNode<String>>,
    detail::DFSNode<Atom>,
    detail::DFSNode<Sequence,
      detail::DFSNode<List>,
      detail::DFSNode<Vector>>,
    detail::DFSNode<Hash>,
    detail::DFSNode<Invocable,
      detail::DFSNode<BuiltIn>,
      detail::DFSNode<FunctionBase,
        detail::DFSNode<Lambda>,
        detail::DFSNode<Macro>>,
      detail::DFSNode<Eval>>>;

template <typename T>
inline constexpr TypeInfo typeInfo = [] {
  auto [found, info] = detail::FindInfo<ValueHierarchy, T>::find(0);
  return found ? info : TypeInfo(~0u, 0u);
}();

class RttiBase {
public:
  explicit RttiBase(std::uint32_t loId) noexcept : loId_{loId} {}

  template <typename T> [[nodiscard]] bool isa() const noexcept {
    return typeInfo<T>.contains(loId_);
  }

  template <typename T> [[nodiscard]] T *dyncast() noexcept {
    return isa<T>() ? static_cast<T *>(this) : nullptr;
  }

  template <typename T> [[nodiscard]] const T *dyncast() const noexcept {
    return isa<T>() ? static_cast<const T *>(this) : nullptr;
  }

protected:
  std::uint32_t loId_;
};

} // namespace mal

#endif // INCLUDE_HIERARCHY
