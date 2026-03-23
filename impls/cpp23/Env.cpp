#include "Env.h" // IWYU pragma: associated
#include "Mal.h"
#include "Types.h"

#include <algorithm> // IWYU pragma: keep
#include <cassert>
#include <cstddef>
#include <functional>
#include <memory>
#include <ranges>
#include <type_traits>
#include <utility>

namespace mal {
ValuePtr EnvBase::find(KeyView key) const {
  if (key == debugEval.asKey()) {
    return debugEval_;
  }
  auto findKey = [&]() {
    if constexpr (detail::IsHashContainer<Map>) {
      return PreHashedKey{key, Hash{}(key)};
    } else {
      return key;
    }
  }();
  for (auto &&env : *this) {
    if (auto value = env.findLocal(findKey)) {
      return value;
    }
  }
  return nullptr;
}

ValuePtr EnvBase::find(const PreHashedKey &phk) const {
  if (phk.key == debugEval.asKey()) {
    return debugEval_;
  }
  const auto &findKey = [&]() {
    if constexpr (detail::IsHashContainer<Map>) {
      return phk;
    } else {
      return phk.key;
    }}();
  for (auto &&env : *this) {
    if (auto value = env.findLocal(findKey)) {
      return value;
    }
  }
  return nullptr;
}

void EnvBase::registerDebugEval(KeyView key, const ValuePtr &value) {
  if (key == debugEval.asKey()) {
    debugEval_ = value;
  }
}

ValuePtr Env::findLocal(FindLocalKey phk) const {
  if (auto item = map_.find(phk); item != map_.end()) {
    return item->second;
  }
  return nullptr;
}

void Env::insert_or_assign(Key key, ValuePtr value) {
  assert(value);
  registerDebugEval(key, value);
  if constexpr (std::is_trivially_copyable_v<Key>) {
    map_.insert_or_assign(key, std::move(value));
  } else {

    map_.insert_or_assign(
        std::move(key), // NOLINT(performance-move-const-arg) - false positive
        std::move(value));
  }
}

void ApplyEnv::insert_or_assign(Key key, ValuePtr value) {
  assert(value);
  registerDebugEval(key, value);
  if constexpr (std::is_trivially_copyable_v<Key>) {
    map_.insert_or_assign(key, std::move(value));
  } else {
    map_.insert_or_assign(
        std::move(key), // NOLINT(performance-move-const-arg) - false positive
        std::move(value));
  }
}

ValuePtr ApplyEnv::findLocal(FindLocalKey phk) const {
  if (auto it = map_.find(phk); it != map_.end()) {
    return it->second;
  }
  return capturedEnv_->find(phk);
}

std::size_t ApplyEnv::mapsSize() const {
  return std::ranges::fold_left(
      *capturedEnv_ |
          std::views::transform([](auto &&env) { return env.mapsSize(); }),
      map_.size(), std::plus<>{});
}

} // namespace mal
