#include "Env.h" // IWYU pragma: associated
#include "Types.h"

#include <algorithm> // IWYU pragma: keep
#include <cassert>
#include <memory>
#include <ranges>
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
  map_.insert_or_assign(std::move(key), std::move(value));
}

std::vector<EnvBase::Key> ApplyEnv::keys() const {
  std::vector<Key> res;
  res.reserve(mapsSize());
  for (auto &&key : std::views::keys(map_)) {
    res.push_back(key);
  }
  for (auto &&key : capturedEnvKeys()) {
    res.emplace_back(std::move(key));
  }
  return res;
}

ApplyEnv::ApplyEnv(EnvPtr evalEnv, EnvPtr capturedEnv) noexcept
    : EnvBase{std::move(evalEnv)}, capturedEnv_{std::move(capturedEnv)} {
  for (auto &&key : capturedEnvKeys()) {
    filter_.insert(key);
  }
}

void ApplyEnv::insert_or_assign(Key key, ValuePtr value) {
  assert(value);
  registerDebugEval(key, value);
  filter_.insert(key);
  map_.insert_or_assign(std::move(key), std::move(value));
}


ValuePtr ApplyEnv::findLocal(FindLocalKey phk) const {
  if (!filter_.possiblyContains(phk)) {
    return nullptr;
  }
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
