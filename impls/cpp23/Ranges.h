#ifndef INCLUDE_RANGES_H
#define INCLUDE_RANGES_H

#include <cassert>
#include <concepts>
#include <cstddef>
#include <iterator>
#include <ranges>
#include <tuple>
#include <type_traits>
#include <utility>
#include <version> // IWYU pragma: keep

/*
  Apple Clang 17 lacks implementation of
  std::ranges::chunk_view/std::views::chunk.  The malChunkView is a simple (and
  working!) implementation of a chunk view.  Below is a more complete
  implementation of chunk view (based on
  std::ranges::chunk_by_view/std::views::chunk_by) - mostly as a pure
  educational exercise.

template <std::ranges::viewable_range R> auto malChunkView(R &&r, std::size_t n)
{ return std::views::iota(0ul, (std::ranges::size(r) + n - 1) / n) |
         std::views::transform([r = std::forward<R>(r), n](auto &&i) {
           auto slice = r | std::views::drop(i * n) | std::views::take(n);
             return std::ranges::subrange{slice.begin(), slice.end()};
         });
}
*/

namespace mal::views {

namespace detail {
template <typename FN>
struct Pipeable
    : FN,
      std::ranges::range_adaptor_closure<Pipeable<FN>> {
  constexpr explicit Pipeable(FN &&fn) : FN{std::move(fn)} {}
};

template <typename FN> Pipeable(FN &&) -> Pipeable<FN>;

constexpr std::size_t divCeil(std::size_t n, std::size_t denominator) {
  return (n / denominator) + ((n % denominator) != 0u ? 1u : 0u);
}

} // namespace detail

template <std::ranges::input_range VIEW>
  requires std::ranges::view<VIEW>
class ChunkView : public std::ranges::view_interface<ChunkView<VIEW>> {
  VIEW base_ = VIEW{};
  std::ranges::range_difference_t<VIEW> count_{1};

  template<bool>
  class iterator;

  template <typename VIEW_ITERATOR>
  [[nodiscard]] auto findNext(VIEW_ITERATOR it) const {
    auto reminder = [&]() {
      if constexpr (std::ranges::sized_range<VIEW> &&
                    std::is_same_v<std::ranges::iterator_t<VIEW>,
                                   std::ranges::sentinel_t<VIEW>>) {
        return std::ranges::advance(it, count_, std::ranges::end(base_));
      } else {
        std::ranges::advance(it, count_);
        return 0;
      }
    }();
    return std::pair{it, reminder};
  }

  template <typename VIEW_ITERATOR>
  [[nodiscard]] auto
  findPrev(VIEW_ITERATOR it,
           std::ranges::range_difference_t<VIEW> reminder) const {
    std::ranges::advance(it, -(count_ - reminder), std::ranges::begin(base_));
    return it;
  }

public:
  ChunkView()
    requires std::default_initializable<VIEW>
  = default;

  constexpr explicit ChunkView(
      VIEW base, std::ranges::range_difference_t<VIEW> count) noexcept
      : base_{std::move(base)}, count_{count} {
    assert(count > 0);
  }

  [[nodiscard]] constexpr VIEW base() const &
    requires std::copy_constructible<VIEW>
  {
    return base_;
  }

  [[nodiscard]] constexpr VIEW base() && { return std::move(base_); }

  [[nodiscard]] constexpr auto begin() {
    auto current = std::ranges::begin(base_);
    auto [next, reminder] = findNext(std::ranges::begin(base_));
    return iterator<false>{this, current, next, std::ranges::end(base_),
                           reminder};
  }

  [[nodiscard]] constexpr auto begin() const
    requires std::ranges::range<const VIEW>
  {
    auto current = std::ranges::begin(base_);
    auto [next, reminder] = findNext(std::ranges::begin(base_));
    return iterator<true>{this, current, next, std::ranges::end(base_),
                          reminder};
  }

  [[nodiscard]] constexpr auto end() {
    using ReminderType = iterator<false>::difference_type;
    if constexpr (std::ranges::sized_range<VIEW>) {
      auto end = std::ranges::end(base_);
      auto n = std::ranges::size(base_);
      auto count = static_cast<decltype(n)>(count_);
      auto reminder = static_cast<ReminderType>(n - ((n / count) * count));
      return iterator<false>{this, end, end, end, reminder};
    } else {
      return std::default_sentinel;
    }
  }

  [[nodiscard]] constexpr auto end() const
    requires std::ranges::range<const VIEW>
  {
    using ReminderType = iterator<true>::difference_type;
    if constexpr (std::ranges::sized_range<const VIEW>) {
      auto end = std::ranges::end(base_);
      auto n = std::ranges::size(base_);
      auto count = static_cast<decltype(n)>(count_);
      auto reminder = static_cast<ReminderType>(n - ((n / count) * count));
      return iterator<true>{this, end, end, end, reminder};
    } else {
      return std::default_sentinel;
    }
  }

  [[nodiscard]] constexpr std::ranges::range_difference_t<VIEW>
  count() const & {
    return count_;
  }

  [[nodiscard]] std::size_t size()
    requires std::ranges::sized_range<VIEW>
  {
    return detail::divCeil(std::ranges::size(base_), count_);
  }

  [[nodiscard]] std::size_t size() const
    requires std::ranges::sized_range<const VIEW>
  {
    return detail::divCeil(std::ranges::size(base_), count_);
  }
};

template <typename RANGE>
ChunkView(RANGE &&, std::ranges::range_difference_t<RANGE>)
    -> ChunkView<std::views::all_t<RANGE>>;

template <std::ranges::input_range VIEW>
  requires std::ranges::view<VIEW>
template <bool CONST>
class ChunkView<VIEW>::iterator {
  friend ChunkView;

  const ChunkView *parent_{nullptr};

  using View = std::conditional_t<CONST, const VIEW, VIEW>;
  using ViewIterator = std::ranges::iterator_t<View>;
  using ViewSentinel = std::ranges::sentinel_t<View>;
  using ViewDifference = std::ranges::range_difference_t<View>;

  ViewIterator current_;
  ViewIterator next_;
  ViewSentinel end_;
  ViewDifference reminder_{0};

  explicit constexpr iterator(const ChunkView *parent, ViewIterator current,
                              ViewIterator next, ViewSentinel end,
                              ViewDifference reminder)
      : parent_{parent}, current_{current}, next_{next}, end_{end},
        reminder_{reminder} {
    assert(parent_);
  }

public:
  using value_type = std::ranges::subrange<ViewIterator, ViewSentinel>;
  using difference_type = ViewDifference;
  using iterator_category = std::input_iterator_tag;
  using iterator_concept = std::conditional_t<
      std::ranges::bidirectional_range<View>, std::bidirectional_iterator_tag,
      std::conditional_t<std::ranges::forward_range<View>,
                         std::forward_iterator_tag, std::input_iterator_tag>>;

  iterator() = default;

  constexpr const value_type operator*() const
  {
    if constexpr (std::is_same_v<ViewIterator, ViewSentinel>) {
      assert(current_ != next_);
      return {current_, next_};
    } else {
      assert(current_ != end_);
      return {current_, end_, parent_->count()};
    }
    return {current_, next_};
  }


  constexpr iterator &operator++() {
    if constexpr (std::is_same_v<ViewIterator, ViewSentinel>) {
      assert(current_ != next_);
    } else {
      assert(next_ != end_);
    }
    current_ = next_;
    std::tie(next_, reminder_) = parent_->findNext(next_);
    return *this;
  }

  constexpr iterator operator++(int) {
    auto tmp = *this;
    ++*this;
    return tmp;
  }

  constexpr iterator &operator--()
    requires std::ranges::bidirectional_range<VIEW> &&
             std::same_as<ViewIterator, ViewSentinel>
  {
    next_ = current_;
    current_ = parent_->findPrev(current_, reminder_);
    reminder_ = 0;
    return *this;
  }

  constexpr iterator operator--(int)
    requires std::ranges::bidirectional_range<VIEW> &&
             std::same_as<ViewIterator, ViewSentinel>
  {
    auto tmp = *this;
    --*this;
    return tmp;
  }

  friend constexpr bool operator==(const iterator &lhs, const iterator &rhs) {
    return lhs.current_ == rhs.current_;
  }

  friend constexpr bool operator==(const iterator &lhs,
                                   std::default_sentinel_t) {
    if constexpr (std::is_same_v<ViewIterator, ViewSentinel>) {
      return lhs.current_ == lhs.next_;
    } else {
      return lhs.current_ == lhs.end_;
    }
  }

};

namespace chunk_fn {

struct ChunkFn : std::ranges::range_adaptor_closure<ChunkFn> {
  template <std::ranges::input_range RANGE>
  [[nodiscard]]
  constexpr auto operator()(RANGE &&range,
                            std::ranges::range_difference_t<RANGE> n) const
      noexcept(noexcept(ChunkView{std::forward<RANGE>(range), n}))
          -> decltype(ChunkView{std::forward<RANGE>(range), n}) {
    return ChunkView{std::forward<RANGE>(range), n};
  }

  [[nodiscard]]
  constexpr auto operator()(std::ptrdiff_t n) const noexcept {
    return detail::Pipeable{
        [n](auto &&range) noexcept(
            noexcept(ChunkView{std::forward<decltype(range)>(range), n}))
            -> decltype(ChunkView{std::forward<decltype(range)>(range), n}) {
          return ChunkView{std::forward<decltype(range)>(range), n};
        }};
  }
};

} // namespace chunk_fn

inline constexpr auto Chunk = chunk_fn::ChunkFn{};

template <std::ranges::input_range VIEW>
  requires std::ranges::view<VIEW>
class StrideView : public std::ranges::view_interface<StrideView<VIEW>> {
  VIEW base_ = VIEW{};
  std::ranges::range_difference_t<VIEW> count_{0};

  template <bool>
  class iterator;

public:
  StrideView()
    requires std::default_initializable<VIEW>
  = default;

  constexpr explicit StrideView(
      VIEW base, std::ranges::range_difference_t<VIEW> count) noexcept
      : base_{std::move(base)}, count_{count} {
    assert(count > 0);
  }

  [[nodiscard]] constexpr VIEW base() const &
    requires std::copy_constructible<VIEW>
  {
    return base_;
  }

  [[nodiscard]] constexpr VIEW base() && { return std::move(base_); }

  [[nodiscard]] constexpr auto begin() {
    return iterator<false>{this, std::ranges::begin(base_)};
  }

  [[nodiscard]] constexpr auto begin() const
    requires std::ranges::range<const VIEW>
  {
    return iterator<true>{this, std::ranges::begin(base_)};
  }

  [[nodiscard]] constexpr auto end() {
    if constexpr (std::ranges::sized_range<VIEW>) {
      return iterator<false>{this, std::ranges::end(base_)};
    } else {
      return std::default_sentinel;
    }
  }

  [[nodiscard]] constexpr auto end() const
    requires std::ranges::range<const VIEW>
  {
    if constexpr (std::ranges::sized_range<VIEW>) {
      return iterator<true>{this, std::ranges::end(base_)};
    } else {
      return std::default_sentinel;
    }
  }

  [[nodiscard]] constexpr std::ranges::range_difference_t<VIEW>
  count() const & {
    return count_;
  }

  [[nodiscard]] std::size_t size()
    requires std::ranges::sized_range<VIEW>
  {
    return detail::divCeil(std::ranges::size(base_), count_);
  }

  [[nodiscard]] std::size_t size() const
    requires std::ranges::sized_range<const VIEW>
  {
    return detail::divCeil(std::ranges::size(base_), count_);
  }
};

template <typename RANGE>
StrideView(RANGE &&, std::ranges::range_difference_t<RANGE>)
    -> StrideView<std::views::all_t<RANGE>>;

template <std::ranges::input_range VIEW>
  requires std::ranges::view<VIEW>
template <bool CONST>
class StrideView<VIEW>::iterator {
  friend StrideView;

  using View = std::conditional_t<CONST, const VIEW, VIEW>;

  const StrideView *parent_;
  using ViewIterator = std::ranges::iterator_t<View>;

  ViewIterator current_;
  std::ranges::range_difference_t<View> reminder_{0};

  explicit constexpr iterator(const StrideView *parent, ViewIterator current)
      : parent_{parent}, current_{current} {
    assert(parent_);
  }


public:
  using value_type = ViewIterator::value_type;
  using difference_type = std::ranges::range_difference_t<View>;
  using iterator_category = std::input_iterator_tag;
  using iterator_concept = std::conditional_t<
      std::ranges::bidirectional_range<View>, std::bidirectional_iterator_tag,
      std::conditional_t<std::ranges::forward_range<View>,
                         std::forward_iterator_tag, std::input_iterator_tag>>;

  iterator() = default;

  constexpr const auto &operator*() const {
    //assert(current_ != std::ranges::end(parent_->base()));
    return *current_;
  }

  constexpr auto &operator*()
    requires(!CONST)
  {
    //assert(current_ != std::ranges::end(parent_->base()));
    return *current_;
  }

  constexpr iterator &operator++() {
    //assert(current_ != std::ranges::end(parent_->base()));
    if constexpr (std::ranges::sized_range<VIEW>) {
      reminder_ = std::ranges::advance(current_, parent_->count(),
                                       std::ranges::end(parent_->base()));
    } else {
      std::ranges::advance(current_, parent_->count());
    }
    return *this;
  }

  constexpr iterator operator++(int) {
    auto tmp = *this;
    ++*this;
    return tmp;
  }

  constexpr iterator &operator--()
    requires std::ranges::bidirectional_range<VIEW>
  {
    std::ranges::advance(current_, -(parent_->count() - reminder_),
                         std::ranges::begin(parent_->base()));
    reminder_ = 0;
    return *this;
  }

  constexpr iterator operator--(int)
    requires std::ranges::bidirectional_range<VIEW>
  {
    auto tmp = *this;
    --*this;
    return tmp;
  }

  friend constexpr bool operator==(const iterator &lhs, const iterator &rhs) {
    return lhs.current_ == rhs.current_;
  }

  friend constexpr bool operator==(const iterator &lhs,
                                   std::default_sentinel_t) {
    return lhs.current_ == std::ranges::end(lhs.parent_->base());
  }

};

namespace stride_fn {

struct StrideFn : std::ranges::range_adaptor_closure<StrideFn> {
  template <std::ranges::input_range RANGE>
  [[nodiscard]]
  constexpr auto operator()(RANGE &&range,
                            std::ranges::range_difference_t<RANGE> n) const
      noexcept(noexcept(StrideView{std::forward<RANGE>(range), n}))
          -> decltype(StrideView{std::forward<RANGE>(range), n}) {
    return StrideView{std::forward<RANGE>(range), n};
  }

  [[nodiscard]]
  constexpr auto operator()(std::ptrdiff_t n) const noexcept {
    return detail::Pipeable{
        [n](auto &&range) noexcept(
            noexcept(StrideView{std::forward<decltype(range)>(range), n}))
            -> decltype(StrideView{std::forward<decltype(range)>(range), n}) {
          return StrideView{std::forward<decltype(range)>(range), n};
        }};
  }
};

} // namespace stride_fn

inline constexpr auto Stride = stride_fn::StrideFn{};

} // namespace mal::views


#endif // INCLUDE_RANGES_H
