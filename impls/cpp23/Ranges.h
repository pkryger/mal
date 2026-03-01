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

namespace mal {
namespace views {

template <typename FN>
struct Pipeable
    : FN,
      std::ranges::range_adaptor_closure<Pipeable<FN>> {
  constexpr explicit Pipeable(FN &&fn) : FN{std::move(fn)} {}
};
template <typename FN>
Pipeable(FN&&) -> Pipeable<FN>;

template <std::ranges::input_range VIEW>
  requires std::ranges::view<VIEW>
class ChunkView : public std::ranges::view_interface<ChunkView<VIEW>> {
  VIEW base_ = VIEW{};
  std::ranges::range_difference_t<VIEW> count_{1};

  template<bool>
  class iterator;

  template <typename VIEW_ITERATOR>
  auto find_next(VIEW_ITERATOR it) const {
    return std::pair{it,
                     std::ranges::advance(it, count_, std::ranges::end(base_))};
  }

  template <typename VIEW_ITERATOR>
  auto find_prev(VIEW_ITERATOR it,
                 std::ranges::range_difference_t<VIEW> reminder)  const {
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

  constexpr VIEW base() const &
    requires std::copy_constructible<VIEW>
  {
    return base_;
  }

  constexpr VIEW base() && { return std::move(base_); }

  constexpr auto begin() {
    auto begin = std::ranges::begin(base_);
    return iterator<false>{
        this, begin, std::ranges::next(begin, count_, std::ranges::end(base_))};
  }

  constexpr auto begin() const {
    auto begin = std::ranges::begin(base_);
    return iterator<true>{
        this, begin, std::ranges::next(begin, count_, std::ranges::end(base_))};
  }

  constexpr auto end() {
    auto end = std::ranges::end(base_);
    return iterator<false>{this, end, end};
  }

  constexpr auto end() const {
    auto end = std::ranges::end(base_);
    return iterator<true>{this, end, end};
  }

  constexpr std::ranges::range_difference_t<VIEW> count() const & {
    return count_;
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

  const ChunkView *parent_;

  using View = std::conditional_t<CONST, const VIEW, VIEW>;

  std::ranges::iterator_t<View> current_;
  std::ranges::iterator_t<View> next_;
  std::ranges::range_difference_t<View> reminder_{0};

  explicit constexpr iterator(const ChunkView *parent,
                              std::ranges::iterator_t<View> current,
                              std::ranges::iterator_t<View> next)
      : parent_{parent}, current_{current}, next_{next} {
    assert(parent_);
  }

public:
  using value_type = std::ranges::subrange<std::ranges::iterator_t<View>>;
  using difference_type = std::ranges::range_difference_t<View>;
  using iterator_category = std::input_iterator_tag;
  using iterator_concept = std::conditional_t<
      std::ranges::bidirectional_range<View>, std::bidirectional_iterator_tag,
      std::conditional_t<std::ranges::forward_range<View>,
                         std::forward_iterator_tag, std::input_iterator_tag>>;

  iterator() = default;

  constexpr const value_type operator*() const {
    assert (current_ != next_);
    return {current_, next_};
  }

  constexpr iterator &operator++() {
    assert(current_ != next_);
    current_ = next_;
    std::tie(next_, reminder_) = parent_->find_next(next_);
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
    next_ = current_;
    current_ = parent_->find_prev(current_, reminder_);
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
    return lhs.current_ == lhs.next_;
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
    return Pipeable{
        [n](auto &&range) noexcept(
            noexcept(ChunkView{std::forward<decltype(range)>(range), n}))
            -> decltype(ChunkView{std::forward<decltype(range)>(range), n}) {
          return ChunkView{std::forward<decltype(range)>(range), n};
        }};
  }
};

} // namespace chunk_fn

inline constexpr auto Chunk = chunk_fn::ChunkFn{};

} // namespace views
} // namespace mal

#ifndef __cpp_lib_ranges_chunk
namespace std {
namespace ranges {
template <std::ranges::input_range VIEW>
  requires std::ranges::view<VIEW>
using chunk_view = mal::views::ChunkView<VIEW>;
namespace views {
inline constexpr auto chunk = mal::views::chunk_fn::ChunkFn{};
} // namespace views
} // namespace ranges
} // namespace std
#endif // __cpp_lib_ranges_chunk

namespace mal {
namespace views {

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

  constexpr VIEW base() const &
    requires std::copy_constructible<VIEW>
  {
    return base_;
  }

  constexpr VIEW base() && { return std::move(base_); }

  constexpr auto begin() {
    return iterator<false>{this, std::ranges::begin(base_)};
  }

  constexpr auto begin() const {
    return iterator<true>{this, std::ranges::begin(base_)};
  }

  constexpr auto end() {
    return iterator<false>{this, std::ranges::end(base_)};
  }

  constexpr auto end() const {
    return iterator<false>{this, std::ranges::end(base_)};
  }

  constexpr std::ranges::range_difference_t<VIEW> count() const & {
    return count_;
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
  std::ranges::iterator_t<View> current_;
  std::ranges::range_difference_t<View> reminder_{0};

  explicit constexpr iterator(const StrideView *parent,
                              std::ranges::iterator_t<View> current)
      : parent_{parent}, current_{current} {
    assert(parent_);
  }


public:
  using value_type = std::ranges::iterator_t<View>::value_type;
  using difference_type = std::ranges::range_difference_t<View>;
  using iterator_category = std::input_iterator_tag;
  using iterator_concept = std::conditional_t<
      std::ranges::bidirectional_range<View>, std::bidirectional_iterator_tag,
      std::conditional_t<std::ranges::forward_range<View>,
                         std::forward_iterator_tag, std::input_iterator_tag>>;

  iterator() = default;

  constexpr const auto &operator*() const {
    assert(current_ != std::ranges::end(parent_->base()));
    return *current_;
  }

  constexpr auto &operator*()
    requires(!CONST)
  {
    assert(current_ != std::ranges::end(parent_->base()));
    return *current_;
  }

  constexpr iterator &operator++() {
    assert(current_ != std::ranges::end(parent_->base()));
    reminder_ = std::ranges::advance(current_, parent_->count(),
                                     std::ranges::end(parent_->base()));
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
    return Pipeable{
        [n](auto &&range) noexcept(
            noexcept(StrideView{std::forward<decltype(range)>(range), n}))
            -> decltype(StrideView{std::forward<decltype(range)>(range), n}) {
          return StrideView{std::forward<decltype(range)>(range), n};
        }};
  }
};

} // namespace stride_fn

inline constexpr auto Stride = stride_fn::StrideFn{};

} // namespace views
} // namespace mal

#ifndef __cpp_lib_ranges_stride
namespace std {
namespace ranges {
template <std::ranges::input_range VIEW>
  requires std::ranges::view<VIEW>
using stride_view = mal::views::StrideView<VIEW>;
namespace views {
inline constexpr auto stride = mal::views::stride_fn::StrideFn{};
} // namespace views
} // namespace ranges
} // namespace std
#endif // __cpp_lib_ranges_stride


#endif // INCLUDE_RANGES_H
