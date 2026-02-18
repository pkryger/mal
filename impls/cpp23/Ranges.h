#ifndef INCLUDE_RANGES_H
#define INCLUDE_RANGES_H

#include <ranges>
#include <version>
#include <concepts>
#include <iterator>

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
struct ChunkPipeable
    : FN,
      std::ranges::range_adaptor_closure<ChunkPipeable<FN>> {
  constexpr explicit ChunkPipeable(FN&& fn): FN{std::move(fn)} {}
};

template <std::ranges::forward_range VIEW>
  requires std::ranges::view<VIEW>
class ChunkView : public std::ranges::view_interface<ChunkView<VIEW>> {
  VIEW base_ = VIEW();
  std::ranges::range_difference_t<VIEW> count_{1};

  class Iterator;

  std::ranges::iterator_t<VIEW> find_next(std::ranges::iterator_t<VIEW> it) {
    std::ranges::advance(it, count_, std::ranges::end(base_));
    return it;
  }

  std::ranges::iterator_t<VIEW> find_prev(std::ranges::iterator_t<VIEW> it) {
    std::ranges::advance(it, -count_, std::ranges::begin(base_));
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

  constexpr Iterator begin() {
    auto begin = std::ranges::begin(base_);
    return Iterator{this, begin, find_next(begin)};
  }

  constexpr Iterator end() {
    auto end = std::ranges::end(base_);
    return Iterator{this, end, end}; }

  constexpr std::ranges::range_difference_t<VIEW> count() const & {
    return count_;
  }
};

template <typename RANGE>
ChunkView(RANGE &&, std::ranges::range_difference_t<RANGE>)
    -> ChunkView<std::views::all_t<RANGE>>;

template <std::ranges::forward_range VIEW>
  requires std::ranges::view<VIEW>
class ChunkView<VIEW>::Iterator {
  friend ChunkView;

  ChunkView *parent_;
  std::ranges::iterator_t<VIEW> current_;
  std::ranges::iterator_t<VIEW> next_;

  explicit constexpr Iterator(ChunkView *parent,
                              std::ranges::iterator_t<VIEW> current,
                              std::ranges::iterator_t<VIEW> next)
      : parent_{parent}, current_{current}, next_{next} {
    assert(parent_);
  }


public:
  using value_type = std::ranges::subrange<std::ranges::iterator_t<VIEW>>;
  using difference_type = std::ranges::range_difference_t<VIEW>;
  using iterator_category = std::input_iterator_tag;
  using iteraotr_concept =
      std::conditional_t<std::ranges::bidirectional_range<VIEW>,
                         std::bidirectional_iterator_tag,
                         std::forward_iterator_tag>;

  Iterator() = default;

  constexpr value_type operator*() const {
    assert (current_ != next_);
    return {current_, next_};
  }

  constexpr Iterator &operator++() {
    assert(current_ != next_);
    current_ = next_;
    next_ = parent_->find_next(next_);
    return *this;
  }

  constexpr Iterator operator++(int) {
    auto tmp = *this;
    ++*this;
    return tmp;
  }

  constexpr Iterator &operator--()
    requires std::ranges::bidirectional_range<VIEW>
  {
    next_ = current_;
    current_ = parent_->find_prev(current_);
    return *this;
  }

  constexpr Iterator operator--(int)
    requires std::ranges::bidirectional_range<VIEW>
  {
    auto tmp = *this;
    --*this;
    return tmp;
  }

  friend constexpr bool operator==(const Iterator &lhs, const Iterator &rhs) {
    return lhs.current_ == rhs.current_;
  }

  friend constexpr bool operator==(const Iterator &lhs,
                                   std::default_sentinel_t) {
    return lhs.current_ == lhs.next_;
  }

};

namespace chunk_fn {
struct ChunkFn : std::ranges::range_adaptor_closure<ChunkFn> {
  template <typename RANGE>
  [[nodiscard]]
  constexpr auto operator()(RANGE &&range,
                            std::ranges::range_difference_t<RANGE> n) const
      noexcept(noexcept(ChunkView{std::forward<RANGE>(range), n}))
          -> decltype(ChunkView{std::forward<RANGE>(range), n}) {
    return ChunkView{std::forward<RANGE>(range), n};
  }

  [[nodiscard]]
  constexpr auto operator()(std::ptrdiff_t n) const
      noexcept {
    return ChunkPipeable{
      [n](auto &&range) noexcept(
          noexcept(ChunkView(std::forward<decltype(range)>(range), n)))
          -> decltype(ChunkView{std::forward<decltype(range)>(range), n}) {
          return ChunkView(std::forward<decltype(range)>(range), n);
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
namespace views {
inline constexpr auto chunk = mal::views::chunk_fn::ChunkFn{};
} // namespace views
} // namespace ranges
} // namespace std
#endif // __cpp_lib_ranges_chunk

#endif // INCLUDE_RANGES_H
