#include "chibicpp/adt/small_vector.hh"

#include "chibicpp/support/mem_alloc.hh"

using namespace chibicpp;

/// Report that min_size doesn't fit into this vector's size type. Throws
/// std::length_error or calls report_fatal_error.
[[noreturn]] static void report_size_overflow(size_t min_size, size_t max_size);

static void report_size_overflow(size_t min_size, size_t max_size) {
  std::string reason = "SmallVector unable to grow. Requested capacity (" +
                       std::to_string(min_size) +
                       ") is larger than maximum value for size type (" +
                       std::to_string(max_size) + ")";

  throw std::length_error(reason);
}

/// Report that this vector is already at maximum capacity. Throws
/// std::length_error or calls report_fatal_error.
[[noreturn]] static void report_at_maximum_capacity(size_t max_size);

static void report_at_maximum_capacity(size_t max_size) {
  std::string reason =
      "SmallVector capacity unable to grow. Already at maximum size " +
      std::to_string(max_size);

  throw std::length_error(reason);
}

// Note: Moving this function into the header may cause performance regression.
template <typename SizeT>
static size_t get_new_capacity(size_t min_size, size_t tsize,
                               size_t old_capacity) {
  constexpr size_t max_size = std::numeric_limits<SizeT>::max();

  // Ensure we can fit the new capacity.
  // This is only going to be applicable when the capacity is 32 bit.
  if (min_size > max_size) report_size_overflow(min_size, max_size);

  // Ensure we can meet the guarantee of space for at least one more element.
  // The above check alone will not catch the case where grow is called with a
  // default min_size of 0, but the current capacity cannot be increased.
  // This is only going to be applicable when the capacity is 32 bit.
  if (old_capacity == max_size) report_at_maximum_capacity(max_size);

  // In theory 2*capacity can overflow if the capacity is 64 bit, but the
  // original capacity would never be large enough for this to be a problem.
  size_t new_capacity = 2 * old_capacity + 1;  // Always grow.

  return std::clamp(new_capacity, min_size, max_size);
}

template <typename SizeT>
void* SmallVectorBase<SizeT>::malloc_for_grow(void* first_el, size_t min_size,
                                              size_t tsize,
                                              size_t& new_capacity) {
  new_capacity = get_new_capacity<SizeT>(min_size, tsize, this->capacity());
  // Even if capacity is not 0 now, if the vector was originally created with
  // capacity 0, it's possible for the malloc to return first_el.
  void* new_elts = chibicpp::safe_malloc(new_capacity * tsize);

  if (new_elts == first_el)
    new_elts = replace_allocation(new_elts, tsize, new_capacity);

  return new_elts;
}

template <typename SizeT>
void SmallVectorBase<SizeT>::grow_pod(void* first_el, size_t min_size,
                                      size_t tsize) {
  size_t new_capacity =
      get_new_capacity<SizeT>(min_size, tsize, this->capacity());
  void* new_elts;

  if (begin_x_ == first_el) {
    new_elts = chibicpp::safe_malloc(new_capacity * tsize);

    if (new_elts == first_el)
      new_elts = replace_allocation(new_elts, tsize, new_capacity);

    // Copy the elements over.  No need to run dtors on PODs.
    memcpy(new_elts, this->begin_x_, size() * tsize);
  } else {
    // If this wasn't grown from the inline copy, grow the allocated space.
    new_elts = chibicpp::safe_realloc(this->begin_x_, new_capacity * tsize);

    if (new_elts == first_el)
      new_elts = replace_allocation(new_elts, tsize, new_capacity, size());
  }

  this->set_allocation_range(new_elts, new_capacity);
}

template <typename SizeT>
void* SmallVectorBase<SizeT>::replace_allocation(void* new_elts, size_t tsize,
                                                 size_t new_capacity,
                                                 size_t vsize) {
  void* new_elts_replace = chibicpp::safe_malloc(new_capacity * tsize);

  if (vsize) memcpy(new_elts_replace, new_elts, vsize * tsize);

  free(new_elts);

  return new_elts_replace;
}