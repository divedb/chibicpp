//===- llvm/ADT/DenseMap.h - Dense probed hash table ------------*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
///
/// \file
/// This file defines the DenseMap class.
///
//===----------------------------------------------------------------------===//

#pragma once

#include <algorithm>
#include <cassert>
#include <cstddef>
#include <cstring>
#include <initializer_list>
#include <iterator>
#include <new>
#include <type_traits>
#include <utility>

#include "chibicpp/adt/dense_map_info.hh"
#include "chibicpp/support/math_extras.hh"
#include "chibicpp/support/mem_alloc.hh"
#include "chibicpp/util/epoch_tracker.hh"

namespace chibicpp {

namespace detail {

/// We extend a pair to allow users to override the bucket type with their own
/// implementation without requiring two members.
template <typename KeyT, typename ValueT>
struct DenseMapPair : public std::pair<KeyT, ValueT> {
  using std::pair<KeyT, ValueT>::pair;

  KeyT& get_first() { return std::pair<KeyT, ValueT>::first; }
  const KeyT& get_first() const { return std::pair<KeyT, ValueT>::first; }
  ValueT& get_second() { return std::pair<KeyT, ValueT>::second; }
  const ValueT& get_second() const { return std::pair<KeyT, ValueT>::second; }
};

}  // end namespace detail

template <typename KeyT, typename ValueT,
          typename KeyInfoT = DenseMapInfo<KeyT>,
          typename Bucket = chibicpp::detail::DenseMapPair<KeyT, ValueT>,
          bool IsConst = false>
class DenseMapIterator;

template <typename DerivedT, typename KeyT, typename ValueT, typename KeyInfoT,
          typename BucketT>
class DenseMapBase : public DebugEpochBase {
  template <typename T>
  using const_arg_type_t = typename const_pointer_or_const_ref<T>::type;

 public:
  using size_type = unsigned;
  using key_type = KeyT;
  using mapped_type = ValueT;
  using value_type = BucketT;

  using iterator = DenseMapIterator<KeyT, ValueT, KeyInfoT, BucketT>;
  using const_iterator =
      DenseMapIterator<KeyT, ValueT, KeyInfoT, BucketT, true>;

  inline iterator begin() {
    // When the map is empty, avoid the overhead of advancing/retreating past
    // empty buckets.
    if (empty()) {
      return end();
    }

    if (should_reverse_iterate<KeyT>()) {
      return make_iterator(get_buckets_end() - 1, get_buckets(), *this);
    }

    return make_iterator(get_buckets(), get_buckets_end(), *this);
  }

  inline iterator end() {
    return make_iterator(get_euckets_end(), get_buckets_end(), *this, true);
  }

  inline const_iterator begin() const {
    if (empty()) return end();

    if (should_reverse_iterate<KeyT>())
      return make_const_iterator(get_buckets_end() - 1, get_buckets(), *this);

    return make_const_iterator(get_buckets(), get_buckets_end(), *this);
  }

  inline const_iterator end() const {
    return make_const_iterator(get_buckets_end(), get_buckets_end(), *this,
                               true);
  }

  [[nodiscard]] bool empty() const { return get_num_entries() == 0; }
  unsigned size() const { return get_num_entries(); }

  /// Grow the densemap so that it can contain at least \p num_entries items
  /// before resizing again.
  void reserve(size_type num_entries) {
    auto num_buckets = get_min_bucket_to_reserve_for_entries(num_entries);
    increment_epoch();

    if (num_buckets > get_num_buckets()) grow(num_buckets);
  }

  void clear() {
    increment_epoch();
    if (get_num_entries() == 0 && get_num_tombstones() == 0) return;

    // If the capacity of the array is huge, and the # elements used is small,
    // shrink the array.
    if (get_num_entries() * 4 < get_num_buckets() && get_num_buckets() > 64) {
      shrink_and_clear();
      return;
    }

    const KeyT kEmptyKey = get_empty_key();

    if (std::is_trivially_destructible<ValueT>::value) {
      // Use a simpler loop when values don't need destruction.
      for (BucketT *p = get_buckets(), *e = get_buckets_end(); p != e; ++p)
        p->get_first() = kEmptyKey;
    } else {
      const KeyT kTombstoneKey = get_tombstone_key();
      unsigned num_entries = get_num_entries();

      for (BucketT *p = get_buckets(), *e = get_buckets_end(); p != e; ++p) {
        if (!KeyInfoT::is_equal(p->get_first(), kEmptyKey)) {
          if (!KeyInfoT::is_equal(p->get_first(), kTombstoneKey)) {
            p->get_second().~ValueT();
            --num_entries;
          }

          p->get_first() = kEmptyKey;
        }
      }

      assert(num_entries == 0 && "Node count imbalance!");
      (void)num_entries;
    }

    set_num_entries(0);
    set_num_tombstones(0);
  }

  /// Return true if the specified key is in the map, false otherwise.
  bool contains(const_arg_type_t<KeyT> val) const {
    return do_find(val) != nullptr;
  }

  /// Return 1 if the specified key is in the map, 0 otherwise.
  size_type count(const_arg_type_t<KeyT> val) const {
    return contains(val) ? 1 : 0;
  }

  iterator find(const_arg_type_t<KeyT> val) {
    if (BucketT* bucket = do_find(val))
      return make_iterator(
          bucket,
          should_reverse_iterate<KeyT>() ? get_buckets() : get_buckets_end(),
          *this, true);

    return end();
  }

  const_iterator find(const_arg_type_t<KeyT> val) const {
    if (const BucketT* bucket = do_find(val))
      return make_const_iterator(
          bucket,
          should_reverse_iterate<KeyT>() ? get_buckets() : get_buckets_end(),
          *this, true);

    return end();
  }

  /// Alternate version of find() which allows a different, and possibly
  /// less expensive, key type.
  /// The DenseMapInfo is responsible for supplying methods
  /// getHashValue(LookupKeyT) and isEqual(LookupKeyT, KeyT) for each key
  /// type used.
  template <typename LookupKeyT>
  iterator find_as(const LookupKeyT& val) {
    if (BucketT* bucket = do_find(val))
      return make_iterator(
          bucket,
          should_reverse_iterate<KeyT>() ? get_buckets() : get_buckets_end(),
          *this, true);

    return end();
  }

  template <typename LookupKeyT>
  const_iterator find_as(const LookupKeyT& val) const {
    if (const BucketT* bucket = do_find(val))
      return make_const_iterator(
          bucket,
          should_reverse_iterate<KeyT>() ? get_buckets() : get_buckets_end(),
          *this, true);

    return end();
  }

  /// Return the entry for the specified key, or a default
  /// constructed value if no such entry exists.
  ValueT lookup(const_arg_type_t<KeyT> val) const {
    if (const BucketT* bucket = do_find(val)) return bucket->get_second();

    return ValueT();
  }

  /// Return the entry for the specified key, or abort if no such
  /// entry exists.
  const ValueT& at(const_arg_type_t<KeyT> val) const {
    auto iter = this->find(std::move(val));

    assert(iter != this->end() && "DenseMap::at failed due to a missing key");

    return iter->second;
  }

  // Inserts key,value pair into the map if the key isn't already in the map.
  // If the key is already in the map, it returns false and doesn't update the
  // value.
  std::pair<iterator, bool> insert(const std::pair<KeyT, ValueT>& kv) {
    return try_emplace(kv.first, kv.second);
  }

  // Inserts key,value pair into the map if the key isn't already in the map.
  // If the key is already in the map, it returns false and doesn't update the
  // value.
  std::pair<iterator, bool> insert(std::pair<KeyT, ValueT>&& kv) {
    return try_emplace(std::move(kv.first), std::move(kv.second));
  }

  // Inserts key,value pair into the map if the key isn't already in the map.
  // The value is constructed in-place if the key is not in the map, otherwise
  // it is not moved.
  template <typename... Ts>
  std::pair<iterator, bool> try_emplace(KeyT&& key, Ts&&... args) {
    BucketT* the_bucket;

    if (lookup_bucket_for(key, the_bucket))
      return std::make_pair(
          make_iterator(the_bucket,
                        should_reverse_iterate<KeyT>() ? get_buckets()
                                                       : get_buckets_end(),
                        *this, true),
          false);  // Already in map.

    // Otherwise, insert the new element.
    the_bucket = insert_into_bucket(the_bucket, std::move(key),
                                    std::forward<Ts>(args)...);
    return std::make_pair(
        make_iterator(
            the_bucket,
            should_reverse_iterate<KeyT>() ? get_buckets() : get_buckets_end(),
            *this, true),
        true);
  }

  // Inserts key,value pair into the map if the key isn't already in the map.
  // The value is constructed in-place if the key is not in the map, otherwise
  // it is not moved.
  template <typename... Ts>
  std::pair<iterator, bool> try_emplace(const KeyT& key, Ts&&... args) {
    BucketT* the_bucket;

    if (lookup_bucket_for(key, the_bucket))
      return std::make_pair(
          make_iterator(the_bucket,
                        should_reverse_iterate<KeyT>() ? get_buckets()
                                                       : get_buckets_end(),
                        *this, true),
          false);  // Already in map.

    // Otherwise, insert the new element.
    the_bucket = insert_into_bucket(the_bucket, Key, std::forward<Ts>(args)...);

    return std::make_pair(
        make_iterator(
            the_bucket,
            should_reverse_iterate<KeyT>() ? get_buckets() : get_buckets_end(),
            *this, true),
        true);
  }

  /// Alternate version of insert() which allows a different, and possibly
  /// less expensive, key type.
  /// The DenseMapInfo is responsible for supplying methods
  /// getHashValue(LookupKeyT) and isEqual(LookupKeyT, KeyT) for each key
  /// type used.
  template <typename LookupKeyT>
  std::pair<iterator, bool> insert_as(std::pair<KeyT, ValueT>&& kv,
                                      const LookupKeyT& val) {
    BucketT* the_bucket;

    if (lookup_bucket_for(val, the_bucket))
      return std::make_pair(
          make_iterator(the_bucket,
                        should_reverse_iterate<KeyT>() ? get_buckets()
                                                       : get_buckets_end(),
                        *this, true),
          false);  // Already in map.

    // Otherwise, insert the new element.
    the_bucket = insert_into_bucket_with_lookup(the_bucket, std::move(kv.first),
                                                std::move(kv.second), val);

    return std::make_pair(
        make_iterator(
            the_bucket,
            should_reverse_iterate<KeyT>() ? get_buckets() : get_buckets_end(),
            *this, true),
        true);
  }

  /// insert - Range insertion of pairs.
  template <typename InputIt>
  void insert(InputIt i, InputIt e) {
    for (; i != e; ++i) insert(*i);
  }

  template <typename V>
  std::pair<iterator, bool> insert_or_assign(const KeyT& key, V&& val) {
    auto ret = try_emplace(key, std::forward<V>(val));
    if (!ret.second) ret.first->second = std::forward<V>(val);

    return ret;
  }

  template <typename V>
  std::pair<iterator, bool> insert_or_assign(KeyT&& key, V&& val) {
    auto ret = try_emplace(std::move(key), std::forward<V>(val));
    if (!ret.second) ret.first->second = std::forward<V>(val);

    return ret;
  }

  /// Returns the value associated to the key in the map if it exists. If it
  /// does not exist, emplace a default value for the key and returns a
  /// reference to the newly created value.
  ValueT& get_or_insert_default(KeyT&& key) {
    return try_emplace(key).first->second;
  }

  /// Returns the value associated to the key in the map if it exists. If it
  /// does not exist, emplace a default value for the key and returns a
  /// reference to the newly created value.
  ValueT& get_or_insert_default(const KeyT& key) {
    return try_emplace(key).first->second;
  }

  bool erase(const KeyT& val) {
    BucketT* the_bucket = do_find(val);

    if (!the_bucket) return false;  // not in map.

    the_bucket->get_second().~ValueT();
    the_bucket->get_first() = get_tombstone_key();

    decrement_num_entries();
    increment_num_tombstones();

    return true;
  }

  void erase(iterator i) {
    BucketT* the_bucket = &*i;

    the_bucket->get_second().~ValueT();
    the_bucket->get_first() = get_tombstone_key();

    decrement_num_entries();
    increment_num_tombstones();
  }

  value_type& find_and_construct(const KeyT& key) {
    BucketT* the_bucket;

    if (lookup_bucket_for(key, the_bucket)) return *the_bucket;

    return *insert_into_bucket(the_bucket, key);
  }

  ValueT& operator[](const KeyT& key) { return find_and_construct(key).second; }

  value_type& FindAndConstruct(KeyT&& key) {
    BucketT* the_bucket;

    if (lookup_bucket_for(key, the_bucket)) return *the_bucket;

    return *insert_into_bucket(the_bucket, std::move(key));
  }

  ValueT& operator[](KeyT&& key) {
    return find_and_construct(std::move(key)).second;
  }

  /// Return true if the specified pointer points
  /// somewhere into the DenseMap's array of buckets (i.e. either to a key or
  /// value in the DenseMap).
  bool is_pointer_into_buckets_array(const void* ptr) const {
    return ptr >= get_buckets() && ptr < get_buckets_end();
  }

  /// Return an opaque pointer into the buckets
  /// array.  In conjunction with the previous method, this can be used to
  /// determine whether an insertion caused the DenseMap to reallocate.
  const void* get_pointer_into_buckets_array() const { return get_buckets(); }

 protected:
  DenseMapBase() = default;

  void destroy_all() {
    if (get_num_buckets() == 0)  // Nothing to do.
      return;

    const KeyT kEmptyKey = get_empty_key();
    const KeyT kTombstoneKey = get_tombstone_key();

    for (BucketT *p = get_buckets(), *e = get_buckets_end(); p != e; ++p) {
      if (!KeyInfoT::is_equal(p->get_first(), kEmptyKey) &&
          !KeyInfoT::is_equal(p->get_first(), kTombstoneKey))
        P->get_second().~ValueT();
      P->get_first().~KeyT();
    }
  }

  void init_empty() {
    set_num_entries(0);
    set_num_tombstones(0);

    assert((get_num_buckets() & (get_num_buckets() - 1)) == 0 &&
           "# initial buckets must be a power of two!");

    const KeyT kEmptyKey = get_empty_key();

    for (BucketT *b = get_buckets(), *e = get_buckets_end(); b != e; ++b)
      ::new (&b->get_first()) KeyT(kEmptyKey);
  }

  /// Returns the number of buckets to allocate to ensure that the DenseMap can
  /// accommodate \p num_entries without need to grow().
  unsigned get_min_bucket_to_reserve_for_entries(unsigned num_entries) {
    // Ensure that "num_entries * 4 < NumBuckets * 3"
    if (num_entries == 0) return 0;
    // +1 is required because of the strict equality.
    // For example if NumEntries is 48, we need to return 128.
    return next_power_of_2(num_entries * 4 / 3 + 1);
  }

  void move_from_old_buckets(BucketT* old_buckets_begin,
                             BucketT* old_buckets_end) {
    init_empty();

    // Insert all the old elements.
    const KeyT kEmptyKey = get_empty_key();
    const KeyT kTombstoneKey = get_tombstone_key();

    for (BucketT *b = old_buckets_begin, *e = old_buckets_end; b != e; ++b) {
      if (!KeyInfoT::is_equal(b->get_first(), kEmptyKey) &&
          !KeyInfoT::is_equal(b->get_first(), kTombstoneKey)) {
        // Insert the key/value into the new table.
        BucketT* dest_bucket;
        bool found_val = lookup_bucket_for(b->get_first(), dest_bucket);
        (void)found_val;  // silence warning.

        assert(!found_val && "Key already in new map?");

        dest_bucket->get_first() = std::move(b->get_first());
        ::new (&dest_bucket->get_second()) ValueT(std::move(b->get_second()));
        increment_num_entries();

        // Free the value.
        b->get_second().~ValueT();
      }

      b->get_first().~KeyT();
    }
  }

  template <typename OtherBaseT>
  void copy_from(
      const DenseMapBase<OtherBaseT, KeyT, ValueT, KeyInfoT, BucketT>& other) {
    assert(&other != this);
    assert(get_num_buckets() == other.get_num_buckets());

    set_num_entries(other.get_num_entries());
    set_num_tombstones(other.get_num_tombstones());

    if (std::is_trivially_copyable<KeyT>::value &&
        std::is_trivially_copyable<ValueT>::value)
      memcpy(reinterpret_cast<void*>(get_buckets()), other.get_buckets(),
             get_num_buckets() * sizeof(BucketT));
    else
      for (size_t i = 0; i < get_num_buckets(); ++i) {
        ::new (&get_buckets()[i].get_first())
            KeyT(other.get_buckets()[i].get_first());
        if (!KeyInfoT::is_equal(get_buckets()[i].get_first(),
                                get_empty_key()) &&
            !KeyInfoT::is_equal(get_buckets()[i].get_first(),
                                get_tombstone_key()))
          ::new (&get_buckets()[i].get_second())
              ValueT(other.get_buckets()[i].get_second());
      }
  }

  static unsigned get_hash_value(const KeyT& val) {
    return KeyInfoT::get_hash_value(val);
  }

  template <typename LookupKeyT>
  static unsigned get_hash_value(const LookupKeyT& val) {
    return KeyInfoT::get_hash_value(val);
  }

  static const KeyT get_empty_key() {
    static_assert(std::is_base_of<DenseMapBase, DerivedT>::value,
                  "Must pass the derived type to this template!");

    return KeyInfoT::get_empty_key();
  }

  static const KeyT get_tombstone_key() {
    return KeyInfoT::get_tombstone_key();
  }

 private:
  iterator make_iterator(BucketT* p, BucketT* e, DebugEpochBase& epoch,
                         bool no_advance = false) {
    if (should_reverse_iterate<KeyT>()) {
      BucketT* b = p == get_buckets_end() ? get_buckets() : p + 1;
      return iterator(b, e, epoch, no_advance);
    }

    return iterator(p, e, epoch, no_advance);
  }

  const_iterator make_const_iterator(const BucketT* P, const BucketT* E,
                                     const DebugEpochBase& Epoch,
                                     const bool NoAdvance = false) const {
    if (should_reverse_iterate<KeyT>()) {
      const BucketT* B = P == getBucketsEnd() ? getBuckets() : P + 1;
      return const_iterator(B, E, Epoch, NoAdvance);
    }
    return const_iterator(P, E, Epoch, NoAdvance);
  }

  unsigned get_num_entries() const {
    return static_cast<const DerivedT*>(this)->get_num_entries();
  }

  void set_num_entries(unsigned Num) {
    static_cast<DerivedT*>(this)->set_num_entries(Num);
  }

  void incrementNumEntries() { setNumEntries(getNumEntries() + 1); }

  void decrementNumEntries() { setNumEntries(getNumEntries() - 1); }

  unsigned get_num_tombstones() const {
    return static_cast<const DerivedT*>(this)->get_num_tombstones();
  }

  void set_num_tombstones(unsigned Num) {
    static_cast<DerivedT*>(this)->set_num_tombstones(Num);
  }

  void increment_num_tombstones() {
    set_num_tombstones(get_num_tombstones() + 1);
  }

  void decrement_num_tombstones() {
    set_num_tombstones(get_num_tombstones() - 1);
  }

  const BucketT* get_buckets() const {
    return static_cast<const DerivedT*>(this)->get_buckets();
  }

  BucketT* get_buckets() { return static_cast<DerivedT*>(this)->get_buckets(); }

  unsigned get_num_buckets() const {
    return static_cast<const DerivedT*>(this)->get_num_buckets();
  }

  BucketT* get_buckets_end() { return get_buckets() + get_num_buckets(); }

  const BucketT* get_buckets_end() const {
    return get_buckets() + get_num_buckets();
  }

  void grow(unsigned at_least) { static_cast<DerivedT*>(this)->grow(at_least); }

  void shrink_and_clear() { static_cast<DerivedT*>(this)->shrink_and_clear(); }

  template <typename KeyArg, typename... ValueArgs>
  BucketT* insert_into_bucket(BucketT* the_bucket, KeyArg&& key,
                              ValueArgs&&... values) {
    the_bucket = insert_into_bucket_impl(key, key, the_bucket);

    the_bucket->get_first() = std::forward<KeyArg>(key);
    ::new (&the_bucket->get_second())
        ValueT(std::forward<ValueArgs>(values)...);

    return the_bucket;
  }

  template <typename LookupKeyT>
  BucketT* insert_into_bucket_with_lookup(BucketT* the_bucket, KeyT&& key,
                                          ValueT&& value, LookupKeyT& lookup) {
    the_bucket = insert_into_bucket_impl(key, lookup, the_bucket);
    the_bucket->get_first() = std::move(key);
    ::new (&the_bucket->get_second()) ValueT(std::move(value));

    return the_bucket;
  }

  template <typename LookupKeyT>
  BucketT* insert_into_bucket_impl(const KeyT& key, const LookupKeyT& lookup,
                                   BucketT* the_bucket) {
    increment_epoch();

    // If the load of the hash table is more than 3/4, or if fewer than 1/8 of
    // the buckets are empty (meaning that many are filled with tombstones),
    // grow the table.
    //
    // The later case is tricky.  For example, if we had one empty bucket with
    // tons of tombstones, failing lookups (e.g. for insertion) would have to
    // probe almost the entire table until it found the empty bucket.  If the
    // table completely filled with tombstones, no lookup would ever succeed,
    // causing infinite loops in lookup.
    unsigned new_num_entries = get_num_entries() + 1;
    unsigned num_buckets = get_num_buckets();

    if (new_num_entries * 4 >= num_buckets * 3) [[unlikely]] {
      this->grow(num_buckets * 2);
      lookup_bucket_for(lookup, the_bucket);
      num_buckets = get_num_buckets();
    } else if (num_buckets - (new_num_entries + get_num_tombstones()) <=
               num_buckets / 8) [[unlikely]] {
      this->grow(num_buckets);
      lookup_bucket_for(lookup, the_bucket);
    }

    assert(the_bucket);

    // Only update the state after we've grown our bucket space appropriately
    // so that when growing buckets we have self-consistent entry count.
    increment_num_entries();

    // If we are writing over a tombstone, remember this.
    const KeyT kEmptyKey = get_empty_key();

    if (!KeyInfoT::is_equal(the_bucket->get_first(), kEmptyKey))
      decrement_num_tombstones();

    return the_bucket;
  }

  template <typename LookupKeyT>
  BucketT* do_find(const LookupKeyT& val) {
    BucketT* buckets_ptr = get_buckets();
    const unsigned num_buckets = get_num_buckets();

    if (num_buckets == 0) return nullptr;

    const KeyT kEmptyKey = get_empty_key();
    unsigned bucket_no = get_hash_value(val) & (num_buckets - 1);
    unsigned probe_amt = 1;

    while (true) {
      BucketT* bucket = buckets_ptr + bucket_no;
      if (KeyInfoT::is_equal(val, bucket->get_first())) [[likely]]
        return bucket;

      if (KeyInfoT::is_equal(bucket->get_first(), kEmptyKey)) [[likely]]
        return nullptr;

      // Otherwise, it's a hash collision or a tombstone, continue quadratic
      // probing.
      bucket_no += probe_amt++;
      bucket_no &= num_buckets - 1;
    }
  }

  template <typename LookupKeyT>
  const BucketT* do_find(const LookupKeyT& val) const {
    return const_cast<DenseMapBase*>(this)->do_find(val);  // NOLINT
  }

  /// Lookup the appropriate bucket for Val, returning it in
  /// FoundBucket.  If the bucket contains the key and a value, this returns
  /// true, otherwise it returns a bucket with an empty marker or tombstone and
  /// returns false.
  template <typename LookupKeyT>
  bool lookup_bucket_for(const LookupKeyT& val,
                         const BucketT*& found_bucket) const {
    const BucketT* buckets_ptr = get_buckets();
    const unsigned num_buckets = get_num_buckets();

    if (num_buckets == 0) {
      found_bucket = nullptr;

      return false;
    }

    // Keep track of whether we find a tombstone while probing.
    const BucketT* found_tombstone = nullptr;
    const KeyT kEmptyKey = get_empty_key();
    const KeyT kTombstoneKey = get_tombstone_key();

    assert(!KeyInfoT::is_equal(val, kEmptyKey) &&
           !KeyInfoT::is_equal(val, kTombstoneKey) &&
           "Empty/Tombstone value shouldn't be inserted into map!");

    unsigned bucket_no = get_hash_value(val) & (num_buckets - 1);
    unsigned probe_amt = 1;

    while (true) {
      const BucketT* this_bucket = buckets_ptr + bucket_no;
      // Found Val's bucket?  If so, return it.
      if (KeyInfoT::is_equal(val, this_bucket->get_first())) [[likely]] {
        found_bucket = this_bucket;

        return true;
      }

      // If we found an empty bucket, the key doesn't exist in the set.
      // Insert it and return the default value.
      if (KeyInfoT::is_equal(this_bucket->get_first(), kEmptyKey)) [[likely]] {
        // If we've already seen a tombstone while probing, fill it in instead
        // of the empty bucket we eventually probed to.
        found_bucket = found_tombstone ? found_tombstone : this_bucket;

        return false;
      }

      // If this is a tombstone, remember it.  If Val ends up not in the map, we
      // prefer to return it than something that would require more probing.
      if (KeyInfoT::is_equal(ThisBucket->get_first(), kTombstoneKey) &&
          !found_tombstone)
        found_tombstone = this_bucket;  // Remember the first tombstone found.

      // Otherwise, it's a hash collision or a tombstone, continue quadratic
      // probing.
      bucket_no += probe_amt++;
      bucket_no &= (num_buckets - 1);
    }
  }

  template <typename LookupKeyT>
  bool lookup_bucket_for(const LookupKeyT& val, BucketT*& found_bucket) {
    const BucketT* const_found_bucket;

    bool result = const_cast<const DenseMapBase*>(this)->lookup_bucket_for(
        val, const_found_bucket);
    found_bucket = const_cast<BucketT*>(const_found_bucket);

    return result;
  }

 public:
  /// Return the approximate size (in bytes) of the actual map.
  /// This is just the raw memory used by DenseMap.
  /// If entries are pointers to objects, the size of the referenced objects
  /// are not included.
  size_t get_memory_size() const { return get_num_buckets() * sizeof(BucketT); }
};

/// Equality comparison for DenseMap.
///
/// Iterates over elements of lhs confirming that each (key, value) pair in lhs
/// is also in rhs, and that no additional pairs are in rhs.
/// Equivalent to N calls to rhs.find and N value comparisons. Amortized
/// complexity is linear, worst case is O(N^2) (if every hash collides).
template <typename DerivedT, typename KeyT, typename ValueT, typename KeyInfoT,
          typename BucketT>
bool operator==(
    const DenseMapBase<DerivedT, KeyT, ValueT, KeyInfoT, BucketT>& lhs,
    const DenseMapBase<DerivedT, KeyT, ValueT, KeyInfoT, BucketT>& rhs) {
  if (lhs.size() != rhs.size()) return false;

  for (auto& kv : lhs) {
    auto i = rhs.find(kv.first);

    if (i == rhs.end() || i->second != kv.second) return false;
  }

  return true;
}

/// Inequality comparison for DenseMap.
///
/// Equivalent to !(lhs == rhs). See operator== for performance notes.
template <typename DerivedT, typename KeyT, typename ValueT, typename KeyInfoT,
          typename BucketT>
bool operator!=(
    const DenseMapBase<DerivedT, KeyT, ValueT, KeyInfoT, BucketT>& lhs,
    const DenseMapBase<DerivedT, KeyT, ValueT, KeyInfoT, BucketT>& rhs) {
  return !(lhs == rhs);
}

template <typename KeyT, typename ValueT,
          typename KeyInfoT = DenseMapInfo<KeyT>,
          typename BucketT = chibicpp::detail::DenseMapPair<KeyT, ValueT>>
class DenseMap : public DenseMapBase<DenseMap<KeyT, ValueT, KeyInfoT, BucketT>,
                                     KeyT, ValueT, KeyInfoT, BucketT> {
  friend class DenseMapBase<DenseMap, KeyT, ValueT, KeyInfoT, BucketT>;

  /// Lift some types from the dependent base class into this class for
  /// simplicity of referring to them.
  using BaseT = DenseMapBase<DenseMap, KeyT, ValueT, KeyInfoT, BucketT>;

  BucketT* buckets_;
  unsigned num_entries_;
  unsigned num_tombstones_;
  unsigned num_buckets_;

 public:
  /// Create a DenseMap with an optional \p initial_reserve that guarantee that
  /// this number of elements can be inserted in the map without grow()
  explicit DenseMap(unsigned initial_reserve = 0) { init(initial_reserve); }

  DenseMap(const DenseMap& other) : BaseT() {
    init(0);
    copy_from(other);
  }

  DenseMap(DenseMap&& other) : BaseT() {
    init(0);
    swap(other);
  }

  template <typename InputIt>
  DenseMap(const InputIt& i, const InputIt& e) {
    init(std::distance(i, e));
    this->insert(i, e);
  }

  DenseMap(std::initializer_list<typename BaseT::value_type> vals) {
    init(vals.size());
    this->insert(vals.begin(), vals.end());
  }

  ~DenseMap() {
    this->destroy_all();
    deallocate_buffer(buckets_, sizeof(BucketT) * num_buckets_,
                      alignof(BucketT));
  }

  void swap(DenseMap& rhs) {
    this->increment_epoch();
    rhs.increment_epoch();
    std::swap(buckets_, rhs.buckets_);
    std::swap(num_entries_, rhs.num_entries_);
    std::swap(num_tombstones_, rhs.num_tombstones_);
    std::swap(num_buckets_, rhs.num_buckets_);
  }

  DenseMap& operator=(const DenseMap& other) {
    if (&other != this) copy_from(other);

    return *this;
  }

  DenseMap& operator=(DenseMap&& other) {
    this->destroy_all();
    deallocate_buffer(buckets_, sizeof(BucketT) * num_buckets_,
                      alignof(BucketT));
    init(0);
    swap(other);

    return *this;
  }

  void copy_from(const DenseMap& other) {
    this->destroy_all();
    deallocate_buffer(buckets_, sizeof(BucketT) * num_buckets_,
                      alignof(BucketT));

    if (allocate_buckets(other.num_buckets_)) {
      this->BaseT::copy_from(other);
    } else {
      num_entries_ = 0;
      num_tombstones_ = 0;
    }
  }

  void init(unsigned init_num_entries) {
    // Compute the number of buckets required.
    auto init_buckets =
        BaseT::get_min_bucket_to_reserve_for_entries(init_num_entries);

    if (allocate_buckets(init_buckets)) {
      this->BaseT::init_empty();
    } else {
      num_entries_ = 0;
      num_tombstones_ = 0;
    }
  }

  void grow(unsigned at_least) {
    unsigned old_num_buckets = num_buckets_;
    BucketT* old_buckets = buckets_;

    allocate_buckets(std::max<unsigned>(
        64, static_cast<unsigned>(next_power_of_2(at_least - 1))));

    assert(buckets_);

    if (!old_buckets) {
      this->BaseT::init_empty();

      return;
    }

    this->move_from_old_buckets(old_buckets, old_buckets + old_num_buckets);

    // Free the old table.
    deallocate_buffer(old_buckets, sizeof(BucketT) * old_num_buckets,
                      alignof(BucketT));
  }

  void shrink_and_clear() {
    unsigned OldNumBuckets = NumBuckets;
    unsigned OldNumEntries = NumEntries;
    this->destroyAll();

    // Reduce the number of buckets.
    unsigned NewNumBuckets = 0;
    if (OldNumEntries)
      NewNumBuckets = std::max(64, 1 << (Log2_32_Ceil(OldNumEntries) + 1));
    if (NewNumBuckets == NumBuckets) {
      this->BaseT::initEmpty();
      return;
    }

    deallocate_buffer(Buckets, sizeof(BucketT) * OldNumBuckets,
                      alignof(BucketT));
    init(NewNumBuckets);
  }

 private:
  unsigned get_num_entries() const { return num_entries_; }
  void set_num_entries(unsigned num) { num_entries_ = num_; }
  unsigned get_num_tombstones() const { return num_tombstones_; }
  void set_num_tombstones(unsigned num) { num_tombstones_ = num; }
  BucketT* get_buckets() const { return buckets_; }
  unsigned get_num_buckets() const { return num_buckets_; }
  bool allocate_buckets(unsigned num) {
    num_buckets_ = num;

    if (num_buckets_ == 0) {
      buckets_ = nullptr;

      return false;
    }

    buckets_ = static_cast<BucketT*>(
        allocate_buffer(sizeof(BucketT) * num_buckets_, alignof(BucketT)));

    return true;
  }
};

template <typename KeyT, typename ValueT, unsigned InlineBuckets = 4,
          typename KeyInfoT = DenseMapInfo<KeyT>,
          typename BucketT = chibicpp::detail::DenseMapPair<KeyT, ValueT>>
class SmallDenseMap
    : public DenseMapBase<
          SmallDenseMap<KeyT, ValueT, InlineBuckets, KeyInfoT, BucketT>, KeyT,
          ValueT, KeyInfoT, BucketT> {
  friend class DenseMapBase<SmallDenseMap, KeyT, ValueT, KeyInfoT, BucketT>;

  // Lift some types from the dependent base class into this class for
  // simplicity of referring to them.
  using BaseT = DenseMapBase<SmallDenseMap, KeyT, ValueT, KeyInfoT, BucketT>;

  static_assert(isPowerOf2_64(InlineBuckets),
                "InlineBuckets must be a power of 2.");

  unsigned Small : 1;
  unsigned NumEntries : 31;
  unsigned NumTombstones;

  struct LargeRep {
    BucketT* Buckets;
    unsigned NumBuckets;
  };

  /// A "union" of an inline bucket array and the struct representing
  /// a large bucket. This union will be discriminated by the 'Small' bit.
  AlignedCharArrayUnion<BucketT[InlineBuckets], LargeRep> storage;

 public:
  explicit SmallDenseMap(unsigned NumInitBuckets = 0) {
    if (NumInitBuckets > InlineBuckets)
      NumInitBuckets = llvm::bit_ceil(NumInitBuckets);
    init(NumInitBuckets);
  }

  SmallDenseMap(const SmallDenseMap& other) : BaseT() {
    init(0);
    copyFrom(other);
  }

  SmallDenseMap(SmallDenseMap&& other) : BaseT() {
    init(0);
    swap(other);
  }

  template <typename InputIt>
  SmallDenseMap(const InputIt& I, const InputIt& E) {
    init(NextPowerOf2(std::distance(I, E)));
    this->insert(I, E);
  }

  SmallDenseMap(std::initializer_list<typename BaseT::value_type> Vals)
      : SmallDenseMap(Vals.begin(), Vals.end()) {}

  ~SmallDenseMap() {
    this->destroyAll();
    deallocateBuckets();
  }

  void swap(SmallDenseMap& rhs) {
    unsigned TmpNumEntries = rhs.NumEntries;
    rhs.NumEntries = NumEntries;
    NumEntries = TmpNumEntries;
    std::swap(NumTombstones, rhs.NumTombstones);

    const KeyT EmptyKey = this->getEmptyKey();
    const KeyT TombstoneKey = this->getTombstoneKey();
    if (Small && rhs.Small) {
      // If we're swapping inline bucket arrays, we have to cope with some of
      // the tricky bits of DenseMap's storage system: the buckets are not
      // fully initialized. Thus we swap every key, but we may have
      // a one-directional move of the value.
      for (unsigned i = 0, e = InlineBuckets; i != e; ++i) {
        BucketT *lhsB = &getInlineBuckets()[i],
                *rhsB = &rhs.getInlineBuckets()[i];
        bool haslhsValue =
            (!KeyInfoT::isEqual(lhsB->get_first(), EmptyKey) &&
             !KeyInfoT::isEqual(lhsB->get_first(), TombstoneKey));
        bool hasrhsValue =
            (!KeyInfoT::isEqual(rhsB->get_first(), EmptyKey) &&
             !KeyInfoT::isEqual(rhsB->get_first(), TombstoneKey));
        if (haslhsValue && hasrhsValue) {
          // Swap together if we can...
          std::swap(*lhsB, *rhsB);
          continue;
        }
        // Swap separately and handle any asymmetry.
        std::swap(lhsB->get_first(), rhsB->get_first());
        if (haslhsValue) {
          ::new (&rhsB->get_second()) ValueT(std::move(lhsB->get_second()));
          lhsB->get_second().~ValueT();
        } else if (hasrhsValue) {
          ::new (&lhsB->get_second()) ValueT(std::move(rhsB->get_second()));
          rhsB->get_second().~ValueT();
        }
      }
      return;
    }
    if (!Small && !rhs.Small) {
      std::swap(getLargeRep()->Buckets, rhs.getLargeRep()->Buckets);
      std::swap(getLargeRep()->NumBuckets, rhs.getLargeRep()->NumBuckets);
      return;
    }

    SmallDenseMap& SmallSide = Small ? *this : rhs;
    SmallDenseMap& LargeSide = Small ? rhs : *this;

    // First stash the large side's rep and move the small side across.
    LargeRep TmpRep = std::move(*LargeSide.getLargeRep());
    LargeSide.getLargeRep()->~LargeRep();
    LargeSide.Small = true;
    // This is similar to the standard move-from-old-buckets, but the bucket
    // count hasn't actually rotated in this case. So we have to carefully
    // move construct the keys and values into their new locations, but there
    // is no need to re-hash things.
    for (unsigned i = 0, e = InlineBuckets; i != e; ++i) {
      BucketT *NewB = &LargeSide.getInlineBuckets()[i],
              *OldB = &SmallSide.getInlineBuckets()[i];
      ::new (&NewB->get_first()) KeyT(std::move(OldB->get_first()));
      OldB->get_first().~KeyT();
      if (!KeyInfoT::isEqual(NewB->get_first(), EmptyKey) &&
          !KeyInfoT::isEqual(NewB->get_first(), TombstoneKey)) {
        ::new (&NewB->get_second()) ValueT(std::move(OldB->get_second()));
        OldB->get_second().~ValueT();
      }
    }

    // The hard part of moving the small buckets across is done, just move
    // the TmpRep into its new home.
    SmallSide.Small = false;
    new (SmallSide.getLargeRep()) LargeRep(std::move(TmpRep));
  }

  SmallDenseMap& operator=(const SmallDenseMap& other) {
    if (&other != this) copyFrom(other);
    return *this;
  }

  SmallDenseMap& operator=(SmallDenseMap&& other) {
    this->destroyAll();
    deallocateBuckets();
    init(0);
    swap(other);
    return *this;
  }

  void copyFrom(const SmallDenseMap& other) {
    this->destroyAll();
    deallocateBuckets();
    Small = true;
    if (other.getNumBuckets() > InlineBuckets) {
      Small = false;
      new (getLargeRep()) LargeRep(allocateBuckets(other.getNumBuckets()));
    }
    this->BaseT::copyFrom(other);
  }

  void init(unsigned InitBuckets) {
    Small = true;
    if (InitBuckets > InlineBuckets) {
      Small = false;
      new (getLargeRep()) LargeRep(allocateBuckets(InitBuckets));
    }
    this->BaseT::initEmpty();
  }

  void grow(unsigned AtLeast) {
    if (AtLeast > InlineBuckets)
      AtLeast = std::max<unsigned>(64, NextPowerOf2(AtLeast - 1));

    if (Small) {
      // First move the inline buckets into a temporary storage.
      AlignedCharArrayUnion<BucketT[InlineBuckets]> TmpStorage;
      BucketT* TmpBegin = reinterpret_cast<BucketT*>(&TmpStorage);
      BucketT* TmpEnd = TmpBegin;

      // Loop over the buckets, moving non-empty, non-tombstones into the
      // temporary storage. Have the loop move the TmpEnd forward as it goes.
      const KeyT EmptyKey = this->getEmptyKey();
      const KeyT TombstoneKey = this->getTombstoneKey();
      for (BucketT *P = getBuckets(), *E = P + InlineBuckets; P != E; ++P) {
        if (!KeyInfoT::isEqual(P->get_first(), EmptyKey) &&
            !KeyInfoT::isEqual(P->get_first(), TombstoneKey)) {
          assert(size_t(TmpEnd - TmpBegin) < InlineBuckets &&
                 "Too many inline buckets!");
          ::new (&TmpEnd->get_first()) KeyT(std::move(P->get_first()));
          ::new (&TmpEnd->get_second()) ValueT(std::move(P->get_second()));
          ++TmpEnd;
          P->get_second().~ValueT();
        }
        P->get_first().~KeyT();
      }

      // AtLeast == InlineBuckets can happen if there are many tombstones,
      // and grow() is used to remove them. Usually we always switch to the
      // large rep here.
      if (AtLeast > InlineBuckets) {
        Small = false;
        new (getLargeRep()) LargeRep(allocateBuckets(AtLeast));
      }
      this->moveFromOldBuckets(TmpBegin, TmpEnd);
      return;
    }

    LargeRep OldRep = std::move(*getLargeRep());
    getLargeRep()->~LargeRep();
    if (AtLeast <= InlineBuckets) {
      Small = true;
    } else {
      new (getLargeRep()) LargeRep(allocateBuckets(AtLeast));
    }

    this->moveFromOldBuckets(OldRep.Buckets,
                             OldRep.Buckets + OldRep.NumBuckets);

    // Free the old table.
    deallocate_buffer(OldRep.Buckets, sizeof(BucketT) * OldRep.NumBuckets,
                      alignof(BucketT));
  }

  void shrink_and_clear() {
    unsigned OldSize = this->size();
    this->destroyAll();

    // Reduce the number of buckets.
    unsigned NewNumBuckets = 0;
    if (OldSize) {
      NewNumBuckets = 1 << (Log2_32_Ceil(OldSize) + 1);
      if (NewNumBuckets > InlineBuckets && NewNumBuckets < 64u)
        NewNumBuckets = 64;
    }
    if ((Small && NewNumBuckets <= InlineBuckets) ||
        (!Small && NewNumBuckets == getLargeRep()->NumBuckets)) {
      this->BaseT::initEmpty();
      return;
    }

    deallocateBuckets();
    init(NewNumBuckets);
  }

 private:
  unsigned getNumEntries() const { return NumEntries; }

  void setNumEntries(unsigned Num) {
    // NumEntries is hardcoded to be 31 bits wide.
    assert(Num < (1U << 31) && "Cannot support more than 1<<31 entries");
    NumEntries = Num;
  }

  unsigned getNumTombstones() const { return NumTombstones; }

  void setNumTombstones(unsigned Num) { NumTombstones = Num; }

  const BucketT* getInlineBuckets() const {
    assert(Small);
    // Note that this cast does not violate aliasing rules as we assert that
    // the memory's dynamic type is the small, inline bucket buffer, and the
    // 'storage' is a POD containing a char buffer.
    return reinterpret_cast<const BucketT*>(&storage);
  }

  BucketT* getInlineBuckets() {
    return const_cast<BucketT*>(
        const_cast<const SmallDenseMap*>(this)->getInlineBuckets());
  }

  const LargeRep* getLargeRep() const {
    assert(!Small);
    // Note, same rule about aliasing as with getInlineBuckets.
    return reinterpret_cast<const LargeRep*>(&storage);
  }

  LargeRep* getLargeRep() {
    return const_cast<LargeRep*>(
        const_cast<const SmallDenseMap*>(this)->getLargeRep());
  }

  const BucketT* getBuckets() const {
    return Small ? getInlineBuckets() : getLargeRep()->Buckets;
  }

  BucketT* getBuckets() {
    return const_cast<BucketT*>(
        const_cast<const SmallDenseMap*>(this)->getBuckets());
  }

  unsigned getNumBuckets() const {
    return Small ? InlineBuckets : getLargeRep()->NumBuckets;
  }

  void deallocateBuckets() {
    if (Small) return;

    deallocate_buffer(getLargeRep()->Buckets,
                      sizeof(BucketT) * getLargeRep()->NumBuckets,
                      alignof(BucketT));
    getLargeRep()->~LargeRep();
  }

  LargeRep allocateBuckets(unsigned Num) {
    assert(Num > InlineBuckets && "Must allocate more buckets than are inline");
    LargeRep Rep = {static_cast<BucketT*>(allocate_buffer(sizeof(BucketT) * Num,
                                                          alignof(BucketT))),
                    Num};
    return Rep;
  }
};

template <typename KeyT, typename ValueT, typename KeyInfoT, typename Bucket,
          bool IsConst>
class DenseMapIterator : DebugEpochBase::HandleBase {
  friend class DenseMapIterator<KeyT, ValueT, KeyInfoT, Bucket, true>;
  friend class DenseMapIterator<KeyT, ValueT, KeyInfoT, Bucket, false>;

 public:
  using difference_type = ptrdiff_t;
  using value_type = std::conditional_t<IsConst, const Bucket, Bucket>;
  using pointer = value_type*;
  using reference = value_type&;
  using iterator_category = std::forward_iterator_tag;

 private:
  pointer ptr_ = nullptr;
  pointer end_ = nullptr;

 public:
  DenseMapIterator() = default;

  DenseMapIterator(pointer pos, pointer e, const DebugEpochBase& epoch,
                   bool no_advance = false)
      : DebugEpochBase::HandleBase(&epoch), ptr_(pos), end_(e) {
    assert(is_handle_in_sync() && "invalid construction!");

    if (no_advance) return;

    if (should_reverse_iterate<KeyT>()) {
      retreat_past_empty_buckets();

      return;
    }

    advance_past_empty_buckets();
  }

  // Converting ctor from non-const iterators to const iterators. SFINAE'd out
  // for const iterator destinations so it doesn't end up as a user defined copy
  // constructor.
  template <bool IsConstSrc,
            typename = std::enable_if_t<!IsConstSrc && IsConst>>
  DenseMapIterator(
      const DenseMapIterator<KeyT, ValueT, KeyInfoT, Bucket, IsConstSrc>& I)
      : DebugEpochBase::HandleBase(I), Ptr(I.Ptr), End(I.End) {}

  reference operator*() const {
    assert(is_handle_in_sync() && "invalid iterator access!");
    assert(ptr_ != end_ && "dereferencing end() iterator");

    if (should_reverse_iterate<KeyT>()) return ptr_[-1];

    return *ptr_;
  }

  pointer operator->() const {
    assert(is_handle_in_sync() && "invalid iterator access!");
    assert(ptr_ != end_ && "dereferencing end() iterator");

    if (should_reverse_iterate<KeyT>()) return &(ptr_[-1]);

    return ptr_;
  }

  friend bool operator==(const DenseMapIterator& lhs,
                         const DenseMapIterator& rhs) {
    assert((!lhs.ptr_ || lhs.is_handle_in_sync()) && "handle not in sync!");
    assert((!rhs.ptr_ || rhs.is_handle_in_sync()) && "handle not in sync!");
    assert(lhs.get_epoch_address() == rhs.get_epoch_address() &&
           "comparing incomparable iterators!");

    return lhs.ptr_ == rhs.ptr_;
  }

  friend bool operator!=(const DenseMapIterator& lhs,
                         const DenseMapIterator& rhs) {
    return !(lhs == rhs);
  }

  inline DenseMapIterator& operator++() {  // Preincrement
    assert(is_handle_in_sync() && "invalid iterator access!");
    assert(ptr_ != end_ && "incrementing end() iterator");

    if (should_reverse_iterate<KeyT>()) {
      --ptr_;
      retreat_past_empty_buckets();
      return *this;
    }

    ++ptr_;
    advance_past_empty_buckets();

    return *this;
  }

  DenseMapIterator operator++(int) {  // Postincrement
    assert(is_handle_in_sync() && "invalid iterator access!");
    DenseMapIterator tmp = *this;
    ++*this;

    return tmp;
  }

 private:
  void advance_past_empty_buckets() {
    assert(ptr_ <= end_);

    const KeyT kEmpty = KeyInfoT::get_empty_key();
    const KeyT kTombstone = KeyInfoT::get_tombstone_key();

    while (ptr_ != end_ && (KeyInfoT::is_equal(ptr_->get_first(), kEmpty) ||
                            KeyInfoT::is_equal(ptr_->get_first(), kTombstone)))
      ++ptr_;
  }

  void retreat_past_empty_buckets() {
    assert(ptr_ >= end_);

    const KeyT kEmpty = KeyInfoT::get_empty_key();
    const KeyT kTombstone = KeyInfoT::get_tombstone_key();

    while (ptr_ != end_ &&
           (KeyInfoT::is_equal(ptr_[-1].get_first(), kEmpty) ||
            KeyInfoT::is_equal(ptr_[-1].get_first(), kTombstone)))
      --ptr_;
  }
};

template <typename KeyT, typename ValueT, typename KeyInfoT>
inline size_t capacity_in_bytes(const DenseMap<KeyT, ValueT, KeyInfoT>& X) {
  return X.getMemorySize();
}

}  // namespace chibicpp