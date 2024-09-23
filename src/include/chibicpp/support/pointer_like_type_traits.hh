//===- llvm/Support/PointerLikeTypeTraits.h - Pointer Traits ----*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file defines the PointerLikeTypeTraits class.  This allows data
// structures to reason about pointers and other things that are pointer sized.
//
//===----------------------------------------------------------------------===//

#pragma once

#include <cstdint>

namespace chibicpp {

/// PointerLikeTypeTraits - This is a traits object that is used to handle
/// pointer types and things that are just wrappers for pointers as a uniform
/// entity.
template <typename T>
class PointerLikeTypeTraits {
  // get_as_void_pointer
  // get_from_void_pointer
};

/// Provide PointerLikeTypeTraits for non-cvr pointers.
template <typename T>
class PointerLikeTypeTraits<T*> {
 public:
  static inline void* get_as_void_pointer(T* p) { return p; }
  static inline T* get_from_void_pointer(void* p) { return static_cast<T*>(p); }

  /// Note, we assume here that malloc returns objects at least 4-byte aligned.
  /// However, this may be wrong, or pointers may be from something other than
  /// malloc.  In this case, you should specialize this template to reduce this.
  ///
  /// All clients should use assertions to do a run-time check to ensure that
  /// this is actually true.
  enum { kNumLowBitsAvailable = 2 };
};

/// Provide PointerLikeTypeTraits for const pointers.
template <typename T>
class PointerLikeTypeTraits<const T*> {
  typedef PointerLikeTypeTraits<T*> NonConst;

 public:
  static inline const void* get_as_void_pointer(const T* p) {
    return NonConst::get_as_void_pointer(const_cast<T*>(p));
  }

  static inline const T* get_from_void_pointer(const void* p) {
    return NonConst::get_from_void_pointer(const_cast<void*>(p));
  }

  enum { kNumLowBitsAvailable = NonConst::kNumLowBitsAvailable };
};

/// Provide PointerLikeTypeTraits for uintptr_t.
template <>
class PointerLikeTypeTraits<uintptr_t> {
 public:
  static inline void* get_as_void_pointer(uintptr_t p) {
    return reinterpret_cast<void*>(p);
  }

  static inline uintptr_t get_from_void_pointer(void* p) {
    return reinterpret_cast<uintptr_t>(p);
  }

  // No bits are available!
  enum { kNumLowBitsAvailable = 0 };
};

}  // namespace chibicpp