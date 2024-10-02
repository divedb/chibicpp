//===- MemAlloc.h - Memory allocation functions -----------------*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
/// \file
///
/// This file defines counterparts of C library allocation functions defined in
/// the namespace 'std'. The new allocation functions crash on allocation
/// failure instead of returning null pointer.
///
//===----------------------------------------------------------------------===//

#pragma once

#include <cstdlib>
#include <exception>

#define REPORT_ALLOC_FAIL(...) throw std::bad_alloc()

namespace chibicpp {

inline void* safe_malloc(size_t sz) {
  void* result = std::malloc(sz);

  if (result == nullptr) {
    // It is implementation-defined whether allocation occurs if the space
    // requested is zero (ISO/IEC 9899:2018 7.22.3). Retry, requesting
    // non-zero, if the space requested was zero.
    if (sz == 0) return safe_malloc(1);

    REPORT_ALLOC_FAIL("Allocation failed");
  }
  return result;
}

inline void* safe_calloc(size_t count, size_t sz) {
  void* result = std::calloc(count, sz);

  if (result == nullptr) {
    // It is implementation-defined whether allocation occurs if the space
    // requested is zero (ISO/IEC 9899:2018 7.22.3). Retry, requesting
    // non-zero, if the space requested was zero.
    if (count == 0 || sz == 0) return safe_malloc(1);

    REPORT_ALLOC_FAIL("Allocation failed");
  }

  return result;
}

inline void* safe_realloc(void* ptr, size_t sz) {
  void* result = std::realloc(ptr, sz);

  if (result == nullptr) {
    // It is implementation-defined whether allocation occurs if the space
    // requested is zero (ISO/IEC 9899:2018 7.22.3). Retry, requesting
    // non-zero, if the space requested was zero.
    if (sz == 0) return safe_malloc(1);

    REPORT_ALLOC_FAIL("Allocation failed");
  }

  return result;
}

/// Allocate a buffer of memory with the given size and alignment.
///
/// When the compiler supports aligned operator new, this will use it to
/// handle even over-aligned allocations.
///
/// However, this doesn't make any attempt to leverage the fancier techniques
/// like posix_memalign due to portability. It is mostly intended to allow
/// compatibility with platforms that, after aligned allocation was added, use
/// reduced default alignment.
void* allocate_buffer(size_t size, size_t alignment);

/// Deallocate a buffer of memory with the given size and alignment.
///
/// If supported, this will used the sized delete operator. Also if supported,
/// this will pass the alignment to the delete operator.
///
/// The pointer must have been allocated with the corresponding new operator,
/// most likely using the above helper.
void deallocate_buffer(void* ptr, size_t size, size_t alignment);

}  // namespace chibicpp