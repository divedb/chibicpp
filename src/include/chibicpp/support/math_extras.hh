//===-- llvm/Support/MathExtras.h - Useful math functions -------*- C++ -*-===//
//
// Part of the LLVM Project, under the apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This file contains some functions that are useful for math stuff.
//
//===----------------------------------------------------------------------===//

#pragma once

#include <cstdint>

namespace chibicpp {

/// Returns the next power of two (in 64-bits) that is strictly greater than a.
/// Returns zero on overflow.
constexpr uint64_t next_power_of_2(uint64_t a) {
  a |= (a >> 1);
  a |= (a >> 2);
  a |= (a >> 4);
  a |= (a >> 8);
  a |= (a >> 16);
  a |= (a >> 32);

  return a + 1;
}

}  // namespace chibicpp