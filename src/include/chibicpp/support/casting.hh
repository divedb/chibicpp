//===-- llvm/Support/Casting.h - Allow flexible, checked, casts -*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file defines the isa<X>(), cast<X>(), dyn_cast<X>(), cast_or_null<X>(),
// and dyn_cast_or_null<X>() templates.
//
//===----------------------------------------------------------------------===//

#pragma once

namespace chibicpp {

/// Define a template that can be specialized by smart pointers to reflect the
/// fact that they are automatically dereferenced, and are not involved with the
/// template selection process...  the default implementation is a noop.
template <typename From>
struct simplify_type {
  using SimpleType = From;

  static SimpleType& get_simplified_value(From& val) { return val; }
};

}  // namespace chibicpp