//===- Twine.h - Fast Temporary String Concatenation ------------*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#pragma once

#include <cassert>
#include <cstdint>
#include <string>
#include <string_view>

namespace chibicpp {

/// Twine - A lightweight data structure for efficiently representing the
/// concatenation of temporary values as strings.
///
/// A Twine is a kind of rope, it represents a concatenated string using a
/// binary-tree, where the string is the preorder of the nodes. Since the
/// Twine can be efficiently rendered into a buffer when its result is used,
/// it avoids the cost of generating temporary values for intermediate string
/// results -- particularly in cases when the Twine result is never
/// required. By explicitly tracking the type of leaf nodes, we can also avoid
/// the creation of temporary strings for conversions operations (such as
/// appending an integer to a string).
///
/// A Twine is not intended for use directly and should not be stored, its
/// implementation relies on the ability to store pointers to temporary stack
/// objects which may be deallocated at the end of a statement. Twines should
/// only be used as const references in arguments, when an API wishes
/// to accept possibly-concatenated strings.
///
/// Twines support a special 'null' value, which always concatenates to form
/// itself, and renders as an empty string. This can be returned from APIs to
/// effectively nullify any concatenations performed on the result.
///
/// \b Implementation
///
/// Given the nature of a Twine, it is not possible for the Twine's
/// concatenation method to construct interior nodes; the result must be
/// represented inside the returned value. For this reason a Twine object
/// actually holds two values, the left- and right-hand sides of a
/// concatenation. We also have nullary Twine objects, which are effectively
/// sentinel values that represent empty strings.
///
/// Thus, a Twine can effectively have zero, one, or two children. The \see
/// is_nullary(), \see isUnary(), and \see isBinary() predicates exist for
/// testing the number of children.
///
/// We maintain a number of invariants on Twine objects (FIXME: Why):
///  - Nullary twines are always represented with their Kind on the left-hand
///    side, and the Empty kind on the right-hand side.
///  - Unary twines are always represented with the value on the left-hand
///    side, and the Empty kind on the right-hand side.
///  - If a Twine has another Twine as a child, that child should always be
///    binary (otherwise it could have been folded into the parent).
///
/// These invariants are check by \see is_valid().
///
/// \b Efficiency Considerations
///
/// The Twine is designed to yield efficient and small code for common
/// situations. For this reason, the concat() method is inlined so that
/// concatenations of leaf nodes can be optimized into stores directly into a
/// single stack allocated object.
///
/// In practice, not all compilers can be trusted to optimize concat() fully,
/// so we provide two additional methods (and accompanying operator+
/// overloads) to guarantee that particularly important cases (cstring plus
/// StringRef) codegen as desired.
class Twine {
  /// NodeKind - Represent the type of an argument.
  enum NodeKind : unsigned char {
    /// An empty string; the result of concatenating anything with it is also
    /// empty.
    kNullKind,

    /// The empty string.
    kEmptyKind,

    /// A pointer to a Twine instance.
    kTwineKind,

    /// A pointer to a C string instance.
    kCStringKind,

    /// A pointer to an std::string instance.
    kStdStringKind,

    /// A Pointer and Length representation. Used for std::string_view,
    /// StringRef, and SmallString.  Can't use a StringRef here
    /// because they are not trivally constructible.
    kPtrAndLengthKind,

    /// A pointer and length representation that's also null-terminated.
    /// Guaranteed to be constructed from a compile-time string literal.
    kStringLiteralKind,

    /// A pointer to a formatv_object_base instance.
    // FormatvObjectKind,

    /// A char value, to render as a character.
    kCharKind,

    /// An unsigned int value, to render as an unsigned decimal integer.
    kDecUIKind,

    /// An int value, to render as a signed decimal integer.
    kDecIKind,

    /// A pointer to an unsigned long value, to render as an unsigned decimal
    /// integer.
    kDecULKind,

    /// A pointer to a long value, to render as a signed decimal integer.
    kDecLKind,

    /// A pointer to an unsigned long long value, to render as an unsigned
    /// decimal integer.
    kDecULLKind,

    /// A pointer to a long long value, to render as a signed decimal integer.
    kDecLLKind,

    /// A pointer to a uint64_t value, to render as an unsigned hexadecimal
    /// integer.
    kUHexKind
  };

  union Child {
    const Twine* twine;
    const char* cstring;
    const std::string* std_string;
    struct {
      const char* ptr;
      size_t length;
    } ptr_and_length;
    // const formatv_object_base* formatvObject;
    char character;
    unsigned int dec_ui;
    int dec_i;
    const unsigned long* dec_ul;
    const long* dec_l;
    const unsigned long long* dec_ull;
    const long long* dec_ll;
    const uint64_t* u_hex;
  };

  /// lhs - The prefix in the concatenation, which may be uninitialized for
  /// Null or Empty kinds.
  Child lhs_;

  /// rhs - The suffix in the concatenation, which may be uninitialized for
  /// Null or Empty kinds.
  Child rhs_;

  /// lhsKind - The NodeKind of the left hand side, \see getlhsKind().
  NodeKind lhs_kind_ = kEmptyKind;

  /// rhs_kind - The NodeKind of the right hand side, \see getrhs_kind().
  NodeKind rhs_kind_ = kEmptyKind;

  /// Construct a nullary twine; the kind must be NullKind or EmptyKind.
  explicit Twine(NodeKind kind) : lhs_kind_{kind} {
    assert(is_nullary() && "Invalid kind!");
  }

  /// Construct a binary twine.
  explicit Twine(const Twine& lhs, const Twine& rhs)
      : lhs_kind_{kTwineKind}, rhs_kind_{kTwineKind} {
    this->lhs_.twine = &lhs;
    this->rhs_.twine = &rhs;

    assert(is_valid() && "Invalid twine!");
  }

  /// Construct a twine from explicit values.
  explicit Twine(Child lhs, NodeKind lhs_kind, Child rhs, NodeKind rhs_kind)
      : lhs_{lhs}, rhs_{rhs}, lhs_kind_{lhs_kind}, rhs_kind_{rhs_kind} {
    assert(is_valid() && "Invalid twine!");
  }

  /// Check for the null twine.
  bool is_null() const { return get_lhs_kind() == kNullKind; }

  /// Check for the empty twine.
  bool is_empty() const { return get_lhs_kind() == kEmptyKind; }

  /// Check if this is a nullary twine (null or empty).
  bool is_nullary() const { return is_null() || is_empty(); }

  /// Check if this is a unary twine.
  bool is_unary() const {
    return get_rhs_kind() == kEmptyKind && !is_nullary();
  }

  /// Check if this is a binary twine.
  bool is_binary() const {
    return get_lhs_kind() != kNullKind && get_rhs_kind() != kEmptyKind;
  }

  /// Check if this is a valid twine (satisfying the invariants on
  /// order and number of arguments).
  bool is_valid() const {
    // Nullary twines always have Empty on the RHS.
    if (is_nullary() && get_rhs_kind() != kEmptyKind) return false;

    // Null should never appear on the RHS.
    if (get_rhs_kind() == kNullKind) return false;

    // The RHS cannot be non-empty if the LHS is empty.
    if (get_rhs_kind() != kEmptyKind && get_lhs_kind() == kEmptyKind)
      return false;

    // A twine child should always be binary.
    if (get_lhs_kind() == kTwineKind && !lhs_.twine->is_binary()) return false;
    if (get_rhs_kind() == kTwineKind && !rhs_.twine->is_binary()) return false;

    return true;
  }

  /// Get the NodeKind of the left-hand side.
  NodeKind get_lhs_kind() const { return lhs_kind_; }

  /// Get the NodeKind of the right-hand side.
  NodeKind get_rhs_kind() const { return rhs_kind_; }

  /// Print one child from a twine.
  // void printOneChild(raw_ostream& OS, Child Ptr, NodeKind Kind) const;

  /// Print the representation of one child from a twine.
  // void printOneChildRepr(raw_ostream& OS, Child Ptr, NodeKind Kind) const;

 public:
  /// @name Constructors
  /// @{

  /// Construct from an empty string.
  /*implicit*/ Twine() { assert(is_valid() && "Invalid twine!"); }

  Twine(const Twine&) = default;

  /// Construct from a C string.
  ///
  /// We take care here to optimize "" into the empty twine -- this will be
  /// optimized out for string constants. This allows Twine arguments have
  /// default "" values, without introducing unnecessary string constants.
  /*implicit*/ Twine(const char* str) {
    if (str[0] != '\0') {
      lhs_.cstring = str;
      lhs_kind_ = kCStringKind;
    } else
      lhs_kind_ = kEmptyKind;

    assert(is_valid() && "Invalid twine!");
  }
  /// Delete the implicit conversion from nullptr as Twine(const char *)
  /// cannot take nullptr.
  /*implicit*/ Twine(std::nullptr_t) = delete;

  /// Construct from an std::string.
  /*implicit*/ Twine(const std::string& str) : lhs_kind_{kStdStringKind} {
    lhs_.std_string = &str;
    assert(is_valid() && "Invalid twine!");
  }

  /// Construct from an std::string_view by converting it to a pointer and
  /// length.  This handles string_views on a pure API basis, and avoids
  /// storing one (or a pointer to one) inside a Twine, which avoids problems
  /// when mixing code compiled under various C++ standards.
  /*implicit*/ Twine(const std::string_view& str)
      : lhs_kind_{kPtrAndLengthKind} {
    lhs_.ptr_and_length.ptr = str.data();
    lhs_.ptr_and_length.length = str.length();

    assert(is_valid() && "Invalid twine!");
  }

  //   /// Construct from a StringRef.
  //   /*implicit*/ Twine(const StringRef& Str) : lhs_kind_{kPtrAndLengthKind} {
  //     lhs_.ptrAndLength.ptr = Str.data();
  //     lhs_.ptrAndLength.length = Str.size();
  //     assert(is_valid() && "Invalid twine!");
  //   }

  //   /// Construct from a StringLiteral.
  //   /*implicit*/ Twine(const StringLiteral& Str) :
  //   lhs_kind_(StringLiteralKind) {
  //     lhs_.ptrAndLength.ptr = Str.data();
  //     lhs_.ptrAndLength.length = Str.size();
  //     assert(is_valid() && "Invalid twine!");
  //   }

  //   /// Construct from a SmallString.
  //   /*implicit*/ Twine(const SmallVectorImpl<char>& Str)
  //       : lhs_kind_(PtrAndLengthKind) {
  //     lhs_.ptrAndLength.ptr = Str.data();
  //     lhs_.ptrAndLength.length = Str.size();
  //     assert(is_valid() && "Invalid twine!");
  //   }

  //   /// Construct from a formatv_object_base.
  //   /*implicit*/ Twine(const formatv_object_base& Fmt)
  //       : lhs_kind_(FormatvObjectKind) {
  //     lhs_.formatvObject = &Fmt;
  //     assert(is_valid() && "Invalid twine!");
  //   }

  /// Construct from a char.
  explicit Twine(char val) : lhs_kind_{kCharKind} { lhs_.character = val; }

  /// Construct from a signed char.
  explicit Twine(signed char val) : lhs_kind_{kCharKind} {
    lhs_.character = static_cast<char>(val);
  }

  /// Construct from an unsigned char.
  explicit Twine(unsigned char val) : lhs_kind_{kCharKind} {
    lhs_.character = static_cast<char>(val);
  }

  /// Construct a twine to print \p val as an unsigned decimal integer.
  explicit Twine(unsigned val) : lhs_kind_{kDecUIKind} { lhs_.dec_ui = val; }

  /// Construct a twine to print \p val as a signed decimal integer.
  explicit Twine(int val) : lhs_kind_{kDecIKind} { lhs_.dec_i = val; }

  /// Construct a twine to print \p val as an unsigned decimal integer.
  explicit Twine(const unsigned long& val) : lhs_kind_{kDecULKind} {
    lhs_.dec_ul = &val;
  }

  /// Construct a twine to print \p val as a signed decimal integer.
  explicit Twine(const long& val) : lhs_kind_{kDecLKind} { lhs_.dec_l = &val; }

  /// Construct a twine to print \p val as an unsigned decimal integer.
  explicit Twine(const unsigned long long& val) : lhs_kind_{kDecULLKind} {
    lhs_.dec_ull = &val;
  }

  /// Construct a twine to print \p val as a signed decimal integer.
  explicit Twine(const long long& val) : lhs_kind_{kDecLLKind} {
    lhs_.dec_ll = &val;
  }

  // FIXME: Unfortunately, to make sure this is as efficient as possible we
  // need extra binary constructors from particular types. We can't rely on
  // the compiler to be smart enough to fold operator+()/concat() down to the
  // right thing. Yet.

  //   /// Construct as the concatenation of a C string and a StringRef.
  //   /*implicit*/ Twine(const char* LHS, const StringRef& RHS)
  //       : lhs_kind_(CStringKind), RHSKind(PtrAndLengthKind) {
  //     this->lhs_.cString = LHS;
  //     this->RHS.ptrAndLength.ptr = RHS.data();
  //     this->RHS.ptrAndLength.length = RHS.size();
  //     assert(is_valid() && "Invalid twine!");
  //   }

  //   /// Construct as the concatenation of a StringRef and a C string.
  //   /*implicit*/ Twine(const StringRef& LHS, const char* RHS)
  //       : lhs_kind_(PtrAndLengthKind), RHSKind(CStringKind) {
  //     this->lhs_.ptrAndLength.ptr = lhs_.data();
  //     this->lhs_.ptrAndLength.length = lhs_.size();
  //     this->RHS.cString = RHS;
  //     assert(is_valid() && "Invalid twine!");
  //   }

  /// Since the intended use of twines is as temporary objects, assignments
  /// when concatenating might cause undefined behavior or stack corruptions
  Twine& operator=(const Twine&) = delete;

  /// Create a 'null' string, which is an empty string that always
  /// concatenates to form another empty string.
  static Twine create_null() { return Twine{kNullKind}; }

  /// @}
  /// @name Numeric Conversions
  /// @{

  // Construct a twine to print \p val as an unsigned hexadecimal integer.
  static Twine u_to_hex_str(const uint64_t& val) {
    Child lhs;
    Child rhs;
    lhs.u_hex = &val;
    rhs.twine = nullptr;

    return Twine{lhs, kUHexKind, rhs, kEmptyKind};
  }

  /// @}
  /// @name Predicate Operations
  /// @{

  /// Check if this twine is trivially empty; a false return value does not
  /// necessarily mean the twine is empty.
  bool is_trivially_empty() const { return is_nullary(); }

  /// Check if this twine is guaranteed to refer to single string literal.
  bool is_single_string_literal() const {
    return is_unary() && get_lhs_kind() == kStringLiteralKind;
  }

  /// Return true if this twine can be dynamically accessed as a single
  /// StringRef value with getSingleStringRef().
  bool is_single_string_ref() const {
    if (get_rhs_kind() != kEmptyKind) return false;

    switch (get_lhs_kind()) {
      case kEmptyKind:
      case kCStringKind:
      case kStdStringKind:
      case kPtrAndLengthKind:
      case kStringLiteralKind:
        return true;
      default:
        return false;
    }
  }

  /// @}
  /// @name String Operations
  /// @{

  Twine concat(const Twine& suffix) const;

  /// @}
  /// @name Output & Conversion.
  /// @{

  /// Return the twine contents as a std::string.
  std::string str() const;

  /// Append the concatenated string into the given SmallString or SmallVector.
  // void toVector(SmallVectorImpl<char>& Out) const;

  //   /// This returns the twine as a single StringRef.  This method is only
  //   valid
  //   /// if isSingleStringRef() is true.
  //   StringRef getSingleStringRef() const {
  //     assert(isSingleStringRef() && "This cannot be had as a single
  //     stringref!"); switch (getlhs_kind_()) {
  //       default:
  //         llvm_unreachable("Out of sync with isSingleStringRef");
  //       case EmptyKind:
  //         return StringRef();
  //       case CStringKind:
  //         return StringRef(lhs_.cString);
  //       case StdStringKind:
  //         return StringRef(*lhs_.stdString);
  //       case PtrAndLengthKind:
  //       case StringLiteralKind:
  //         return StringRef(lhs_.ptrAndLength.ptr, lhs_.ptrAndLength.length);
  //     }
  //   }

  /// This returns the twine as a single StringRef if it can be
  /// represented as such. Otherwise the twine is written into the given
  /// SmallVector and a StringRef to the SmallVector's data is returned.
  //   StringRef toStringRef(SmallVectorImpl<char>& Out) const {
  //     if (isSingleStringRef()) return getSingleStringRef();
  //     toVector(Out);
  //     return StringRef(Out.data(), Out.size());
  //   }

  /// This returns the twine as a single null terminated StringRef if it
  /// can be represented as such. Otherwise the twine is written into the
  /// given SmallVector and a StringRef to the SmallVector's data is returned.
  ///
  /// The returned StringRef's size does not include the null terminator.
  // StringRef toNullTerminatedStringRef(SmallVectorImpl<char>& Out) const;

  /// Write the concatenated string represented by this twine to the
  /// stream \p OS.
  // void print(raw_ostream& OS) const;

  /// Dump the concatenated string represented by this twine to stderr.
  void dump() const;

  /// Write the representation of this twine to the stream \p OS.
  // void printRepr(raw_ostream& OS) const;

  /// Dump the representation of this twine to stderr.
  void dumpRepr() const;

  /// @}
};

/// @name Twine Inline Implementations
/// @{

inline Twine Twine::concat(const Twine& suffix) const {
  // Concatenation with null is null.
  if (is_null() || suffix.is_null()) return Twine{kNullKind};

  // Concatenation with empty yields the other side.
  if (is_empty()) return suffix;
  if (suffix.is_empty()) return *this;

  // Otherwise we need to create a new node, taking care to fold in unary
  // twines.
  Child new_lhs;
  Child new_rhs;
  new_lhs.twine = this;
  new_rhs.twine = &suffix;
  NodeKind new_lhs_kind = kTwineKind;
  NodeKind new_rhs_kind = kTwineKind;

  if (is_unary()) {
    new_lhs = lhs_;
    new_lhs_kind = get_lhs_kind();
  }
  if (suffix.is_unary()) {
    new_rhs = suffix.lhs_;
    new_rhs_kind = suffix.get_lhs_kind();
  }

  return Twine(new_lhs, new_lhs_kind, new_rhs, new_rhs_kind);
}

inline Twine operator+(const Twine& lhs, const Twine& rhs) {
  return lhs.concat(rhs);
}

/// Additional overload to guarantee simplified codegen; this is equivalent to
/// concat().

// inline Twine operator+(const char* LHS, const StringRef& RHS) {
//   return Twine(LHS, RHS);
// }

// /// Additional overload to guarantee simplified codegen; this is equivalent
// to
// /// concat().

// inline Twine operator+(const StringRef& LHS, const char* RHS) {
//   return Twine(LHS, RHS);
// }

// inline raw_ostream& operator<<(raw_ostream& OS, const Twine& RHS) {
//   RHS.print(OS);
//   return OS;
// }

}  // namespace chibicpp