//===-- llvm/Value.h - Definition of the Value class ------------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file declares the Value class.
//
//===----------------------------------------------------------------------===//

#pragma once

namespace chibicpp {

/// This is a very important LLVM class. It is the base class of all values
/// computed by a program that may be used as operands to other values. Value is
/// the super class of other important classes such as Instruction and Function.
/// All Values have a Type. Type is not a subclass of Value. Some values can
/// have a name and they belong to some Module.  Setting the name on the Value
/// automatically updates the module's symbol table.
///
/// Every value has a "use list" that keeps track of which other Values are
/// using this Value.  A Value can also have an arbitrary number of ValueHandle
/// objects that watch it and listen to RAUW and Destroy events.  See
/// llvm/Support/ValueHandle.h for details.
///
/// \brief LLVM Value Representation
class Value {
 private:
  /// Subclass identifier (for isa/dyn_cast).
  const unsigned char sub_class_id_;
  /// Has a ValueHandle pointing to this?
  unsigned char has_value_handle_ : 1;

 protected:
  /// This member is similar to SubclassData, however it is for holding
  /// information which may be used to aid optimization, but which may be
  /// cleared to zero without affecting conservative interpretation.
  unsigned char subclass_optional_data_ : 7;

 private:
  /// This member is defined by this class, but is not used for
  /// anything.  Subclasses can use it to hold whatever state they find useful.
  /// This field is initialized to zero by the ctor.
  unsigned short subclass_data_;
};

}  // namespace chibicpp