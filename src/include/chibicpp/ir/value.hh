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

#include "chibicpp/ast/type.hh"

namespace chibicpp {

class Use;

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
 public:
  /// An enumeration for keeping track of the concrete subclass of Value that
  /// is actually instantiated. Values of this enumeration are kept in the
  /// Value classes SubclassID field. They are used for concrete type
  /// identification.
  enum ValueTy {
    kArgumentVal,               // This is an instance of Argument
    kBasicBlockVal,             // This is an instance of BasicBlock
    kFunctionVal,               // This is an instance of Function
    kGlobalAliasVal,            // This is an instance of GlobalAlias
    kGlobalVariableVal,         // This is an instance of GlobalVariable
    kUndefValueVal,             // This is an instance of UndefValue
    kBlockAddressVal,           // This is an instance of BlockAddress
    kConstantExprVal,           // This is an instance of ConstantExpr
    kConstantAggregateZeroVal,  // This is an instance of ConstantAggregateZero
    kConstantDataArrayVal,      // This is an instance of ConstantDataArray
    kConstantDataVectorVal,     // This is an instance of ConstantDataVector
    kConstantIntVal,            // This is an instance of ConstantInt
    kConstantFPVal,             // This is an instance of ConstantFP
    kConstantArrayVal,          // This is an instance of ConstantArray
    kConstantStructVal,         // This is an instance of ConstantStruct
    kConstantVectorVal,         // This is an instance of ConstantVector
    kConstantPointerNullVal,    // This is an instance of ConstantPointerNull
    kMDNodeVal,                 // This is an instance of MDNode
    kMDStringVal,               // This is an instance of MDString
    kInlineAsmVal,              // This is an instance of InlineAsm
    vPseudoSourceValueVal,      // This is an instance of PseudoSourceValue
    kFixedStackPseudoSourceValueVal,  // This is an instance of
                                      // FixedStackPseudoSourceValue
    kInstructionVal,                  // This is an instance of Instruction
    // Enum values starting at InstructionVal are used for Instructions;
    // don't add new values here!

    // Markers:
    kConstantFirstVal = kFunctionVal,
    kConstantLastVal = kConstantPointerNullVal
  };

  virtual ~Value();

  /// \return Type of this value.
  ObserverPtr<Type> type() const { return type_; }

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

  ObserverPtr<Type> type_;
  Use* use_list_;
};

}  // namespace chibicpp