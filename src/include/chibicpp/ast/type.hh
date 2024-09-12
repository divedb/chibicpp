#pragma once

#include <cassert>
#include <map>
#include <memory>
#include <string>
#include <vector>

namespace chibicpp {

/// The C language provides the four basic arithmetic type specifiers char, int,
/// float and double, and the modifiers signed, unsigned, short, and long.
/// ===========================================================================
/// Type
/// char
/// signed char
/// unsigned char
/// short
/// short int
/// signed short
/// signed short int
/// unsigned short
/// unsigned short int
/// int
/// signed
/// signed int
/// unsigned
/// unsigned int
/// long
/// long int
/// signed long
/// signed long int
/// unsigned long
/// unsigned long int
/// long long
/// long long int
/// signed long long
/// signed long long int
/// unsigned long long
/// unsigned long long int
/// float
/// double
/// long double
enum TypeKind : int {
  kChar = 0x01,
  kInt = 0x02,
  kFloat = 0x04,
  kDouble = 0x08,
  kPointer = 0x10,
  kArray = 0x20,
  kStruct = 0x40,
  kPrimitive = kChar | kInt | kFloat | kDouble
};

enum TypeModifier : int {
  kSigned = 0x0100,
  kUnsigned = 0x0200,
  kShort = 0x0400,
  kLong = 0x800,
  kLongLong = 0x1000
};

struct Type {
 public:
  /// @name Constructors
  /// @{

  /// \brief Construct a primitive type from specified parameters.
  ///
  /// \param kind Primitive type.
  /// \param base Base type and default is nullptr.
  Type(int kind, Type* base = nullptr) : kind_(kind), base_(base) {}

  /// \brief Construct a customer type from the specified parameters.
  ///
  /// Example usage:
  /// @code
  /// struct Student { int age; };
  /// Type s("Student");
  /// @endcode
  ///
  /// \param name Type name.
  /// \param base Base type.
  Type(std::string const& name) : kind_(TypeKind::kStruct), name_(name) {}

  ///@}

  /// @name Type kind.
  /// @{

  /// \brief Check this type is a `char`.
  ///
  /// \return `true` if type is `char`, otherwise `false`.
  bool is_char() const { return kind_ & kChar; }

  /// \brief Check this type is an integer.
  ///
  /// Note: This type must exactly be `int`.
  ///
  /// \return `true` if type is `int`, otherwise `false`.
  bool is_integer() const { return kind_ & kInt; }

  /// \brief Check this type is a `float`.
  ///
  /// \return `true` if type is `float`, otherwise `false`.
  bool is_float() const { return kind_ & kFloat; }

  /// \brief Check this type is a `double`.
  ///
  /// \return `true` if type is `double`, otherwise `false`.
  bool is_double() const { return kind_ & kDouble; }

  bool is_pointer() const { return kind_ & kPointer; }

  /// @}

  /// @name Type modifiers.
  /// @{

  /// \brief Check this type is `signed`.
  ///
  /// \return `true` if it's `signed`, otherwise `false`.
  bool is_signed() const { return kind_ & kSigned; }

  /// \brief Check this type is `unsigned`.
  ///
  /// \return `true` if it's `unsigned`, otherwise `false`.
  bool is_unsigned() const { return kind_ & kUnsigned; }

  /// \brief Check this type is `short`.
  ///
  /// \return `true` if it's `short`, otherwise `false`.
  bool is_short() const { return kind_ & kShort; }

  /// \brief Check this type is `long`.
  ///
  /// \return `true` if it's `long`, otherwise `false`.
  bool is_long() const { return kind_ & kLong; }

  /// \brief Check this type is `long long`.
  ///
  /// \return `true` if it's `long long`, otherwise `false`.
  bool is_long_long() const { return kind_ & kLongLong; }

  /// @}

  std::string name() const { return name_; }
  int kind() const { return kind_; }
  Type* base() const { return base_; }
  Type* alias() const { return alias_; }

  /// \brief Retrieve detailed type information for this type.
  ///
  /// For example, calling `to_string` on type of `a` will return "unsigned long
  /// long int".
  /// @code
  /// unsigned long long int a = 42;
  /// @endcode
  ///
  /// \return A string representation of the type.
  std::string to_string() const;

 private:
  int kind_;
  Type* base_{};
  Type* alias_{};  ///< Typedef
  std::string name_;
};

class ArrayType : public Type {
 public:
  /// \brief Construct an array type with the specified `size` and `base` type.
  ///
  /// TODO(gc): give an example for 2D array.
  ///
  /// \param size Array size.
  /// \param base Base type.
  ArrayType(int size, Type* base) : Type(kArray, base), size_(size) {}

  /// \brief Get array size.
  ///
  /// \return Array size.
  int size() const { return size_; }

 private:
  int size_;
};

struct Field {
  Type* type;
  std::string name;
};

class StructType : public Type {
 public:
  StructType(std::string const& name, std::vector<Field> const& field_types)
      : Type(name), field_types_(field_types) {}

 private:
  std::vector<Field> field_types_;
};

class Node;
void add_type(Node* node);

/// \brief This class manages type information.
///
/// The type information can be shared among all nodes and variables.
/// The returned pointer to `Type` must not be freed manually.
class TypeMgr {
 public:
  /// \brief Retrieve the primitive type for a given kind.
  ///
  /// Note: The primitive type must already exist.
  ///
  /// \param kind An integer representing a type kind, possibly combined with
  ///             modifiers using the OR operator.
  /// \return A pointer to the primitive `Type`.
  static Type* get_primitive(int kind) {
    assert(kind & kPrimitive);
    auto& ty_mgr = instance();
    auto& primitive_type_info = ty_mgr.primitive_type_info_;
    auto iter = primitive_type_info.find(kind);
    assert(iter != primitive_type_info.end());

    return iter->second.get();
  }

  /// \brief Retrieve a pointer type for the specified base type.
  ///
  /// For example:
  /// @code
  /// auto int_type = TypeMgr::get_primitive(kInt);
  /// auto ptr_to_int_type = TypeMgr::get_pointer(int_type);
  /// @endcode
  /// In this way, we could get a `int*` type.
  ///
  /// \param base A pointer to base type.
  /// @return A pointer to the pointer `Type` based on the base type.
  static Type* get_pointer(Type* base) {
    assert(base);

    // TODO(gc): do we need to check the base has existed in this manager?
    auto& ty_mgr = instance();
    auto& pointer_type_info = ty_mgr.pointer_type_info_;
    auto iter = pointer_type_info.find(base);

    if (iter == pointer_type_info.end()) {
      pointer_type_info[base] = std::make_unique<Type>(kPointer, base);
    }

    return pointer_type_info[base].get();
  }

  static Type* get_aggregate() {
    std::abort();

    return nullptr;
  }

 private:
  /// \brief Retrieve a static instance of type manager.
  ///
  /// \return An instance of type manager.
  static TypeMgr& instance() {
    static TypeMgr ty_mgr;

    return ty_mgr;
  }

  /// \brief Default constructor. Initialize with primitive types.
  TypeMgr() {
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wdeprecated-enum-enum-conversion"

#define INT_INITIALIZER(...) \
  std::initializer_list<int> { __VA_ARGS__ }

    // char, signed char, unsigned char
    for (auto key :
         INT_INITIALIZER(kChar, kChar | kSigned, kChar | kUnsigned)) {
      primitive_type_info_[key] = std::make_unique<Type>(key);
    }

    // 1.
    // short, short int, signed short, signed short int, unsigned short,
    // unsigned short int
    // 2.
    // long, long int, signed long, signed long int, unsigned long,
    // unsigned long int
    // 3.
    // long long, long long int, signed long long, signed long long int,
    // unsigned long long,
    // unsigned long long int
    for (auto k1 : INT_INITIALIZER(kShort, kLong, kLongLong)) {
      for (auto k2 : INT_INITIALIZER(k1, kInt, kSigned, kSigned | kInt,
                                     kUnsigned, kUnsigned | kInt)) {
        auto key = k2 | k1;
        primitive_type_info_[key] = std::make_unique<Type>(key);
      }
    }

    // int, signed, signed int, unsigned, unsigned int
    for (auto key : INT_INITIALIZER(kInt, kSigned, kSigned | kInt, kUnsigned,
                                    kUnsigned | kInt)) {
      primitive_type_info_[key] = std::make_unique<Type>(key);
    }

    // float, double, long double
    for (auto key : INT_INITIALIZER(kFloat, kDouble, kDouble | kLong)) {
      primitive_type_info_[key] = std::make_unique<Type>(key);
    }

#undef INT_INITIALIZER

#pragma GCC diagnostic pop
  }

  std::map<int, std::unique_ptr<Type>> primitive_type_info_;
  std::map<Type*, std::unique_ptr<Type>> pointer_type_info_;
  std::map<std::string, std::unique_ptr<Type>> aggregate_type_info_;
};

}  // namespace chibicpp