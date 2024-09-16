#pragma once

#include <algorithm>
#include <cassert>
#include <map>
#include <memory>
#include <numeric>
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

/// \brief Combines multiple enum flags using bitwise OR operation.
///
/// This function takes any number of enum values and performs a bitwise OR
/// operation on them. Each argument is first converted to an integer before
/// combining. This is useful for aggregating multiple flags into a single
/// result, especially when dealing with bitmask operations.
///
/// \tparam Args The types of the arguments. Each argument should be an enum
///         value that can be cast to an integer.
/// \param args The enum values to be combined. They are combined using
///             bitwise OR operation.
/// \return The result of combining all the provided enum values with bitwise
///         OR, represented as an integer.
template <typename... Args>
int bitwise_or_enum(Args... args) {
  return (... | static_cast<int>(args));
}

struct Type {
 public:
  /// @name Constructors
  /// @{

  /// \brief Construct a primitive type from specified parameters.
  ///
  /// \param kind Primitive type.
  /// \param base Base type and default is nullptr.
  Type(int kind, Type* base = nullptr) : kind_(kind), base_(base) {}

  ~Type() = default;

  ///@}

  /// @name Type kind.
  /// @{

  /// \brief Check type is a `char`.
  ///
  /// \return `true` if type is `char`, otherwise `false`.
  bool is_char() const { return kind_ & kChar; }

  /// \brief Check type is an integer.
  ///
  /// \return `true` if type is `int`, otherwise `false`.
  bool is_integer() const {
    return kind_ & bitwise_or_enum(kChar, kShort, kInt, kLong, kLongLong);
  }

  /// \brief Check type is a `float`.
  ///
  /// \return `true` if type is `float`, otherwise `false`.
  bool is_float() const { return kind_ & kFloat; }

  /// \brief Check type is a `double`.
  ///
  /// \return `true` if type is `double`, otherwise `false`.
  bool is_double() const { return kind_ & kDouble; }

  /// \brief Check type is a `pointer`.
  ///
  /// \return `true` if type is `pointer`, otherwise `false`.
  bool is_pointer() const { return kind_ & kPointer; }

  /// \brief Check type is an `array`.
  ///
  /// \return `true` if type is `array`, otherwise `false`.
  bool is_array() const { return kind_ & kArray; }

  /// \brief Check type is a `struct`.
  ///
  /// \return `true` if type is `struct`, otherwise `false`.
  bool is_struct() const { return kind_ & kStruct; }

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

  /// \brief Get the size of this type in bytes.
  ///
  /// Type of `array` and `struct` must override this method.
  ///
  /// \return Size in bytes.
  virtual size_t size_in_bytes() const {
    // Order matters.
    // warning: kind_ & (kLong | kDouble)
    if ((kind_ & kLong) && (kind_ & kDouble)) {
      return sizeof(long double);
    }

    if (kind_ & kDouble) {
      return sizeof(double);
    }

    if (kind_ & kFloat) {
      return sizeof(float);
    }

    if (kind_ & kLongLong) {
      return sizeof(long long);
    }

    if (kind_ & kLong) {
      return sizeof(long);
    }

    if (kind_ & kInt) {
      // TODO(gc): need to return `sizeof(int)`.
      return 8;
    }

    if (kind_ & kShort) {
      return sizeof(short);
    }

    if (kind_ & kChar) {
      return sizeof(char);
    }

    if (kind_ & kPointer) {
      return sizeof(void*);
    }

    // We can't determine the size of array and struct.
    __builtin_unreachable();

    return 0;
  }

  int kind() const { return kind_; }

  /// \brief Get base type.
  ///
  /// \return Base type.
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
};

class ArrayType : public Type {
 public:
  /// \brief Construct an array type with the specified `size` and `base` type.
  ///
  /// TODO(gc): give an example for 2D array.
  ///
  /// \param size Number of elements in array.
  /// \param base Base type.
  ArrayType(size_t length, Type* base) : Type(kArray, base), length_(length) {}

  /// \brief Get number of elements in this array.
  ///
  /// \return Number of elements.
  size_t length() const { return length_; }

  size_t size_in_bytes() const override {
    return length() * base()->size_in_bytes();
  }

 private:
  size_t length_;
};

struct Field {
  Type* type;
  std::string name;
};

class StructType : public Type {
 public:
  StructType(std::string const& name, std::vector<Field> const& field_types)
      : Type(kStruct, /*base*/ nullptr),
        name_(name),
        field_types_(field_types) {}

  std::string name() const { return name_; }

  size_t size_in_bytes() const override {
    return std::accumulate(field_types_.begin(), field_types_.end(), 0,
                           [](size_t init, auto const& field) {
                             return init + field.type->size_in_bytes();
                           });
  }

 private:
  std::string name_;
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

  /// \brief Get a pointer type for the specified `base` type.
  ///
  /// For example, we could get a `int*` type through:
  /// @code
  /// auto int_type = TypeMgr::get_primitive(kInt);
  /// auto ptr_to_int_type = TypeMgr::get_pointer(int_type);
  /// @endcode
  ///
  /// \param base Base type.
  /// \return A `pointer` type based on the `base` type.
  static Type* get_pointer(Type* base) {
    assert(base);

    auto& ty_mgr = instance();
    auto& pointer_type_info = ty_mgr.pointer_type_info_;
    auto iter =
        std::find_if(pointer_type_info.begin(), pointer_type_info.end(),
                     [base](auto const& uptr) { return uptr->base() == base; });

    if (iter != pointer_type_info.end()) {
      return iter->get();
    }

    pointer_type_info.push_back(std::make_unique<Type>(kPointer, base));

    return pointer_type_info.back().get();
  }

  /// \brief Get an array type.
  ///
  /// For example, we could get `int[8]` type through:
  /// @code
  /// auto int_type = TypeMgr::get_primitive(kInt);
  /// auto array_type = TypeMgr::get_array(8, int_type);
  /// @endcode
  ///
  /// \param size Number of elements in array.
  /// \param base Base type.
  /// \return An `array` type based on the `base` type.
  static Type* get_array(size_t length, Type* base) {
    assert(base);

    auto& ty_mgr = instance();
    auto& array_type_info = ty_mgr.array_type_info_;
    auto iter =
        std::find_if(array_type_info.begin(), array_type_info.end(),
                     [length, base](auto const& uptr) {
                       return uptr->length() == length && uptr->base() == base;
                     });

    if (iter != array_type_info.end()) {
      return iter->get();
    }

    array_type_info.push_back(std::make_unique<ArrayType>(length, base));

    return array_type_info.back().get();
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
#define INT_INITIALIZER(...) \
  std::initializer_list<int> { __VA_ARGS__ }

    // char, signed char, unsigned char
    for (auto key : INT_INITIALIZER(kChar, bitwise_or_enum(kChar, kSigned),
                                    bitwise_or_enum(kChar, kUnsigned))) {
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
      for (auto k2 :
           INT_INITIALIZER(k1, kInt, kSigned, bitwise_or_enum(kSigned, kInt),
                           kUnsigned, bitwise_or_enum(kUnsigned, kInt))) {
        auto key = k2 | k1;
        primitive_type_info_[key] = std::make_unique<Type>(key);
      }
    }

    // int, signed, signed int, unsigned, unsigned int
    for (auto key :
         INT_INITIALIZER(kInt, kSigned, bitwise_or_enum(kSigned, kInt),
                         kUnsigned, bitwise_or_enum(kUnsigned, kInt))) {
      primitive_type_info_[key] = std::make_unique<Type>(key);
    }

    // float, double, long double
    for (auto key :
         INT_INITIALIZER(kFloat, kDouble, bitwise_or_enum(kDouble, kLong))) {
      primitive_type_info_[key] = std::make_unique<Type>(key);
    }

#undef INT_INITIALIZER
  }

  std::map<int, std::unique_ptr<Type>> primitive_type_info_;
  std::vector<std::unique_ptr<Type>> pointer_type_info_;
  std::vector<std::unique_ptr<ArrayType>> array_type_info_;
  std::map<std::string, std::unique_ptr<Type>> aggregate_type_info_;
};

}  // namespace chibicpp