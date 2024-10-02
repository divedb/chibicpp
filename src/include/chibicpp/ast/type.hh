#pragma once

#include <algorithm>
#include <cassert>
#include <iostream>
#include <map>
#include <memory>
#include <string>
#include <vector>

#include "chibicpp/support/math.hh"
#include "chibicpp/support/observer_ptr.hh"
#include "chibicpp/support/type_traits.hh"

namespace chibicpp {

/// The C language provides the four basic arithmetic type specifiers and the
/// modifiers:
/// ===========================================================================
/// Type:
/// - char
/// - int
/// - float
/// - double
/// ===========================================================================
/// Modifier:
/// - signed
/// - unsigned
/// - short
/// - long
/// ===========================================================================
class Type {
 public:
  class Layout {
   public:
    int size;
    int alignment;
  };

  enum Qualifier { kConst, kVolatile };

  enum ID : int {
    kChar = 0x01,
    kInt = 0x02,

    kFloat = 0x04,
    kDouble = 0x08,
    kPointer = 0x10,
    kArray = 0x20,
    kStruct = 0x40,
    kFunction = 0x80,
    kLabels = 0x100,

    kSigned = 0x1000,
    kUnsigned = 0x2000,
    kShort = 0x4000,
    kLong = 0x8000,
    kLongLong = 0x10000,

    kBits = 0x20000,
    kTypedef = 0x40000,

    kPrimitive = kChar | kInt | kFloat | kDouble,
    kTypeModifier = kSigned | kUnsigned | kShort | kLong | kLongLong,
  };

  friend inline ID operator|(ID lhs, ID rhs) {
    return static_cast<ID>(static_cast<int>(lhs) | static_cast<int>(rhs));
  }

  friend inline ID operator&(ID lhs, ID rhs) {
    return static_cast<ID>(static_cast<int>(lhs) & static_cast<int>(rhs));
  }

  /// @name Constructor
  /// @{

  /// Construct a type from the specified parameters.
  ///
  /// For example: the following snippet get a type of `int*`.
  ///
  /// \code
  /// auto type{Type::kInt, 8, TypeFactory::get_integer()};
  /// \endcode
  ///
  /// \param id The ID of the type.
  /// \param base The base type, defaulting to nullptr.
  Type(ID id, ObserverPtr<Type> base = nullptr);

  ///@}

  /// Destructor.
  virtual ~Type() = default;

  /// @name Type Check
  /// @{

  /// \return `true` if type is `char`, `false` otherwise.
  bool is_char() const { return id_ & ID::kChar; }

  /// \return `true` if type is `int`, `false` otherwise.
  bool is_integer() const {
    return id_ & (kBits | kChar | kShort | kInt | kLong | kLongLong);
  }

  /// \return `true` if type is `float`, `false` otherwise.
  bool is_float() const { return id_ & ID::kFloat; }

  /// \return `true` if type is `double`, `false` otherwise.
  bool is_double() const { return id_ & ID::kDouble; }

  /// \return `true` if type is `pointer`, `false` otherwise.
  bool is_pointer() const { return id_ & ID::kPointer; }

  /// \return `true` if type is `array`, `false` otherwise.
  bool is_array() const { return id_ & ID::kArray; }

  /// \return `true` if type is `struct`, `false` otherwise.
  bool is_struct() const { return id_ & ID::kStruct; }

  /// @}

  /// @name Type Modifiers
  /// @{

  /// \return `true` if it's `signed`, `false` otherwise.
  bool is_signed() const { return id_ & ID::kSigned; }

  /// \return `true` if it's `unsigned`, `false` otherwise.
  bool is_unsigned() const { return id_ & ID::kUnsigned; }

  /// \return `true` if it's `short`, `false` otherwise.
  bool is_short() const { return id_ & ID::kShort; }

  /// \return `true` if it's `long`, `false` otherwise.
  bool is_long() const { return id_ & ID::kLong; }

  /// \return `true` if it's `long long`, `false` otherwise.
  bool is_long_long() const { return id_ & ID::kLongLong; }

  bool has_modifier() const { return id_ & ID::kTypeModifier; }

  /// @}

  /// @name Access
  /// @{

  ID id() const { return id_; }

  /// \return The alignment value of the type.
  int alignment() const { return layout_.alignment; }

  /// \return Base type if it exists, nullptr otherwise.
  ObserverPtr<Type> base() const { return base_; }
  ObserverPtr<Type> alias() const { return alias_; }

  /// \return The size of this type in bytes.
  size_t size_in_bytes() const { return layout_.size; }

  ///@}

  /// Get detailed type information for this type.
  ///
  /// For example, calling `to_string` on type `a` will return "unsigned long
  /// long int".
  ///
  /// \code
  /// unsigned long long int a = 42;
  /// \endcode
  ///
  /// \return A string representation of the type.
  std::string to_string() const;

  /// Check if this type is equal to another type.
  ///
  /// \param other The type to compare with.
  /// \return `true` if two types are equal, `false` otherwise.
  virtual bool equals(const ObserverPtr<Type> other) const {
    // `int` and `unsigned int` are different.
    if (is_pointer() && other->is_pointer()) {
      return base_->equals(other->base_);
    }

    return id() == other->id();
  }

 protected:
  Layout layout_;

 private:
  ID id_;
  ObserverPtr<Type> base_;
  ObserverPtr<Type> alias_;  ///< Typedef
};

namespace {

template <typename T>
inline Type::Layout get_type_layout() {
  using TyTrait = TypeTraits<T>;

  return {TyTrait::size, TyTrait::alignment};
}

inline Type::Layout type_id_to_layout(Type::ID id) {
  if ((id & Type::kLong) && (id & Type::kDouble)) {
    return get_type_layout<long double>();
  } else if (id & Type::kDouble) {
    return get_type_layout<double>();
  } else if (id & Type::kFloat) {
    return get_type_layout<float>();
  } else if (id & Type::kLongLong) {
    return get_type_layout<long long>();
  } else if (id & Type::kLong) {
    return get_type_layout<long>();
  } else if (id & Type::kInt) {
    return get_type_layout<int>();
  } else if (id & Type::kShort) {
    return get_type_layout<short>();
  } else if (id & Type::kChar) {
    return get_type_layout<char>();
  } else if (id & Type::kPointer) {
    return get_type_layout<void*>();
  }

  // Make it crash if this dirty value is used.
  return {-1, -1};
}

}  // namespace

/// Class to represent integer types. Note that this class is also used to
/// represent the built-in integer types: Int1Ty, Int8Ty, Int16Ty, Int32Ty and
/// Int64Ty.
class IntegerType : public Type {
 public:
  explicit IntegerType(Type::ID type_id, int nbits)
      : Type{type_id}, nbits_{nbits} {}

  /// Get the number of bits to represent the integer type.
  ///
  /// \return Number of bits to represent the integer type.
  int num_bits() const { return nbits_; }

  /// Get a bitmask with ones set for all of the bits that can be set by
  ///        an unsigned version of this type.  This is 0xFF for i8, 0xFFFF for
  ///        i16, etc.
  ///
  /// \return Bit mask.
  uint64_t bit_mask() const { return ~uint64_t(0UL) >> (64 - num_bits()); }

  /// Get uint64_t with just the most significant bit set (the
  ///        sign bit, if the value is treated as a signed number).
  ///
  /// \return Sign bit.
  uint64_t sign_bit() const { return 1ULL << (num_bits() - 1); }

  /// Check if this type is equal to another type.
  ///
  /// Two `int` types are considered equal if they have the same number of bits.
  /// For example, the types of `a1` and `a2` in the following struct are
  /// different:
  ///
  /// \code
  /// struct Foo { int a1: 1; int a2: 2; };
  /// \endcode
  ///
  /// \param other The type to compare with.
  /// \return `true` if two types are equal, `false` otherwise.
  bool equals(const ObserverPtr<Type> other) const override {
    if (!Type::equals(other)) {
      return false;
    }

    return static_cast<const IntegerType*>(other.get())->num_bits() == nbits_;
  }

  /// Methods for support type inquiry through isa, cast, and dyn_cast.
  static inline bool classof(const ObserverPtr<Type> type) {
    return type->is_integer();
  }

 private:
  int nbits_;
};

/// Class to represent function types.
class FunctionType : public Type {
 public:
  FunctionType(ObserverPtr<Type> return_type,
               const std::vector<ObserverPtr<Type>>& params,
               bool is_var_arg = false)
      : Type{ID::kFunction},
        is_var_arg_{is_var_arg},
        return_type_{return_type},
        params_{params} {}

  bool is_var_arg() const { return is_var_arg_; }
  ObserverPtr<Type> return_type() const { return return_type_; }
  const std::vector<ObserverPtr<Type>>& params() const { return params_; }

  /// Get the parameter type at the specified index.
  ///
  /// \param i The index of the parameter, starting from 0.
  /// \return The parameter type at the specified index.
  ObserverPtr<Type> get_param_type(unsigned i) const {
    assert(i < params_.size());

    return params_[i];
  }

  /// Get the number of params for this function.
  ///
  /// This doesn't include the `is_var_arg`.
  ///
  /// \return The number of parameters.
  unsigned get_num_params() const { return params_.size(); }

  /// Check if this type is equal to another type.
  ///
  /// Two `function` types are considered equal if they are have:
  /// 1. Matching return type.
  /// 2. Matching parameter types.
  /// 3. Matching variadic flag.
  ///
  /// \param other The type to compare with.
  /// \return `true` if two types are equal, `false` otherwise.
  bool equals(const ObserverPtr<Type> other) const override {
    if (!Type::equals(other)) {
      return false;
    }

    auto fp = static_cast<const FunctionType*>(other.get());

    return fp->return_type_ == return_type_ && fp->params_ == params_ &&
           fp->is_var_arg_ == is_var_arg_;
  }

  static inline bool classof(const ObserverPtr<Type> type) {
    return type->id() == Type::kFunction;
  }

 private:
  bool is_var_arg_;
  ObserverPtr<Type> return_type_;
  std::vector<ObserverPtr<Type>> params_;
};

class ArrayType : public Type {
 public:
  /// Construct an array type with the specified `size` and `base` type.
  ///
  /// TODO(gc): give an example for 2D array.
  ///
  /// \param size Number of elements in array.
  /// \param base Base type.
  ArrayType(size_t length, ObserverPtr<Type> base)
      : Type{ID::kArray, base}, length_{length} {
    layout_.size = length * base->size_in_bytes();
    layout_.alignment = base->alignment();
  }

  /// Check if this array is equal to another type.
  ///
  /// Two `array` types are considered equal if they have same number of
  /// elements and identical base type.
  ///
  /// For example, while both `a` and `b` have a length of 8, they differ
  /// in base type: one is `int` and the other is `double`.
  ///
  /// \code
  /// int a[8]; double b[8];
  /// \endcode
  ///
  /// \param other The type to compare with.
  /// \return `true` if two types are equal, `false` otherwise.
  bool equals(const ObserverPtr<Type> other) const override {
    if (!Type::equals(other)) {
      return false;
    }

    auto arr = static_cast<const ArrayType*>(other.get());

    return length_ == arr->length_ && base()->equals(other->base());
  }

  size_t length() const { return length_; }

  static inline bool classof(const ObserverPtr<Type> type) {
    return type->id() == Type::kArray;
  }

 private:
  size_t length_;
};

class StructType;

class Member {
 public:
  /// Construct a member with the specified type.
  ///
  /// This constructor focuses solely on the member's type, ignoring its name.
  /// For instance, when generating IR, it is sufficient to know that the
  /// `Foo` structure contains an `int` and a `char`.
  ///
  /// \code
  /// struct Foo { int a; char c; };
  /// \endcode
  ///
  /// \param type The type of the member.
  Member(ObserverPtr<Type> type) : type_{type} {}

  Member(const std::string& name, ObserverPtr<Type> type)
      : type_{type}, name_{name} {}

  /// @name Modifier
  /// @{

  constexpr void set_index(int index) { index_ = index; }
  constexpr void set_offset(int offset) { offset_ = offset; }
  constexpr void set_owner(ObserverPtr<StructType> owner) { owner_ = owner; }

  /// @}

  /// @name Access
  /// @{

  constexpr int index() const { return index_; }
  constexpr int offset() const { return offset_; }
  constexpr ObserverPtr<Type> type() const { return type_; }
  constexpr ObserverPtr<StructType> owner() const { return owner_; }
  std::string name() const { return name_; }

  /// @}

  void dump(std::ostream& os) const;

 private:
  int index_;   ///< Index of the member in the order it was declared.
  int offset_;  ///< Offset of the member within the structure.
  ObserverPtr<Type> type_;         ///< Type of the member.
  ObserverPtr<StructType> owner_;  ///< The structure this member belongs.
  std::string name_;               ///< Name of the member.
};

/// StructType - Class to represent struct types.  There are two different kinds
/// of struct types: Literal structs and Identified structs.
///
/// Literal struct types (e.g. { i32, i32 }) are uniqued structurally, and must
/// always have a body when created.
class StructType : public Type {
 public:
  /// Structure meta information.
  enum Meta {
    kHasBody = 0x01,
    kIsPacked = 0x02,
    kIsLiteral = 0x04,
    kIsSized = 0x08,
    kDefault = 0x10
  };

  StructType(const std::string& name,
             std::vector<std::unique_ptr<Member>> members, Meta meta = kDefault)
      : Type{ID::kStruct, /*base*/ nullptr},
        meta_{meta},
        name_{name},
        members_{std::move(members)} {
    int offset = 0;
    int alignment = 0;

    for (size_t i = 0; i < members_.size(); ++i) {
      auto type = members_[i]->type();

      offset = align_to(offset, type->alignment());
      members_[i]->set_offset(offset);
      members_[i]->set_index(i);
      members_[i]->set_owner(this);
      offset += type->size_in_bytes();

      // Struct alignment is determined by the type of maximum alignment.
      alignment = std::max(alignment, type->alignment());
    }

    layout_.size = align_to(offset, alignment);
    layout_.alignment = alignment;
  }

  /// \return The tag of struct.
  std::string name() const { return name_; }

  /// Dump the member infomration inside this struct.
  void dump(std::ostream& os) const {
    for (const auto& mem : members_) {
      mem->dump(os);
    }
  }

  /// Get a member from the struct by its name.
  ///
  /// \param name The name of the member to search for.
  /// \return A non owning pointer to the member if it exists, nullptr
  ///         otherwise.
  ObserverPtr<Member> find_member(const std::string& name) const {
    auto name_match = [&name](const auto& m) { return m->name() == name; };
    auto iter = std::find_if(members_.begin(), members_.end(), name_match);

    if (iter == members_.end()) {
      return nullptr;
    }

    return iter->get();
  }

  bool is_packed() const { return meta_ & kIsPacked; }
  bool is_literal() const { return meta_ & kIsLiteral; }

  /// \return `true` if this type has an identity that has no body, `false`
  ///         otherwise.
  bool is_opaque() const { return (meta_ & kHasBody) == 0; }

  /// \return `true` if this type is a sized type, `false` otherwise.
  bool is_sized() const { return meta_ & kIsSized; }

  /// \return The number of members.
  unsigned get_num_members() const { return members_.size(); }

  /// Get the member's type at specified index.
  ///
  /// \param n Member index.
  /// \return Member type at specified index.
  ObserverPtr<Type> get_member_type(unsigned n) const {
    assert(n < members_.size());

    return members_[n]->type();
  }

  /// Check if this type is equal to another type.
  ///
  /// Two `struct` are considered same if they have same name.
  /// We may don't need to check the members.
  ///
  /// \param other The type to compare with.
  /// \return `true` if two types are equal, `false` otherwise.
  bool equals(const ObserverPtr<Type> other) const override {
    if (!Type::equals(other)) {
      return false;
    }

    auto st = static_cast<const StructType*>(other.get());

    return name_ == st->name_;
  }

  static inline bool classof(const ObserverPtr<Type> type) {
    return type->is_struct();
  }

 private:
  Meta meta_;
  std::string name_;
  std::vector<std::unique_ptr<Member>> members_;
};

class PointerType : public Type {
 public:
  explicit PointerType(ObserverPtr<Type> base) : Type{Type::kPointer, base} {
    assert(base);
  }
};

class TypedefType : public Type {
 public:
  explicit TypedefType(const std::string& name)
      : Type{Type::kTypedef}, name_{name} {}

  const std::string& name() const { return name_; }

 private:
  std::string name_;
};

/// This class manages type information.
///
/// The type information can be shared among all nodes and variables.
/// The returned pointer to `Type` must not be freed manually.
///
/// It provides various static methods to retrieve and create type instances,
/// including primitive types (e.g., char, int, float), pointer types,
/// array types, and user-defined struct types. The factory ensures that
/// only one instance of each unique type is created and reused.
class TypeFactory {
 public:
  /// @name Get Char
  /// @{

  /// \return A pointer to `char` type.
  static ObserverPtr<Type> get_char() { return get_native(Type::kChar); }

  /// \return A pointer to `signed char` type.
  static ObserverPtr<Type> get_signed_char() { return get_signed(Type::kChar); }

  /// \return A pointer to `unsigned char` type.
  static ObserverPtr<Type> get_unsigned_char() {
    return get_unsigned(Type::kChar);
  }

  /// @}

  /// @name Get Integer
  /// @{

  static ObserverPtr<Type> get_signed_bits(int nbits) {
    return get_bits(Index::kSigned, nbits);
  }

  static ObserverPtr<Type> get_unsigned_bits(int nbits) {
    return get_bits(Index::kUnsigned, nbits);
  }

  static ObserverPtr<Type> get_signed_short() {
    return get_signed(Type::kShort);
  }

  static ObserverPtr<Type> get_unsigned_short() {
    return get_unsigned(Type::kShort);
  }

  static ObserverPtr<Type> get_signed_int() { return get_signed(Type::kInt); }

  static ObserverPtr<Type> get_unsigned_int() {
    return get_unsigned(Type::kInt);
  }

  static ObserverPtr<Type> get_signed_long() { return get_signed(Type::kLong); }

  static ObserverPtr<Type> get_unsigned_long() {
    return get_unsigned(Type::kLong);
  }

  static ObserverPtr<Type> get_signed_long_long() {
    return get_signed(Type::kLongLong);
  }

  static ObserverPtr<Type> get_unsigned_long_long() {
    return get_unsigned(Type::kLongLong);
  }

  /// @}

  /// @name Get Float
  /// @{

  static ObserverPtr<Type> get_float() { return get_native(Type::kFloat); }

  static ObserverPtr<Type> get_double() { return get_native(Type::kDouble); }

  static ObserverPtr<Type> get_long_double() {
    return get_native(Type::kDouble | Type::kLong);
  }

  /// @}

  /// Get a function type with the specified signature.
  ///
  /// \param return_type The return type of the function.
  /// \param params The parameter types of the function.
  /// \param is_var_arg Indicates if the function is variadic.
  /// \return A pointer to the function type matching the specified signature.
  static ObserverPtr<Type> get_function(
      ObserverPtr<Type> return_type,
      const std::vector<ObserverPtr<Type>>& params, bool is_var_arg) {
    auto fn = [&](const auto& uptr) {
      auto fp = static_cast<const FunctionType*>(uptr.get());

      return fp->return_type() == return_type && fp->params() == params &&
             fp->is_var_arg() == is_var_arg;
    };

    return get<FunctionType>(Index::kNative, Type::kFunction, fn, return_type,
                             params, is_var_arg);
  }

  /// Get a function type taking no parameters.
  ///
  /// \param return_type The return type of the function.
  /// \param is_var_arg Indicates if the function is variadic.
  /// \return A pointer to the function type matching the specified signature.
  static ObserverPtr<Type> get_function(ObserverPtr<Type> return_type,
                                        bool is_var_arg) {
    return get_function(return_type, {}, is_var_arg);
  }

  /// Get a pointer type for the specified `base` type.
  ///
  /// For example, we could get a `int*` type through:
  ///
  /// \code
  /// auto int_type = TypeFactory::get_primitive(kInt);
  /// auto ptr_to_int_type = TypeFactory::get_pointer(int_type);
  /// \endcode
  ///
  /// \param base Base type.
  /// \return A `pointer` type based on the `base` type.
  static ObserverPtr<Type> get_pointer(ObserverPtr<Type> base) {
    assert(base);

    auto fn = [base](const auto& uptr) { return uptr->base() == base; };

    return get<PointerType>(Index::kNative, Type::kPointer, fn, base);
  }

  /// Get an array type by specified length and base type.
  ///
  /// For example, we could get type of `int[8]` through:
  ///
  /// \code
  /// auto int_type = TypeFactory::get__signed_int();
  /// auto arr_type = TypeFactory::get_array(8, int_type);
  /// \endcode
  ///
  /// \param size The number of elements in array.
  /// \param base The base type.
  /// \return A pointer to `array` type.
  static ObserverPtr<Type> get_array(size_t length, ObserverPtr<Type> base) {
    assert(base);

    auto fn = [length, base](const auto& uptr) {
      auto arr = static_cast<const ArrayType*>(uptr.get());

      return arr->length() == length && arr->base() == base;
    };

    return get<ArrayType>(Index::kNative, Type::kArray, fn, length, base);
  }

  static ObserverPtr<Type> get_struct(
      const std::string& name, std::vector<std::unique_ptr<Member>> members) {
    // Two structs are considered match if they have same tag and all the
    // members have identical type.
    auto fn = [&](const auto& uptr) {
      auto st = static_cast<const StructType*>(uptr.get());

      if (st->name() != name || st->get_num_members() != members.size()) {
        return false;
      }

      for (unsigned i = 0; i < st->get_num_members(); ++i) {
        if (st->get_member_type(i) != members[i]->type()) {
          return false;
        }
      }

      return true;
    };

    // It's safe to `std::move(members)` though `fn` references `members`.
    // Because it's a rvalue referenece, we do move after `fn` check passes.
    return get<StructType>(Index::kNative, Type::kStruct, fn, name,
                           std::move(members));
  }

  template <typename... Types>
  static ObserverPtr<Type> get_struct(const std::string& name,
                                      Types&&... types) {
    std::vector<std::unique_ptr<Member>> members;

    // Expand the parameter pack and create unique_ptr for each type.
    (members.emplace_back(std::make_unique<Member>(std::forward<Types>(types))),
     ...);

    return get_struct(name, std::move(members));
  }

  static ObserverPtr<Type> get_typedef(const std::string& name) {
    auto fn = [&](const auto& uptr) {
      auto type = static_cast<const TypedefType*>(uptr.get());

      return type->name() == name;
    };

    return get<TypedefType>(Index::kNative, Type::kTypedef, fn, name);
  }

 private:
  enum Index { kNative = 0, kSigned = 1, kUnsigned = 2, kMax };

  template <typename T, typename Fn, typename... Args>
  static ObserverPtr<Type> get(Index index, Type::ID type_id, Fn&& fn,
                               Args&&... args) {
    auto& factory = instance();
    auto& types = factory.type_info_[index][type_id];
    auto iter = std::find_if(types.begin(), types.end(), std::forward<Fn>(fn));

    if (iter != types.end()) {
      return iter->get();
    }

    types.push_back(std::make_unique<T>(std::forward<Args>(args)...));

    return types.back().get();
  }

  /// Get an integer type with the specified number of bits.
  ///
  /// When generating the IR, we may don't care the type of integer whether it's
  /// `long long`, `short` or `int` since we have the number of bits
  /// information. But we need to know it's signed or not.
  ///
  /// \param index Indicate the type is `signed` or `unsigned`.
  /// \param nbits The number of bits for the desired integer type.
  /// \return An integer type with the number of bits.
  static ObserverPtr<Type> get_bits(Index index, int nbits) {
    auto fn = [nbits](const auto& uptr) {
      return static_cast<const IntegerType*>(uptr.get())->num_bits() == nbits;
    };

    return get<IntegerType>(index, Type::kBits, fn, Type::kBits, nbits);
  }

  /// \return A static instance of type factory.
  static TypeFactory& instance() {
    static TypeFactory factory;

    return factory;
  }

  static ObserverPtr<Type> get_native(Type::ID type_id) {
    return get_predefined(Index::kNative, type_id);
  }

  static ObserverPtr<Type> get_signed(Type::ID type_id) {
    return get_predefined(Index::kSigned, type_id);
  }

  static ObserverPtr<Type> get_unsigned(Type::ID type_id) {
    return get_predefined(Index::kUnsigned, type_id);
  }

  static ObserverPtr<Type> get_predefined(Index index, Type::ID type_id) {
    assert(0 <= index && index < Index::kMax);

    auto& types = instance().type_info_[index];

    return types[type_id].back().get();
  }

  /// Initialize the factory with predefined types.
  TypeFactory() {
    for (auto type_id : {Type::kChar, Type::kFloat, Type::kDouble,
                         Type::kLong | Type::kDouble}) {
      type_info_[kNative][type_id].push_back(std::make_unique<Type>(type_id));
    }

    for (auto type_id : {Type::kChar, Type::kShort, Type::kInt, Type::kLong,
                         Type::kLongLong}) {
      auto [nbytes, _] = type_id_to_layout(type_id);
      auto nbits = nbytes * __CHAR_BIT__;

      type_info_[kSigned][type_id].push_back(
          std::make_unique<IntegerType>(type_id | Type::kSigned, nbits));
      type_info_[kUnsigned][type_id].push_back(
          std::make_unique<IntegerType>(type_id | Type::kUnsigned, nbits));
    }
  }

  /// Index:
  /// - native   => char, float, double, long double, pointer, array, struct,
  ///               function, label
  /// - signed   => char, short, int, long, long long
  /// - unsigned => char, short, int, long, long long
  using K = Type::ID;
  using V = std::vector<std::unique_ptr<Type>>;

  std::array<std::map<K, V>, kMax> type_info_;
};

}  // namespace chibicpp