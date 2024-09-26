#pragma once

#include <algorithm>
#include <cassert>
#include <map>
#include <memory>
#include <numeric>
#include <string>
#include <vector>

#include "chibicpp/util/observer_ptr.hh"

namespace chibicpp {

struct Type {
 public:
  /// The C language provides the four basic arithmetic type specifiers char,
  /// int, float and double, and the modifiers signed, unsigned, short, and
  /// long.
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
  /// unsignedcont_len
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
  enum TypeID : int {
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

    kPrimitive = kChar | kInt | kFloat | kDouble,
    kTypeModifier = kSigned | kUnsigned | kShort | kLong | kLongLong,
  };

  friend inline TypeID operator|(TypeID lhs, TypeID rhs) {
    return static_cast<TypeID>(static_cast<int>(lhs) | static_cast<int>(rhs));
  }

  friend inline TypeID operator&(TypeID lhs, TypeID rhs) {
    return static_cast<TypeID>(static_cast<int>(lhs) & static_cast<int>(rhs));
  }

  friend inline bool is_integer_type(TypeID type_id) {
    return type_id & (kChar | kShort | kInt | kLong | kLongLong);
  }

  /// @name Constructor
  /// @{

  /// \brief Construct a type from the specified parameters.
  ///
  /// \param id The type ID.
  /// \param base The base type and default is nullptr.
  Type(TypeID id, ObserverPtr<Type> base = nullptr) : id_{id}, base_{base} {}

  ///@}

  /// \brief Destructor.
  virtual ~Type() = default;

  /// @name Type ID
  /// @{

  /// \brief Check type is a `char`.
  ///
  /// \return `true` if type is `char`, `false` otherwise.
  bool is_char() const { return id_ & TypeID::kChar; }

  /// \brief Check type is an integer.
  ///
  /// \return `true` if type is `int`, `false` otherwise.
  bool is_integer() const { return is_integer_type(id_); }

  /// \brief Check type is a `float`.
  ///
  /// \return `true` if type is `float`, `false` otherwise.
  bool is_float() const { return id_ & TypeID::kFloat; }

  /// \brief Check type is a `double`.
  ///
  /// \return `true` if type is `double`, `false` otherwise.
  bool is_double() const { return id_ & TypeID::kDouble; }

  /// \brief Check type is a `pointer`.
  ///
  /// \return `true` if type is `pointer`, `false` otherwise.
  bool is_pointer() const { return id_ & TypeID::kPointer; }

  /// \brief Check type is an `array`.
  ///
  /// \return `true` if type is `array`, `false` otherwise.
  bool is_array() const { return id_ & TypeID::kArray; }

  /// \brief Check type is a `struct`.
  ///
  /// \return `true` if type is `struct`, `false` otherwise.
  bool is_struct() const { return id_ & TypeID::kStruct; }

  /// @}

  /// @name Type modifiers.
  /// @{

  /// \brief Check this type is `signed`.
  ///
  /// \return `true` if it's `signed`, `false` otherwise.
  bool is_signed() const { return id_ & TypeID::kSigned; }

  /// \brief Check this type is `unsigned`.
  ///
  /// \return `true` if it's `unsigned`, `false` otherwise.
  bool is_unsigned() const { return id_ & TypeID::kUnsigned; }

  /// \brief Check this type is `short`.
  ///
  /// \return `true` if it's `short`, `false` otherwise.
  bool is_short() const { return id_ & TypeID::kShort; }

  /// \brief Check this type is `long`.
  ///
  /// \return `true` if it's `long`, `false` otherwise.
  bool is_long() const { return id_ & TypeID::kLong; }

  /// \brief Check this type is `long long`.
  ///
  /// \return `true` if it's `long long`, `false` otherwise.
  bool is_long_long() const { return id_ & TypeID::kLongLong; }

  bool is_modifier() const { return id_ & TypeID::kTypeModifier; }

  /// @}

  TypeID id() const { return id_; }

  /// \brief Get base type.
  ///
  /// \return Base type.
  ObserverPtr<Type> base() const { return base_; }
  ObserverPtr<Type> alias() const { return alias_; }

  void clear_signed() {
    unsigned mask = ~kSigned;
    id_ = static_cast<TypeID>(id_ & mask);
  }

  void clear_unsigned() {
    unsigned mask = ~kUnsigned;
    id_ = static_cast<TypeID>(id_ & mask);
  }

  void set_signed() {
    // A type can't be signed and unsigned simutaneously.
    clear_unsigned();
    id_ = id_ | kSigned;
  }

  void set_unsigned() {
    clear_signed();
    id_ = id_ | kUnsigned;
  }

  /// \brief Get the size of this type in bytes.
  ///
  /// Type of `array` and `struct` must override this method.
  ///
  /// \return Size in bytes.
  virtual size_t size_in_bytes() const {
    // Order matters.
    if (is_long() && is_double()) {
      return sizeof(long double);
    }

    if (is_double()) {
      return sizeof(double);
    }

    if (is_float()) {
      return sizeof(float);
    }

    if (is_long_long()) {
      return sizeof(long long);
    }

    if (is_long()) {
      return sizeof(long);
    }

    if (is_short()) {
      return sizeof(short);
    }

    if (is_char()) {
      return sizeof(char);
    }

    if (is_pointer()) {
      return sizeof(void*);
    }

    if (is_integer()) {
      // TODO(gc): need to return `sizeof(int)`.
      return 8;
    }

    // We can't determine the size of array and struct.
    __builtin_unreachable();

    return 0;
  }

  /// \brief Retrieve detailed type information for this type.
  ///
  /// For example, calling `to_string` on type of `a` will return "unsigned long
  /// long int".
  /// \code
  /// unsigned long long int a = 42;
  /// \endcode
  ///
  /// \return A string representation of the type.
  std::string to_string() const;

  /// \brief Check if this type is equal to another type.
  ///
  /// This default implementation compares only primitive types, such as `int`,
  /// `char`, `short`, and etc.
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

 private:
  TypeID id_;
  ObserverPtr<Type> base_;
  ObserverPtr<Type> alias_;  ///< Typedef
};

/// Class to represent integer types. Note that this class is also used to
/// represent the built-in integer types: Int1Ty, Int8Ty, Int16Ty, Int32Ty and
/// Int64Ty.
class IntegerType : public Type {
 public:
  explicit IntegerType(int num_bits, TypeID type_id)
      : Type{TypeID::kInt | type_id}, num_bits_{num_bits} {}

  /// \brief Get the number of bits to represent the integer type.
  ///
  /// \return Number of bits to represent the integer type.
  int num_bits() const { return num_bits_; }

  /// \brief Get a bitmask with ones set for all of the bits that can be set by
  ///        an unsigned version of this type.  This is 0xFF for i8, 0xFFFF for
  ///        i16, etc.
  ///
  /// \return Bit mask.
  uint64_t bit_mask() const { return ~uint64_t(0UL) >> (64 - num_bits()); }

  /// \brief Get uint64_t with just the most significant bit set (the
  ///        sign bit, if the value is treated as a signed number).
  ///
  /// \return Sign bit.
  uint64_t sign_bit() const { return 1ULL << (num_bits() - 1); }

  /// \brief Check if this type is equal to another type.
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

    return static_cast<const IntegerType*>(other.get())->num_bits() ==
           num_bits_;
  }

  /// Methods for support type inquiry through isa, cast, and dyn_cast.
  static inline bool classof(const ObserverPtr<Type> type) {
    return type->id() == TypeID::kInt;
  }

 private:
  int num_bits_;
};

/// Class to represent function types.
class FunctionType : public Type {
 public:
  FunctionType(ObserverPtr<Type> return_type,
               const std::vector<ObserverPtr<Type>>& params,
               bool is_var_arg = false)
      : Type{TypeID::kFunction},
        is_var_arg_{is_var_arg},
        return_type_{return_type},
        params_{params} {}

  bool is_var_arg() const { return is_var_arg_; }
  ObserverPtr<Type> return_type() const { return return_type_; }
  const std::vector<ObserverPtr<Type>>& params() const { return params_; }

  /// \brief Get the parameter type at the specified index.
  ///
  /// \param i The index of the parameter, starting from 0.
  /// \return The parameter type at the specified index.
  ObserverPtr<Type> get_param_type(unsigned i) const {
    assert(i < params_.size());

    return params_[i];
  }

  /// \brief Get the number of params for this function.
  ///
  /// This doesn't include the `is_var_arg`.
  ///
  /// \return The number of parameters.
  unsigned get_num_params() const { return params_.size(); }

  /// \brief Check if this type is equal to another type.
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
  /// \brief Construct an array type with the specified `size` and `base` type.
  ///
  /// TODO(gc): give an example for 2D array.
  ///
  /// \param size Number of elements in array.
  /// \param base Base type.
  ArrayType(size_t length, ObserverPtr<Type> base)
      : Type{TypeID::kArray, base}, length_{length} {}

  /// \brief Get number of elements in this array.
  ///
  /// \return Number of elements.
  size_t length() const { return length_; }

  size_t size_in_bytes() const override {
    return length() * base()->size_in_bytes();
  }

  /// \brief Check if this type is equal to another type.
  ///
  /// Two `array` types are considered equal if they have same number of
  /// elements and the same base type.
  ///
  /// For example, while both `a` and `b` have a length of 8, they differ
  /// in base type: one is `int` and the other is `double`.
  ///
  /// \code
  /// int a[8]; double b[8];
  /// \endcode
  ///
  /// \param other The type to compare with. \return `true` if two
  /// types are equal, `false` otherwise.
  bool equals(const ObserverPtr<Type> other) const override {
    if (!Type::equals(other)) {
      return false;
    }

    auto arr = static_cast<const ArrayType*>(other.get());

    return length_ == arr->length_ && base()->equals(other->base());
  }

  static inline bool classof(const ObserverPtr<Type> type) {
    return type->id() == Type::kArray;
  }

 private:
  size_t length_;
};

class StructType;

class Member {
 public:
  /// \brief Construct a member with the specified type.
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
  constexpr void set_owner(StructType* owner) { owner_ = owner; }

  /// @}

  /// @name Access
  /// @{

  constexpr int index() const { return index_; }
  constexpr int offset() const { return offset_; }
  constexpr ObserverPtr<Type> type() const { return type_; }
  constexpr ObserverPtr<StructType> owner() const { return owner_; }
  std::string name() const { return name_; }

  /// @}

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
      : Type{TypeID::kStruct, /*base*/ nullptr},
        meta_{meta},
        name_{name},
        members_{std::move(members)} {
    int offset = 0;

    for (size_t i = 0; i < members.size(); ++i) {
      members[i]->set_offset(offset);
      members[i]->set_index(i);
      members[i]->set_owner(this);
      offset += members[i]->type()->size_in_bytes();
    }
  }

  std::string name() const { return name_; }

  size_t size_in_bytes() const override {
    return std::accumulate(members_.begin(), members_.end(), 0,
                           [](size_t init, auto const& m) {
                             return init + m->type()->size_in_bytes();
                           });
  }

  /// \brief Get a member from the struct by its name.
  ///
  /// \param name The name of the member to search for.
  /// \return A non owning pointer to the member if it exists, nullptr
  ///         otherwise.
  ObserverPtr<Member> find_member(const std::string& name) const {
    auto name_match = [&name](auto const& m) { return m->name() == name; };
    auto iter = std::find_if(members_.begin(), members_.end(), name_match);

    if (iter == members_.end()) {
      return nullptr;
    }

    return ObserverPtr<Member>{iter->get()};
  }

  bool is_packed() const { return meta_ & kIsPacked; }
  bool is_literal() const { return meta_ & kIsLiteral; }

  /// \brief Check this type has an identity that has no body.
  ///
  /// \return `true` if this type has an identity that has no body, `false`
  ///         otherwise.
  bool is_opaque() const { return (meta_ & kHasBody) == 0; }

  /// \brief Check this type is a sized type.
  ///
  /// \return `true` if this type is a sized type, `false` otherwise.
  bool is_sized() const { return meta_ & kIsSized; }

  /// \brief Get the number of members inside the structure.
  ///
  /// \return The number of members.
  unsigned get_num_members() const { return members_.size(); }

  /// \brief Get the member's type at specified index.
  ///
  /// \param n Member index.
  /// \return Member type at specified index.
  ObserverPtr<Type> get_member_type(unsigned n) const {
    assert(n < members_.size());

    return members_[n]->type();
  }

  /// \brief Check if this type is equal to another type.
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
    return type->id() == Type::kStruct;
  }

 private:
  Meta meta_;
  std::string name_;
  std::vector<std::unique_ptr<Member>> members_;
};

class Node;
void add_type(Node* node);

/// \brief This class manages type information.
///
/// The type information can be shared among all nodes and variables.
/// The returned pointer to `Type` must not be freed manually.
class TypeMgr {
 public:
  static ObserverPtr<Type> get_integer(Type::TypeID type_id) {
    assert(is_integer_type(type_id));

    // If the type identifier is not specified as `unsigned`, it defaults to
    // `signed`.
    // Therefore, `int` is equivalent to `signed int`.
    if (!(type_id & Type::kUnsigned)) {
      type_id = type_id | Type::kSigned;
    }

    int nbytes;

    if (type_id & Type::kLongLong) {
      nbytes = sizeof(long long);
    } else if (type_id & Type::kLong) {
      nbytes = sizeof(long);
    } else if (type_id & Type::kShort) {
      nbytes = sizeof(short);
    } else if (type_id & Type::kChar) {
      nbytes = sizeof(char);
    } else {
      nbytes = sizeof(int);
    }

    return get_integer(nbytes * __CHAR_BIT__, type_id);
  }

  /// \brief Get an integer type with the specified number of bits.
  ///
  /// When generating the IR, we may don't care the type of integer whether it's
  /// `long long`, `short` or `int` since we have the number of bits
  /// information. But we need to know it's signed or not.
  ///
  /// \param num_bits The number of bits for the desired integer type.
  /// \param type_id A combination of flags indicating whether the type is
  ///                `signed` or `unsigned`, and includes options like `long
  ///                long`, etc.
  /// \return An integer type with the number of bits.
  static ObserverPtr<Type> get_integer(int num_bits, Type::TypeID type_id) {
    auto& ty_mgr = instance();
    auto new_type_id = (type_id & Type::kUnsigned)
                           ? (Type::kInt | Type::kUnsigned)
                           : (Type::kInt);
    auto& int_types = ty_mgr.type_info_[new_type_id];

    auto iter = std::find_if(
        int_types.begin(), int_types.end(), [num_bits](const auto& uptr) {
          return static_cast<const IntegerType*>(uptr.get())->num_bits() ==
                 num_bits;
        });

    if (iter != int_types.end()) {
      return iter->get();
    }

    int_types.push_back(std::make_unique<IntegerType>(num_bits, type_id));

    return int_types.back().get();
  }

  static ObserverPtr<Type> get_char() { return get_predefined(Type::kChar); }

  static ObserverPtr<Type> get_signed_char() {
    return get_predefined(Type::kChar | Type::kSigned);
  }

  static ObserverPtr<Type> get_unsigned_char() {
    return get_predefined(Type::kChar | Type::kUnsigned);
  }

  static ObserverPtr<Type> get_float(Type::TypeID type_id) {
    return get_predefined(type_id);
  }

  /// \brief Get a function type with the specified signature.
  ///
  /// \param return_type The return type of the function.
  /// \param params The parameter types of the function.
  /// \param is_var_arg Indicates if the function is variadic.
  /// \return A pointer to the function type matching the specified signature.
  static ObserverPtr<Type> get_function(
      ObserverPtr<Type> return_type,
      const std::vector<ObserverPtr<Type>>& params, bool is_var_arg) {
    auto& ty_mgr = instance();
    auto& fn_types = ty_mgr.type_info_[Type::kFunction];
    auto fn_match = [&](const auto& uptr) {
      auto fp = static_cast<const FunctionType*>(uptr.get());

      return fp->return_type() == return_type && fp->params() == params &&
             fp->is_var_arg() == is_var_arg;
    };

    auto iter = std::find_if(fn_types.begin(), fn_types.end(), fn_match);

    if (iter != fn_types.end()) {
      return iter->get();
    }

    fn_types.push_back(
        std::make_unique<FunctionType>(return_type, params, is_var_arg));

    return fn_types.back().get();
  }

  /// \brief Get a function type taking no parameters.
  ///
  /// \param return_type The return type of the function.
  /// \param is_var_arg Indicates if the function is variadic.
  /// \return A pointer to the function type matching the specified signature.
  static ObserverPtr<Type> get_function(ObserverPtr<Type> return_type,
                                        bool is_var_arg) {
    return get_function(return_type, {}, is_var_arg);
  }

  /// \brief Get a pointer type for the specified `base` type.
  ///
  /// For example, we could get a `int*` type through:
  ///
  /// \code
  /// auto int_type = TypeMgr::get_primitive(kInt);
  /// auto ptr_to_int_type = TypeMgr::get_pointer(int_type);
  /// \endcode
  ///
  /// \param base Base type.
  /// \return A `pointer` type based on the `base` type.
  static ObserverPtr<Type> get_pointer(ObserverPtr<Type> base) {
    assert(base);

    auto& ty_mgr = instance();
    auto& ptr_types = ty_mgr.type_info_[Type::kPointer];
    auto ptr_match = [base](const auto& uptr) { return uptr->base() == base; };

    auto iter = std::find_if(ptr_types.begin(), ptr_types.end(), ptr_match);

    if (iter != ptr_types.end()) {
      return iter->get();
    }

    ptr_types.push_back(std::make_unique<Type>(Type::kPointer, base));

    return ptr_types.back().get();
  }

  /// \brief Get an array type.
  ///
  /// For example, we could get `int[8]` type through:
  ///
  /// \code
  /// auto int_type = TypeMgr::get_integer(kInt);
  /// auto array_type = TypeMgr::get_array(8, int_type);
  /// \endcode
  ///
  /// \param size The number of elements in array.
  /// \param base The base type.
  /// \return An `array` type based on the `base` type.
  static ObserverPtr<Type> get_array(size_t length, ObserverPtr<Type> base) {
    assert(base);

    auto& ty_mgr = instance();
    auto& arr_types = ty_mgr.type_info_[Type::kArray];
    auto arr_match = [length, base](const auto& uptr) {
      auto arr = static_cast<const ArrayType*>(uptr.get());

      return arr->length() == length && arr->base() == base;
    };

    auto iter = std::find_if(arr_types.begin(), arr_types.end(), arr_match);

    if (iter != arr_types.end()) {
      return iter->get();
    }

    arr_types.push_back(std::make_unique<ArrayType>(length, base));

    return arr_types.back().get();
  }

  static ObserverPtr<Type> get_struct(
      const std::string& name, std::vector<std::unique_ptr<Member>> members) {
    auto& ty_mgr = instance();
    auto& st_types = ty_mgr.type_info_[Type::kStruct];
    auto st_match = [&name](const auto& uptr) {
      auto st = static_cast<const StructType*>(uptr.get());

      return st->name() == name;
    };

    auto iter = std::find_if(st_types.begin(), st_types.end(), st_match);

    if (iter != st_types.end()) {
      return iter->get();
    }

    st_types.push_back(std::make_unique<StructType>(name, std::move(members)));

    return st_types.back().get();
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

 private:
  /// \brief Retrieve a static instance of type manager.
  ///
  /// \return An instance of type manager.
  static TypeMgr& instance() {
    static TypeMgr ty_mgr;

    return ty_mgr;
  }

  static ObserverPtr<Type> get_predefined(Type::TypeID type_id) {
    assert(type_id & (Type::kFloat | Type::kDouble | Type::kChar));

    auto& ty_mgr = instance();
    auto& type_info = ty_mgr.type_info_;

    return type_info[type_id].back().get();
  }

  TypeMgr() { init(); }

  /// \brief Initializes type information with predefined values.
  ///
  /// This includes basic types such as `char`, `float`, and `double`,
  /// which differ from `int` types, the latter being specified by
  /// their respective bit widths.
  void init() {
    for (auto type_id : {Type::kChar, Type::kChar | Type::kSigned,
                         Type::kChar | Type::kUnsigned, Type::kInt,
                         Type::kInt | Type::kUnsigned, Type::kFloat,
                         Type::kDouble, Type::kPointer, Type::kArray,
                         Type::kStruct, Type::kFunction, Type::kLabels}) {
      type_info_.emplace(type_id, std::vector<std::unique_ptr<Type>>{});
    }

    for (auto type_id :
         {Type::kChar, Type::kChar | Type::kSigned,
          Type::kChar | Type::kUnsigned, Type::kFloat, Type::kDouble}) {
      type_info_[type_id].push_back(std::make_unique<Type>(type_id));
    }
  }

  std::map<Type::TypeID, std::vector<std::unique_ptr<Type>>> type_info_;
};

}  // namespace chibicpp