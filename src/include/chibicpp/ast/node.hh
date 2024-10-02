#pragma once

#include <cassert>
#include <limits>
#include <memory>
#include <string>
#include <vector>

#include "chibicpp/ast/type.hh"
#include "chibicpp/lex/token.hh"
#include "chibicpp/parser/symbol_table.hh"

namespace chibicpp {

class Member;

class Var {
 public:
  static constexpr auto kInvalidOffset = -1;

  /// @name Constructor
  /// @{

  /// Construct a local variable.
  ///
  /// \param name The name of the variable.
  /// \param type The type of the variable.
  /// \param offset The offset of the variable from base pointer (rbp).
  Var(const std::string& name, ObserverPtr<Type> type, int offset)
      : name_{name}, type_{type}, offset_{offset} {}

  /// Construct a global variable.
  ///
  /// \param name The name of the variable.
  /// \param type The type of the variable.
  Var(const std::string& name, ObserverPtr<Type> type)
      : Var{name, type, kInvalidOffset} {}

  /// Construct a global variable of string literal type.
  ///
  /// For example, the following code demonstrates creating a label constant
  /// ".LC" in rodata section, the content is "hello world" and the type is
  /// "const char*".
  ///
  /// \code
  /// const char* p = "hello world";
  /// \endcode
  ///
  /// \param label The label of the string literal.
  /// \param content The content of string literal.
  /// \param type The type of the variable.
  Var(const std::string& label, const std::string& content,
      ObserverPtr<Type> type)
      : name_{label}, literal_{content}, type_{type}, offset_{kInvalidOffset} {}

  /// Construct a typedef.
  ///
  /// For example:
  /// Type of `MyInt` is `int` and type_def of `MyInt` is `int`.
  /// Type of `MyInt2 is `MyInt` and type_def of `MyInt2` is `int`.
  /// Type of `MyInt3` is `MyInt2` and type_def of `MyInt3` is `int`.
  ///
  /// This may be helpful to keep track of the type chain.
  ///
  /// \code
  /// typedef int MyInt;
  /// typedef MyInt MyInt2;
  /// typedef MyInt2 MyInt3;
  /// \endcode
  ///
  /// \param name The name of the variable.
  /// \param type The type of the variable.
  /// \param type_def The real type of the variable.
  Var(const std::string& name, ObserverPtr<Type> type,
      ObserverPtr<Type> type_def)
      : name_{name}, type_{type}, type_def_{type_def} {}

  /// @}

  /// \return The name of variable.
  std::string name() const { return name_; }

  /// \return The type of variable.
  ObserverPtr<Type> type() const { return type_; }

  /// Note: This only make sense when this variable is local.
  ///
  /// \return The offset of variable.
  int offset() const {
    assert(is_local());

    return offset_;
  }

  /// \return A string literal.
  const std::string& string_literal() const {
    assert(is_string_literal());

    return literal_;
  }

  /// \return
  ObserverPtr<Type> type_def() const { return type_def_; }

  /// \return `true` if the variable is local, `false` otherwise.
  bool is_local() const { return !is_global(); }

  /// \return `true` if the variable is global, `false` otherwise.
  bool is_global() const { return offset_ == kInvalidOffset; }

  /// \return `true` if the variable is string literal, `false` otherwise.
  bool is_string_literal() const { return is_global() && !literal_.empty(); }

  /// \return `true` if the variable is a typedef, `false` otherwise.
  bool is_typedef() const { return static_cast<bool>(type_def_); }

  /// Update variable's offset.
  ///
  /// \param offset The new offset.
  void set_offset(int offset) { offset_ = offset; }

 private:
  std::string name_;            ///< The name of variable.
  std::string literal_;         ///< String literal.
  ObserverPtr<Type> type_;      ///< Variable type.
  ObserverPtr<Type> type_def_;  ///< Typedef.
  int offset_;                  ///< Local variable offset from rbp register.
};

class Node {
 public:
  /// AST node.
  enum NodeID {
    kAdd,       ///< num + num
    kPtrAdd,    ///< ptr + num or num + ptr
    kSub,       ///< -
    kPtrSub,    ///< ptr - num
    kPtrDiff,   ///< ptr - ptr
    kMul,       ///< *
    kDiv,       ///< /
    kEq,        ///< ==
    kNe,        ///< !=
    kLt,        ///< <
    kLe,        ///< <=
    kAssign,    ///< =
    kMember,    ///< . (struct member access)
    kAddr,      ///< unary &
    kDeref,     ///< unary *
    kReturn,    ///< "return"
    kIf,        ///< "if"
    kWhile,     ///< "while"
    kFor,       ///< "for"
    kBlock,     ///< {...}
    kFunCall,   ///< Function call
    kExprStmt,  ///< Expression statement
    kStmtExpr,  ///< Statement expression
    kVar,       ///< Variable
    kNum,       ///< Integer
    kEmpty,     ///< Empty statement
  };

  explicit Node(NodeID id) : id_{id} {}

  NodeID id() const { return id_; }

  ObserverPtr<Type> type() const { return type_; }

  void update_type();

  /// Make a deep clone of the node.
  ///
  /// \return
  std::unique_ptr<Node> clone() const;

  static std::unique_ptr<Node> make_empty() {
    return std::make_unique<Node>(Node::kEmpty);
  }

  /// Make a node type of variable.
  ///
  /// \param var A pointer to variable.
  /// \return
  static std::unique_ptr<Node> make_var(ObserverPtr<Var> var) {
    auto node = std::make_unique<Node>(Node::kVar);

    node->var_ = var;
    node->update_type();

    return node;
  }

  static std::unique_ptr<Node> make_number(long v) {
    auto node = std::make_unique<Node>(Node::kNum);

    node->num_ = v;
    node->update_type();

    return node;
  }

  static std::unique_ptr<Node> make_binary(NodeID id, std::unique_ptr<Node> lhs,
                                           std::unique_ptr<Node> rhs) {
    auto node = std::make_unique<Node>(id);

    node->lhs_ = std::move(lhs);
    node->rhs_ = std::move(rhs);
    node->update_type();

    return node;
  }

  static std::unique_ptr<Node> make_unary(NodeID id,
                                          std::unique_ptr<Node> expr) {
    auto node = make_binary(id, std::move(expr), nullptr);

    node->update_type();

    return node;
  }

  static std::unique_ptr<Node> make_member(std::unique_ptr<Node> expr,
                                           ObserverPtr<Member> member) {
    auto node = std::make_unique<Node>(Node::kMember);

    node->lhs_ = std::move(expr);
    node->member_ = member;
    node->update_type();

    return node;
  }

  static std::unique_ptr<Node> make_if(std::unique_ptr<Node> cond,
                                       std::unique_ptr<Node> then,
                                       std::unique_ptr<Node> els) {
    auto node = std::make_unique<Node>(Node::kIf);

    node->cond_ = std::move(cond);
    node->then_ = std::move(then);
    node->else_ = std::move(els);
    node->update_type();

    return node;
  }

  static std::unique_ptr<Node> make_while(std::unique_ptr<Node> cond,
                                          std::unique_ptr<Node> then) {
    auto node = std::make_unique<Node>(Node::kWhile);

    node->cond_ = std::move(cond);
    node->then_ = std::move(then);
    node->update_type();

    return node;
  }

  static std::unique_ptr<Node> make_for(std::unique_ptr<Node> init,
                                        std::unique_ptr<Node> cond,
                                        std::unique_ptr<Node> inc,
                                        std::unique_ptr<Node> then) {
    auto node = std::make_unique<Node>(Node::kFor);

    node->init_ = std::move(init);
    node->cond_ = std::move(cond);
    node->inc_ = std::move(inc);
    node->then_ = std::move(then);
    node->update_type();

    return node;
  }

  static std::unique_ptr<Node> make_function_call(
      const std::string& func_name, std::vector<std::unique_ptr<Node>> args) {
    auto node = std::make_unique<Node>(Node::kFunCall);

    node->func_name_ = func_name;
    node->args_ = std::move(args);
    node->update_type();

    return node;
  }

  static std::unique_ptr<Node> make_stmt_expr(
      std::vector<std::unique_ptr<Node>> body) {
    auto node = std::make_unique<Node>(Node::kStmtExpr);

    // The last node is an expression statement.
    // But we need its expression as the return type.
    auto& lhs = body.back()->lhs_;
    body.back() = std::move(lhs);
    node->body_ = std::move(body);
    node->update_type();

    return node;
  }

  static std::unique_ptr<Node> make_block(
      std::vector<std::unique_ptr<Node>> body) {
    auto node = std::make_unique<Node>(Node::kBlock);

    node->body_ = std::move(body);

    return node;
  }

  static std::unique_ptr<Node> make_add(std::unique_ptr<Node> lhs,
                                        std::unique_ptr<Node> rhs) {
    if (lhs->type()->is_integer() && rhs->type()->is_integer()) {
      return make_binary(Node::kAdd, std::move(lhs), std::move(rhs));
    }

    if (lhs->type()->base() && rhs->type()->is_integer()) {
      return make_binary(Node::kPtrAdd, std::move(lhs), std::move(rhs));
    }

    if (lhs->type()->is_integer() && rhs->type()->base()) {
      return make_binary(Node::kPtrAdd, std::move(rhs), std::move(lhs));
    }

    __builtin_unreachable();
  }

  static std::unique_ptr<Node> make_sub(std::unique_ptr<Node> lhs,
                                        std::unique_ptr<Node> rhs) {
    if (lhs->type()->is_integer() && rhs->type()->is_integer()) {
      return make_binary(Node::kSub, std::move(lhs), std::move(rhs));
    }

    if (lhs->type()->base() && rhs->type()->is_integer()) {
      return make_binary(Node::kPtrSub, std::move(lhs), std::move(rhs));
    }

    if (lhs->type()->base() && rhs->type()->base()) {
      return make_binary(Node::kPtrDiff, std::move(lhs), std::move(rhs));
    }

    __builtin_unreachable();
  }

  size_t block_size() const { return body_.size(); }
  auto block_begin() { return body_.begin(); }
  auto block_begin() const { return body_.begin(); }
  auto block_end() { return body_.end(); }
  auto block_end() const { return body_.end(); }

  size_t args_size() const { return args_.size(); }
  auto args_begin() { return args_.begin(); }
  auto args_begin() const { return args_.begin(); }
  auto args_end() { return args_.end(); }
  auto args_end() const { return args_.end(); }

  ObserverPtr<Node> lhs() const { return lhs_.get(); }
  ObserverPtr<Node> rhs() const { return rhs_.get(); }
  ObserverPtr<Node> cond() const { return cond_.get(); }
  ObserverPtr<Node> then() const { return then_.get(); }
  ObserverPtr<Node> els() const { return else_.get(); }
  ObserverPtr<Node> init() const { return init_.get(); }
  ObserverPtr<Node> inc() const { return inc_.get(); }

  ObserverPtr<Var> var() const { return var_; }
  ObserverPtr<Member> member() const { return member_; }
  long number() const { return num_; }
  const std::string& func_name() const { return func_name_; }

 private:
  NodeID id_;                  ///< Node kind.
  ObserverPtr<Type> type_;     ///< Type, e.g. int or pointer to int
  ObserverPtr<Var> var_;       ///< Used if kind == kVar.
  long num_;                   ///< Used if kind == kNum.
  std::unique_ptr<Node> lhs_;  ///< Left-hand node.
  std::unique_ptr<Node> rhs_;  ///< Right-hand node.

  /// "if", "while" or "for" statement.
  std::unique_ptr<Node> cond_;
  std::unique_ptr<Node> then_;
  std::unique_ptr<Node> else_;
  std::unique_ptr<Node> init_;
  std::unique_ptr<Node> inc_;

  /// Block or statement expression.
  std::vector<std::unique_ptr<Node>> body_;

  /// Function call.
  std::string func_name_;
  std::vector<std::unique_ptr<Node>> args_;

  /// Struct member access.
  ObserverPtr<Member> member_;
};

class SymbolTable;

class Function : public SymbolTableIterator {
 public:
  struct Prototype {
    ObserverPtr<Type> ret_type;            ///< Return type.
    std::string fname;                     ///< Function name.
    std::vector<ObserverPtr<Var>> params;  ///< Parameters.
  };

  /// Construct a function with the specified function prototype, body and
  /// symbol table.
  ///
  /// \param prototype The prototype of the function.
  /// \param body The function body.
  /// \param locals Local variables within the function.
  Function(std::unique_ptr<Prototype> prototype,
           std::vector<std::unique_ptr<Node>> body,
           std::unique_ptr<SymbolTable> sym_table)
      : SymbolTableIterator{std::move(sym_table)},
        stack_size_{},
        prototype_{std::move(prototype)},
        body_{std::move(body)} {
    update_var_offset();
  }

  /// Get the stack size required for this function.
  ///
  /// For example, consider a function `foo`. The approximate stack size
  /// required for this function is 3 * sizeof(int).
  /// @code
  /// void foo() { int a = 1; int b = 2; int c = 3; }
  /// @endcode
  ///
  /// \return Stack size.
  int stack_size() const { return stack_size_; }

  /// Get function name.
  ///
  /// \return
  std::string name() const { return prototype_->fname; }

  /// @name Iterators
  /// @{

  /// Returns an iterator to the beginning of the parameter list.
  ///
  /// \return An iterator pointing to the first element of the parameters.
  auto param_begin() { return prototype_->params.begin(); }
  auto param_begin() const { return prototype_->params.begin(); }

  /// Returns an iterator to the end of the parameter list.
  ///
  /// \return An iterator pointing to the end (past-the-last) element of the
  ///         parameters.
  auto param_end() { return prototype_->params.end(); }
  auto param_end() const { return prototype_->params.end(); }

  /// Returns an iterator to the beginning of the function body.
  ///
  /// \return An iterator pointing to the first element of the function body.
  auto body_begin() { return body_.begin(); }
  auto body_begin() const { return body_.begin(); }

  /// Returns an iterator to the end of the function body.
  ///
  /// \return An iterator pointing to the end (past-the-last) element of the
  ///         function body.
  auto body_end() { return body_.end(); }
  auto body_end() const { return body_.end(); }

  /// @}

  /// This method is used for debugging purposes.
  ///
  /// It prints the variables within this function along with their
  /// corresponding types.
  void dump_var_with_typeinfo(std::ostream& os) const {
    for (auto iter = var_begin(); iter != var_end(); ++iter) {
      auto var = iter->get();

      os << var->name() << ':' << var->type()->to_string() << std::endl;
    }
  }

 private:
  void update_var_offset() {
    int offset = 0;

    // TODO(gc): Investigate why we place variables that are used far away
    // from their declaration onto the upper stack. This could affect
    // memory layout and access patterns?
    for (auto iter = var_rbegin(); iter != var_rend(); ++iter) {
      auto type = iter->get()->type();

      offset = align_to(offset, type->size_in_bytes());
      offset += type->size_in_bytes();
      iter->get()->set_offset(offset);
    }

    stack_size_ = offset;
  }

  int stack_size_;
  std::unique_ptr<Prototype> prototype_;
  std::vector<std::unique_ptr<Node>> body_;
};

class Program : public SymbolTableIterator {
 public:
  Program() = default;
  explicit Program(std::vector<std::unique_ptr<Function>> funcs,
                   std::unique_ptr<SymbolTable> sym_table)
      : SymbolTableIterator{std::move(sym_table)}, funcs_{std::move(funcs)} {}

  /// \name Iterators
  /// @{

  /// Returns an iterator to the beginning of the functions container.
  ///
  /// \return An iterator to the first element of the `funcs_` container.
  auto func_begin() { return funcs_.begin(); }
  auto func_begin() const { return funcs_.begin(); }

  /// \return An iterator to one past the last element of the `funcs_`
  ///         container.
  auto func_end() { return funcs_.end(); }
  auto func_end() const { return funcs_.end(); }

  /// @}

 private:
  std::vector<std::unique_ptr<Function>> funcs_;
};

}  // namespace chibicpp