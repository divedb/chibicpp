#pragma once

#include <cassert>
#include <limits>
#include <memory>
#include <string>
#include <vector>

#include "chibicpp/ast/type.hh"
#include "chibicpp/lex/token.hh"

namespace chibicpp {

class Member;

class Var {
 public:
  /// Bit shift for local variable's offset.
  static constexpr auto kOffsetShift = 1;

  /// This mask is used to determine if a variable is global
  /// by checking the lowest bit of the offset.
  static constexpr auto kGlobalVarTypeMask = 0x01;

  /// @name Constructor
  /// @{

  /// \brief Construct a local variable.
  ///
  /// \param name The name of the variable.
  /// \param type The type of the variable.
  /// \param offset The offset of the variable from base pointer (rbp).
  Var(const std::string& name, ObserverPtr<Type> type, int offset)
      : name_{name}, type_{type}, offset_{offset << kOffsetShift} {}

  /// \brief Construct a global variable.
  ///
  /// \param name The name of the variable.
  /// \param type The type of the variable.
  Var(const std::string& name, ObserverPtr<Type> type)
      : name_{name}, type_{type}, offset_{kGlobalVarTypeMask} {}

  /// \brief Construct a global variable of string literal type.
  ///
  /// \param label The label of the string literal.
  /// \param content The content of string literal.
  /// \param type The type of the variable.
  Var(const std::string& label, const std::string& content,
      ObserverPtr<Type> type)
      : name_{label},
        literal_{content},
        type_{type},
        offset_{kGlobalVarTypeMask} {}

  /// @}

  /// \brief Get name of the variable.
  ///
  /// \return Name of variable.
  std::string name() const { return name_; }

  /// \brief Get type of the variable.
  ///
  /// Note: This is non-owning pointer.
  ///
  /// \return Type of variable.
  ObserverPtr<Type> type() const { return type_; }

  /// \brief Get offset of the variable.
  ///
  /// Note: This only make sense when this variable is local.
  ///
  /// \return Offset of variable.
  int offset() const {
    assert(is_local());

    return offset_ >> kOffsetShift;
  }

  /// \brief Get string literal of the variable.
  ///
  /// \return A string literal.
  const std::string& string_literal() const {
    assert(is_string_literal());

    return literal_;
  }

  /// \brief Check if the variable is local.
  ///
  /// \return `true` if the variable is local, otherwise `false`.
  bool is_local() const { return !is_global(); }

  /// \brief Check if the variable is global.
  ///
  /// \return `true` if the variable is global, otherwise `false`.
  bool is_global() const { return offset_ & kGlobalVarTypeMask; }

  /// \brief Check if the variable is string literal.
  ///
  /// \return `true` if the variable is string literal, otherwise `false`.
  bool is_string_literal() const { return is_global() && !literal_.empty(); }

  /// \brief Update variable's offset.
  ///
  /// \param offset New offset.
  void set_offset(int offset) { offset_ = offset << kOffsetShift; }

 private:
  std::string name_;        ///< Do we shared the memory buffer
  std::string literal_;     ///< String literal.
  ObserverPtr<Type> type_;  ///< Variable type.
  int offset_;              ///< Local variable offset from rbp register.
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

  /// \brief Make a deep clone of the node.
  ///
  /// \return
  std::unique_ptr<Node> clone() const;

  /// \brief Make a node type of variable.
  ///
  /// \param var A pointer to variable.
  /// \return
  static std::unique_ptr<Node> make_a_var(ObserverPtr<Var> var) {
    auto node = std::make_unique<Node>(Node::kVar);

    node->var_ = var;
    node->update_type();

    return node;
  }

  static std::unique_ptr<Node> make_a_number(long v) {
    auto node = std::make_unique<Node>(Node::kNum);

    node->num_ = v;
    node->update_type();

    return node;
  }

  static std::unique_ptr<Node> make_a_binary(NodeID id,
                                             std::unique_ptr<Node> lhs,
                                             std::unique_ptr<Node> rhs) {
    auto node = std::make_unique<Node>(id);

    node->lhs_ = std::move(lhs);
    node->rhs_ = std::move(rhs);
    node->update_type();

    return node;
  }

  static std::unique_ptr<Node> make_a_unary(NodeID id,
                                            std::unique_ptr<Node> expr) {
    auto node = make_a_binary(id, std::move(expr), nullptr);

    node->update_type();

    return node;
  }

  static std::unique_ptr<Node> make_a_member(std::unique_ptr<Node> expr,
                                             ObserverPtr<Member> member) {
    auto node = std::make_unique<Node>(Node::kMember);

    node->lhs_ = std::move(expr);
    node->member_ = member;
    node->update_type();

    return node;
  }

  static std::unique_ptr<Node> make_a_if(std::unique_ptr<Node> cond,
                                         std::unique_ptr<Node> then,
                                         std::unique_ptr<Node> els) {
    auto node = std::make_unique<Node>(Node::kIf);

    node->cond_ = std::move(cond);
    node->then_ = std::move(then);
    node->else_ = std::move(els);
    node->update_type();

    return node;
  }

  static std::unique_ptr<Node> make_a_while(std::unique_ptr<Node> cond,
                                            std::unique_ptr<Node> then) {
    auto node = std::make_unique<Node>(Node::kWhile);

    node->cond_ = std::move(cond);
    node->then_ = std::move(then);
    node->update_type();

    return node;
  }

  static std::unique_ptr<Node> make_a_for(std::unique_ptr<Node> init,
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

  static std::unique_ptr<Node> make_a_function_call(
      const std::string& func_name, std::vector<std::unique_ptr<Node>> args) {
    auto node = std::make_unique<Node>(Node::kFunCall);

    node->func_name_ = func_name;
    node->args_ = std::move(args);
    node->update_type();

    return node;
  }

  static std::unique_ptr<Node> make_a_stmt_expr(
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

  static std::unique_ptr<Node> make_a_block(
      std::vector<std::unique_ptr<Node>> body) {
    auto node = std::make_unique<Node>(Node::kBlock);

    node->body_ = std::move(body);

    return node;
  }

  static std::unique_ptr<Node> make_add(std::unique_ptr<Node> lhs,
                                        std::unique_ptr<Node> rhs) {
    if (lhs->type()->is_integer() && rhs->type()->is_integer()) {
      return make_a_binary(Node::kAdd, std::move(lhs), std::move(rhs));
    }

    if (lhs->type()->base() && rhs->type()->is_integer()) {
      return make_a_binary(Node::kPtrAdd, std::move(lhs), std::move(rhs));
    }

    if (lhs->type()->is_integer() && rhs->type()->base()) {
      return make_a_binary(Node::kPtrAdd, std::move(rhs), std::move(lhs));
    }

    __builtin_unreachable();
  }

  static std::unique_ptr<Node> make_sub(std::unique_ptr<Node> lhs,
                                        std::unique_ptr<Node> rhs) {
    if (lhs->type()->is_integer() && rhs->type()->is_integer()) {
      return make_a_binary(Node::kSub, std::move(lhs), std::move(rhs));
    }

    if (lhs->type()->base() && rhs->type()->is_integer()) {
      return make_a_binary(Node::kPtrSub, std::move(lhs), std::move(rhs));
    }

    if (lhs->type()->base() && rhs->type()->base()) {
      return make_a_binary(Node::kPtrDiff, std::move(lhs), std::move(rhs));
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

class Function {
 public:
  struct Prototype {
    ObserverPtr<Type> ret_type;            ///< Return type.
    std::string fname;                     ///< Function name.
    std::vector<ObserverPtr<Var>> params;  ///< Parameters.
  };

  /// \brief Construct a function with the specified function prototype, body
  ///        and local variables.
  ///
  /// \param prototype The prototype of the function.
  /// \param body The function body.
  /// \param locals Local variables within the function.
  Function(std::unique_ptr<Prototype> prototype,
           std::vector<std::unique_ptr<Node>> body,
           std::vector<std::unique_ptr<Var>> locals,
           std::vector<std::unique_ptr<Var>> string_literals,
           std::vector<std::unique_ptr<Var>> statics)
      : stack_size_{0},
        prototype_{std::move(prototype)},
        body_{std::move(body)},
        locals_{std::move(locals)},
        string_literals_{std::move(string_literals)},
        statics_{std::move(statics)} {
    update_var_offset();
  }

  /// \brief Get the stack size required for this function.
  ///
  /// For example, consider a function `foo`. The approximate stack size
  /// required for this function is 3 * sizeof(int).
  /// @code
  /// void foo() { int a = 1; int b = 2; int c = 3; }
  /// @endcode
  ///
  /// \return Stack size.
  int stack_size() const { return stack_size_; }

  /// \brief Get function name.
  ///
  /// \return
  std::string name() const { return prototype_->fname; }

  /// @name Iterators
  /// @{

  /// \brief Returns an iterator to the beginning of the parameter list.
  ///
  /// \return An iterator pointing to the first element of the parameters.
  auto param_begin() { return prototype_->params.begin(); }
  auto param_begin() const { return prototype_->params.begin(); }

  /// \brief Returns an iterator to the end of the parameter list.
  ///
  /// \return An iterator pointing to the end (past-the-last) element of the
  ///         parameters.
  auto param_end() { return prototype_->params.end(); }
  auto param_end() const { return prototype_->params.end(); }

  /// \brief Returns an iterator to the beginning of the function body.
  ///
  /// \return An iterator pointing to the first element of the function body.
  auto body_begin() { return body_.begin(); }
  auto body_begin() const { return body_.begin(); }

  /// \brief Returns an iterator to the end of the function body.
  ///
  /// \return An iterator pointing to the end (past-the-last) element of the
  ///         function body.
  auto body_end() { return body_.end(); }
  auto body_end() const { return body_.end(); }

  auto string_literal_begin() { return string_literals_.begin(); }
  auto string_literal_begin() const { return string_literals_.begin(); }
  auto string_literal_end() { return string_literals_.end(); }
  auto string_literal_end() const { return string_literals_.end(); }

  auto static_begin() { return statics_.begin(); }
  auto static_begin() const { return statics_.begin(); }
  auto static_end() { return statics_.end(); }
  auto static_end() const { return statics_.end(); }

  /// @}

  /// \brief This method is used for debugging purposes.
  ///
  /// It prints the variables within this function along with their
  /// corresponding types.
  void dump_var_with_typeinfo(std::ostream& os) const {
    for (auto& var : locals_) {
      os << var->name() << ':' << var->type()->to_string() << std::endl;
    }
  }

  /// \brief Get number of local variables inside this function.
  ///
  /// \return Number of local variables.
  int local_var_count() const { return locals_.size(); }

 private:
  void update_var_offset() {
    int offset = 0;

    // TODO(gc): Investigate why we place variables that are used far away
    // from their declaration onto the upper stack. This could affect
    // memory layout and access patterns?
    for (auto it = locals_.rbegin(); it != locals_.rend(); ++it) {
      auto type = it->get()->type();

      offset = align_to(offset, type->size_in_bytes());
      offset += type->size_in_bytes();
      it->get()->set_offset(offset);
    }

    stack_size_ = offset;
  }

  int stack_size_;
  std::unique_ptr<Prototype> prototype_;
  std::vector<std::unique_ptr<Node>> body_;
  std::vector<std::unique_ptr<Var>> locals_;           ///< The local variables.
  std::vector<std::unique_ptr<Var>> string_literals_;  ///< The string literals.
  std::vector<std::unique_ptr<Var>> statics_;  ///< The static variables.
};

class Program {
 public:
  Program() = default;
  explicit Program(std::vector<std::unique_ptr<Var>> globals,
                   std::vector<std::unique_ptr<Function>> other)
      : globals_(std::move(globals)), funcs_(std::move(other)) {}

  /// \name Iterators.
  /// @{

  /// \brief Returns an iterator to the beginning of the functions container.
  ///
  /// \return An iterator to the first element of the `funcs_` container.
  auto func_begin() { return funcs_.begin(); }
  auto func_begin() const { return funcs_.begin(); }

  /// \brief Returns an iterator to the end of the functions container.
  ///
  /// \return An iterator to one past the last element of the `funcs_`
  ///         container.
  auto func_end() { return funcs_.end(); }
  auto func_end() const { return funcs_.end(); }

  /// \brief Returns an iterator to the beginning of the global variables
  ///        container.
  /// \return An iterator to the first element of the `globals_` container.
  auto global_begin() { return globals_.begin(); }
  auto global_begin() const { return globals_.begin(); }

  /// \brief Returns an iterator to the end of the global variables container.
  ///
  /// \return An iterator to one past the last element of the `globals_`
  ///         container.
  auto global_end() { return globals_.end(); }
  auto global_end() const { return globals_.end(); }

  /// @}

 private:
  std::vector<std::unique_ptr<Var>> globals_;  ///< The global variables.
  std::vector<std::unique_ptr<Function>> funcs_;
};

}  // namespace chibicpp