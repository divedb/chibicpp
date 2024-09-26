#pragma once

#include <cassert>
#include <limits>
#include <memory>
#include <string>
#include <vector>

#include "chibicpp/ast/ast_context.hh"
#include "chibicpp/ast/type.hh"
#include "chibicpp/ast/visitor.hh"
#include "chibicpp/lex/token.hh"
#include "chibicpp/util/observer_ptr.hh"

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
  Var(std::string const& name, ObserverPtr<Type> type)
      : name_{name}, type_{type}, offset_{kGlobalVarTypeMask} {}

  /// \brief Construct a global variable of string literal type.
  ///
  /// \param name The name of the string literal.
  /// \param content The content of string literal.
  /// \param type The type of the variable.
  Var(std::string const& name, std::string const& content,
      ObserverPtr<Type> type)
      : name_{name},
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

/// AST node.
enum class NodeKind {
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

struct Node {
  explicit Node(NodeKind kind) : kind(kind) {}

  /// \brief Check if this node is an integer.
  ///
  /// \return `true` if this node is an integer, otherwise false.
  bool is_integer() const;

  /// \brief Check if this node has a base type.
  ///
  /// For pointer type, like `int*`, the base type is `int`.
  ///
  /// \return The base type of this node if it exists, otherwise nullptr.
  bool has_base() const;

  auto block_begin() { return body.begin(); }
  auto block_begin() const { return body.begin(); }
  auto block_end() { return body.end(); }
  auto block_end() const { return body.end(); }
  auto args_begin() { return args.begin(); }
  auto args_begin() const { return args.begin(); }
  auto args_end() { return args.end(); }
  auto args_end() const { return args.end(); }

  std::unique_ptr<Node> clone() {
    auto cloned = std::make_unique<Node>(kind);

    cloned->type = this->type;
    cloned->func_name = this->func_name;
    cloned->val = this->val;
    cloned->var = this->var;

    if (lhs) {
      cloned->lhs = lhs->clone();
    }
    if (rhs) {
      cloned->rhs = rhs->clone();
    }
    if (cond) {
      cloned->cond = cond->clone();
    }
    if (then) {
      cloned->then = then->clone();
    }
    if (els) {
      cloned->els = els->clone();
    }
    if (init) {
      cloned->init = init->clone();
    }
    if (inc) {
      cloned->inc = inc->clone();
    }

    for (auto& n : body) {
      cloned->body.push_back(n->clone());
    }

    for (auto& arg : args) {
      cloned->args.push_back(arg->clone());
    }

    return cloned;
  }

  NodeKind kind;              ///< Node kind.
  ObserverPtr<Type> type;     ///< Type, e.g. int or pointer to int
  std::unique_ptr<Node> lhs;  ///< Left-hand node.
  std::unique_ptr<Node> rhs;  ///< Right-hand node.

  /// "if", "while" or "for" statement.
  std::unique_ptr<Node> cond;
  std::unique_ptr<Node> then;
  std::unique_ptr<Node> els;
  std::unique_ptr<Node> init;
  std::unique_ptr<Node> inc;

  /// Block or statement expression.
  std::vector<std::unique_ptr<Node>> body;

  /// Struct member access.
  ObserverPtr<Member> member;

  /// Function call.
  std::string func_name;
  std::vector<std::unique_ptr<Node>> args;

  ObserverPtr<Var> var;  ///< Used if kind == kVar.
  long val;              ///< Used if kind == kNum.
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
           std::vector<std::unique_ptr<Var>> locals)
      : stack_size_{0},
        prototype_{std::move(prototype)},
        body_{std::move(body)},
        locals_{std::move(locals)} {
    update_var_offset();
  }

  void accept(AstVisitor& visitor, AstContext& context) {
    auto& params = prototype_->params;

    for (auto i = 0; i < static_cast<int>(params.size()); ++i) {
      visitor.visit_function_params(params[i], i, context);
    }

    for (auto& node : body_) {
      visitor.visit_function_body(node.get(), context);
    }
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
      offset += it->get()->type()->size_in_bytes();
      it->get()->set_offset(offset);
    }

    stack_size_ = offset;
  }

  int stack_size_;
  std::unique_ptr<Prototype> prototype_;
  std::vector<std::unique_ptr<Node>> body_;
  std::vector<std::unique_ptr<Var>> locals_;
};

class Program {
 public:
  Program() = default;
  explicit Program(std::vector<std::unique_ptr<Var>> globals,
                   std::vector<std::unique_ptr<Function>> other)
      : globals_(std::move(globals)), funcs_(std::move(other)) {}

  void accept(AstVisitor& visitor, AstContext& context) {
    for (auto& var : globals_) {
      visitor.visit_global(var.get(), context);
    }

    for (auto& func : funcs_) {
      visitor.visit_function(func.get(), context);
    }
  }

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
  std::vector<std::unique_ptr<Var>> globals_;
  std::vector<std::unique_ptr<Function>> funcs_;
};

inline std::unique_ptr<Node> make_a_var(ObserverPtr<Var> var) {
  auto node = std::make_unique<Node>(NodeKind::kVar);
  node->var = var;

  return node;
}

inline std::unique_ptr<Node> make_a_number(long v) {
  auto node = std::make_unique<Node>(NodeKind::kNum);
  node->val = v;

  return node;
}

inline std::unique_ptr<Node> make_a_binary(NodeKind kind,
                                           std::unique_ptr<Node> lhs,
                                           std::unique_ptr<Node> rhs) {
  auto node = std::make_unique<Node>(kind);
  node->lhs = std::move(lhs);
  node->rhs = std::move(rhs);

  return node;
}

inline std::unique_ptr<Node> make_a_unary(NodeKind kind,
                                          std::unique_ptr<Node> expr) {
  auto node = std::make_unique<Node>(kind);
  node->lhs = std::move(expr);

  return node;
}

}  // namespace chibicpp