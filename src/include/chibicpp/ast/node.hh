#pragma once

#include <cassert>
#include <memory>
#include <string>
#include <vector>

#include "chibicpp/ast/ast_context.hh"
#include "chibicpp/ast/type.hh"
#include "chibicpp/ast/visitor.hh"
#include "chibicpp/lex/token.hh"

namespace chibicpp {

struct Var {
  std::string name;  ///< Do we shared the memory buffer
  Type* type;        ///< Variable type.
  int offset;        ///< Local variable offset from RBP.
  bool is_local;     ///< Local or global variable.
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
  kAddr,      ///< unary &
  kDeref,     ///< unary *
  kReturn,    ///< "return"
  kIf,        ///< "if"
  kWhile,     ///< "while"
  kFor,       ///< "for"
  kBlock,     ///< {...}
  kFunCall,   ///< Function call
  kExprStmt,  ///< Expression statement
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

  NodeKind kind;              ///< Node kind.
  Type* type{};               ///< Type, e.g. int or pointer to int
  std::unique_ptr<Node> lhs;  ///< Left-hand node.
  std::unique_ptr<Node> rhs;  ///< Right-hand node.

  /// "if", "while" or "for" statement.
  std::unique_ptr<Node> cond;
  std::unique_ptr<Node> then;
  std::unique_ptr<Node> els;
  std::unique_ptr<Node> init;
  std::unique_ptr<Node> inc;

  /// Block statement.
  std::vector<std::unique_ptr<Node>> body;

  /// Function call.
  std::string func_name;
  std::vector<std::unique_ptr<Node>> args;

  Var* var;  ///< Used if kind == kVar.
  long val;  ///< Used if kind == kNum.
};

class Function {
 public:
  struct Prototype {
    Type* ret_type;            ///< Return type.
    std::string fname;         ///< Function name.
    std::vector<Var*> params;  ///< Parameters.
  };

  /// \brief Construct a function with the given function prototype, body and
  ///        local variables.
  ///
  /// \param prototype The prototype of the function.
  /// \param body The function body.
  /// \param locals Local variables within the function.
  Function(Prototype const& prototype, std::vector<std::unique_ptr<Node>> body,
           std::vector<std::unique_ptr<Var>> locals)
      : stack_size_(0),
        prototype_(prototype),
        body_(std::move(body)),
        locals_(std::move(locals)) {
    update_offset();
  }

  void accept(AstVisitor& visitor, AstContext& context) {
    auto& params = prototype_.params;

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
  std::string name() const { return prototype_.fname; }

  /// @name Iterators
  /// @{

  /// \brief Returns an iterator to the beginning of the parameter list.
  ///
  /// \return An iterator pointing to the first element of the parameters.
  auto param_begin() { return prototype_.params.begin(); }
  auto param_begin() const { return prototype_.params.begin(); }

  /// \brief Returns an iterator to the end of the parameter list.
  ///
  /// \return An iterator pointing to the end (past-the-last) element of the
  ///         parameters.
  auto param_end() { return prototype_.params.end(); }
  auto param_end() const { return prototype_.params.end(); }

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
      os << var->name << ':' << var->type->to_string() << std::endl;
    }
  }

 private:
  void update_offset() {
    int offset = 0;

    // TODO(gc): Investigate why we place variables that are used far away
    // from their declaration onto the upper stack. This could affect
    // memory layout and access patterns?
    for (auto it = locals_.rbegin(); it != locals_.rend(); ++it) {
      offset += it->get()->type->size_in_bytes();
      it->get()->offset = offset;
    }

    stack_size_ = offset;
  }

  int stack_size_;
  Prototype prototype_;
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

inline std::unique_ptr<Node> make_a_var(Var* var) {
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