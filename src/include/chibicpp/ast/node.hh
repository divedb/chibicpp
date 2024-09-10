#pragma once

#include <cassert>
#include <deque>
#include <memory>
#include <string>
#include <vector>

#include "chibicpp/ast/ast_context.hh"
#include "chibicpp/ast/visitor.hh"
#include "chibicpp/lex/token.hh"

namespace chibicpp {

struct Var {
  std::string name;  ///< Do we shared the memory buffer
  int offset;        ///< Offset from RBP.
};

/// AST node.
enum class NodeKind {
  kAdd,       ///< +
  kSub,       ///< -
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
};

inline std::string node_kind_to_string(NodeKind kind) {
  switch (kind) {
    case NodeKind::kAdd:
      return "+";
    case NodeKind::kSub:
      return "-";
    case NodeKind::kMul:
      return "*";
    case NodeKind::kDiv:
      return "/";
    case NodeKind::kEq:
      return "==";
    case NodeKind::kNe:
      return "!=";
    case NodeKind::kLt:
      return "<";
    case NodeKind::kLe:
      return "<=";
    case NodeKind::kAssign:
      return "=";
    case NodeKind::kReturn:
      return "return";
    case NodeKind::kIf:
      return "if";
    case NodeKind::kExprStmt:
      return "kExprStmt";
    case NodeKind::kVar:
      return "kVar";
    case NodeKind::kNum:
      return "kNum";
    default:
      return "Unknown";
  }
}

struct Node {
  explicit Node(NodeKind kind) : kind(kind) {}

  NodeKind kind;  ///< Node kind.

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

inline bool is_binary_operator(NodeKind kind) {
  return kind == NodeKind::kAdd || kind == NodeKind::kSub ||
         kind == NodeKind::kMul || kind == NodeKind::kDiv ||
         kind == NodeKind::kEq || kind == NodeKind::kNe ||
         kind == NodeKind::kLt || kind == NodeKind::kLe ||
         kind == NodeKind::kAssign;
}

inline void dump(std::ostream& os, Node* node) {
  if (node == nullptr) {
    return;
  }

  auto kind = node->kind;

  if (kind == NodeKind::kNum) {
    os << node->val;
  } else if (is_binary_operator(kind)) {
    dump(os, node->lhs.get());
    os << node_kind_to_string(kind);
    dump(os, node->rhs.get());
  } else if (kind == NodeKind::kReturn) {
    os << node_kind_to_string(kind);
    dump(os, node->lhs.get());
  } else if (kind == NodeKind::kIf) {
    os << "if (";
    dump(os, node->cond.get());
    os << ")\n{";
    dump(os, node->then.get());
    os << "\n}";

    if (node->els) {
      os << "else {";
      dump(os, node->els.get());
      os << "}";
    }
  } else if (kind == NodeKind::kExprStmt) {
    dump(os, node->lhs.get());
  } else if (kind == NodeKind::kVar) {
    os << node->var->name;
  } else {
    assert(false);
  }
}

class Function {
 public:
  Function(std::string name, std::vector<std::unique_ptr<Node>> nodes,
           std::vector<Var*> params, std::deque<std::unique_ptr<Var>> locals)
      : stack_size_(0),
        name_(std::move(name)),
        nodes_(std::move(nodes)),
        params_(std::move(params)),
        locals_(std::move(locals)) {}

  int stack_size() const { return stack_size_; }
  void accept(AstVisitor& visitor, AstContext& context) {
    /// We need to update the AST context to reflect that we are currently
    /// visiting this function.
    context.func = this;

    for (auto& node : nodes_) {
      visitor.visit_node(node.get(), context);
    }
  }

  /// \brief Not a good way to do it.
  ///        TODO(gc): fix it.
  void update_offset() {
    int offset = 0;

    for (auto& var : locals_) {
      offset += 8;
      var->offset = offset;
    }

    stack_size_ = offset;
  }

  /// \brief Get function name.
  ///
  /// \return
  std::string name() const { return name_; }

  /// @name Parameter Iterators
  /// @{

  /// \brief
  ///
  /// \return
  auto param_begin() { return params_.begin(); }
  auto param_begin() const { return params_.begin(); }
  auto param_end() { return params_.end(); }
  auto param_end() const { return params_.end(); }

  /// @}

 private:
  int stack_size_;
  std::string name_;
  std::vector<std::unique_ptr<Node>> nodes_;
  std::vector<Var*> params_;  ///< The locals include parameter.
  std::deque<std::unique_ptr<Var>> locals_;
};

class Program {
 public:
  Program() = default;
  explicit Program(std::vector<std::unique_ptr<Function>> other)
      : funcs_(std::move(other)) {
    update_offset();
  }

  void accept(AstVisitor& visitor, AstContext& context) {
    for (auto& func : funcs_) {
      visitor.visit_function(func.get(), context);
    }
  }

 private:
  void update_offset() {
    for (auto& func : funcs_) {
      func->update_offset();
    }
  }

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