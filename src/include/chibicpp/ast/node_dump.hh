#pragma once

#include "chibicpp/ast/node.hh"

namespace chibicpp {

inline std::string operator_to_string(NodeKind kind) {
  switch (kind) {
    case NodeKind::kAdd:
    case NodeKind::kPtrAdd:
      return "+";
    case NodeKind::kSub:
    case NodeKind::kPtrSub:
    case NodeKind::kPtrDiff:
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
    case NodeKind::kAddr:
      return "&";
    case NodeKind::kDeref:
      return "*";
    default:
      return "";
  }
}

inline bool is_binary_operator(NodeKind kind) {
  return kind == NodeKind::kAdd || kind == NodeKind::kPtrAdd ||
         kind == NodeKind::kSub || kind == NodeKind::kPtrSub ||
         kind == NodeKind::kPtrDiff || kind == NodeKind::kMul ||
         kind == NodeKind::kDiv || kind == NodeKind::kEq ||
         kind == NodeKind::kNe || kind == NodeKind::kLt ||
         kind == NodeKind::kLe || kind == NodeKind::kAssign;
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
    os << operator_to_string(kind);
    dump(os, node->rhs.get());
  } else if (kind == NodeKind::kAddr) {
    os << "&";
    dump(os, node->lhs.get());
  } else if (kind == NodeKind::kDeref) {
    os << "*";
    dump(os, node->lhs.get());
  } else if (kind == NodeKind::kReturn) {
    os << "return";
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

}  // namespace chibicpp