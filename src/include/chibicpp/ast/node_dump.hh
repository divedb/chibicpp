#pragma once

#include "chibicpp/ast/node.hh"

namespace chibicpp {

inline std::string operator_to_string(Node::ID id) {
  switch (id) {
    case Node::kAdd:
    case Node::kPtrAdd:
      return "+";
    case Node::kSub:
    case Node::kPtrSub:
    case Node::kPtrDiff:
      return "-";
    case Node::kMul:
      return "*";
    case Node::kDiv:
      return "/";
    case Node::kEq:
      return "==";
    case Node::kNe:
      return "!=";
    case Node::kLt:
      return "<";
    case Node::kLe:
      return "<=";
    case Node::kAssign:
      return "=";
    case Node::kAddr:
      return "&";
    case Node::kDeref:
      return "*";
    default:
      return "";
  }
}

inline bool is_binary_operator(Node::ID id) {
  return id == Node::kAdd || id == Node::kPtrAdd || id == Node::kSub ||
         id == Node::kPtrSub || id == Node::kPtrDiff || id == Node::kMul ||
         id == Node::kDiv || id == Node::kEq || id == Node::kNe ||
         id == Node::kLt || id == Node::kLe || id == Node::kAssign;
}

inline void dump(std::ostream& os, ObserverPtr<Node> node) {
  if (!node) {
    return;
  }

  auto id = node->id();

  if (id == Node::kNum) {
    os << node->number();
  } else if (is_binary_operator(id)) {
    dump(os, node->lhs());
    os << operator_to_string(id);
    dump(os, node->rhs());
  } else if (id == Node::kAddr) {
    os << "&";
    dump(os, node->lhs());
  } else if (id == Node::kDeref) {
    os << "*";
    dump(os, node->lhs());
  } else if (id == Node::kReturn) {
    os << "return";
    dump(os, node->lhs());
  } else if (id == Node::kIf) {
    os << "if (";
    dump(os, node->cond());
    os << ")\n{";
    dump(os, node->then());
    os << "\n}";

    if (node->els()) {
      os << "else {";
      dump(os, node->els());
      os << "}";
    }
  } else if (id == Node::kExprStmt) {
    dump(os, node->lhs());
  } else if (id == Node::kVar) {
    os << node->var()->name();
  } else {
    assert(false);
  }
}

}  // namespace chibicpp