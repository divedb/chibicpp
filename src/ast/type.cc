#include "chibicpp/ast/type.hh"

#include <cassert>

#include "chibicpp/ast/node.hh"

namespace chibicpp {

/// \brief Get integer type.
///
/// \return
// static Type* int_type() {
//   static Type type;
//   type.kind = TypeKind::kInt;
//   type.base = nullptr;

//   return &type;
// }

bool is_integer(Type* type) { return type->kind == TypeKind::kInt; }

void add_type(Node* node) {
  if (!node || node->type) {
    return;
  }

  add_type(node->lhs.get());
  add_type(node->rhs.get());
  add_type(node->cond.get());
  add_type(node->then.get());
  add_type(node->els.get());
  add_type(node->init.get());
  add_type(node->inc.get());

  // Block statement.
  for (auto iter = node->block_begin(); iter != node->block_end(); iter++) {
    add_type(iter->get());
  }

  for (auto iter = node->args_begin(); iter != node->args_end(); iter++) {
    add_type(iter->get());
  }

  switch (node->kind) {
    case NodeKind::kAdd:
    case NodeKind::kSub:
    case NodeKind::kPtrDiff:
    case NodeKind::kMul:
    case NodeKind::kDiv:
    case NodeKind::kEq:
    case NodeKind::kNe:
    case NodeKind::kLt:
    case NodeKind::kLe:
    case NodeKind::kVar:
    case NodeKind::kFunCall:
    case NodeKind::kNum:
      node->type = std::make_unique<Type>(TypeKind::kInt, nullptr);
      return;

    case NodeKind::kPtrAdd:
    case NodeKind::kPtrSub:
    case NodeKind::kAssign:
      node->type =
          std::make_unique<Type>(node->lhs->type->kind, node->lhs->type->base);
      return;
    case NodeKind::kAddr:
      node->type =
          std::make_unique<Type>(TypeKind::kPointer, node->lhs->type.get());
      return;
    case NodeKind::kDeref:
      if (node->lhs->type->kind == TypeKind::kPointer) {
        assert(node->lhs->type->base != nullptr);

        node->type = std::make_unique<Type>(node->lhs->type->base->kind,
                                            node->lhs->type->base->base);
      } else {
        node->type = std::make_unique<Type>(TypeKind::kInt, nullptr);
      }

      return;
  }
}

}  // namespace chibicpp