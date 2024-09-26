#include "chibicpp/ast/type.hh"

#include <cassert>

#include "chibicpp/ast/node.hh"
#include "chibicpp/common/error.hh"

namespace chibicpp {

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
    case NodeKind::kFunCall:
    case NodeKind::kNum:
      node->type = TypeMgr::get_integer(Type::kInt);

      return;

    case NodeKind::kPtrAdd:
    case NodeKind::kPtrSub:
    case NodeKind::kAssign:
      node->type = node->lhs->type;

      return;

    case NodeKind::kVar:
      node->type = node->var->type();

      return;

    case NodeKind::kMember:
      node->type = node->member->type();

    case NodeKind::kAddr:
      // TODO(gc): `int a[8]; int* b = &a`.
      // This code will not work in C++ but work in C.
      if (node->lhs->type->is_array()) {
        node->type = TypeMgr::get_pointer(node->lhs->type->base());
      } else {
        node->type = TypeMgr::get_pointer(node->lhs->type);
      }

      return;

    case NodeKind::kDeref:
      if (!node->lhs->type->base()) {
        CHIBICPP_THROW_ERROR("Deference on non-pointer type");
      }

      node->type = node->lhs->type->base();

      return;

    case NodeKind::kStmtExpr:
      node->type = node->body.back()->type;
      break;

    default:
      break;
  }
}

std::string Type::to_string() const {
  std::string str;

  static std::map<TypeID, std::string> primitive_type_descriptor{
      {kSigned, "signed"}, {kUnsigned, "unsigned"},  {kShort, "short"},
      {kLong, "long"},     {kLongLong, "long long"}, {kChar, "char"},
      {kInt, "int"},       {kFloat, "float"},        {kDouble, "double"}};

  for (auto key : {kSigned, kUnsigned, kShort, kLong, kLongLong, kChar, kInt,
                   kFloat, kDouble}) {
    if (id() & key) {
      str += primitive_type_descriptor[key];
    }
  }

  if (is_pointer()) {
    str = base_->to_string() + '*';
  }

  return str;
}

}  // namespace chibicpp