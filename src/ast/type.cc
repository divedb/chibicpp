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
      node->type = TypeMgr::get_primitive(kInt);

      return;

    case NodeKind::kPtrAdd:
    case NodeKind::kPtrSub:
    case NodeKind::kAssign:
      node->type = node->lhs->type;

      return;

    case NodeKind::kVar:
      node->type = node->var->type;

      return;

    case NodeKind::kAddr:
      node->type = TypeMgr::get_pointer(node->lhs->type);

      return;

    case NodeKind::kDeref:
      if (!node->lhs->type->is_pointer()) {
        CHIBICPP_THROW_ERROR("Deference on non-pointer type");
      }

      node->type = node->lhs->type->base();

      return;

    default:
      // std::cout << "Unhandled node kind: " << static_cast<int>(node->kind)
      //           << std::endl;

      break;
  }
}

std::string Type::to_string() const {
  std::string str;

  static std::map<int, std::string> primitive_type_descriptor{
      {kSigned, "signed"}, {kUnsigned, "unsigned"},  {kShort, "short"},
      {kLong, "long"},     {kLongLong, "long long"}, {kChar, "char"},
      {kInt, "int"},       {kFloat, "float"},        {kDouble, "double"}};

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wdeprecated-enum-enum-conversion"

#define INT_INITIALIZER(...) \
  std::initializer_list<int> { __VA_ARGS__ }

  for (auto key : INT_INITIALIZER(kSigned, kUnsigned, kShort, kLong, kLongLong,
                                  kChar, kInt, kFloat, kDouble)) {
    if (kind_ & key) {
      str += primitive_type_descriptor[key];
    }
  }

#undef INT_INITIALIZER

#pragma GCC diagnostic pop

  if (is_pointer()) {
    str = base_->to_string() + '*';
  }

  return str;
}

}  // namespace chibicpp