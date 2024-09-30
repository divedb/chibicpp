#include "chibicpp/ast/node.hh"

#include "chibicpp/common/error.hh"

namespace chibicpp {

void Node::update_type() {
  if (type()) {
    return;
  }

  if (lhs_) lhs_->update_type();
  if (rhs_) rhs_->update_type();
  if (cond_) cond_->update_type();
  if (then_) then_->update_type();
  if (else_) else_->update_type();
  if (init_) init_->update_type();
  if (inc_) inc_->update_type();

  for (auto iter = block_begin(); iter != block_end(); ++iter) {
    iter->get()->update_type();
  }

  for (auto iter = args_begin(); iter != args_end(); ++iter) {
    iter->get()->update_type();
  }

  switch (id_) {
    case Node::kAdd:
    case Node::kSub:
    case Node::kPtrDiff:
    case Node::kMul:
    case Node::kDiv:
    case Node::kEq:
    case Node::kNe:
    case Node::kLt:
    case Node::kLe:
    case Node::kFunCall:
    case Node::kNum:
      type_ = TypeFactory::get_signed_int();

      return;

    case Node::kPtrAdd:
    case Node::kPtrSub:
    case Node::kAssign:
      type_ = lhs_->type();

      return;

    case Node::kVar:
      type_ = var_->type();

      return;

    case Node::kMember:
      type_ = member_->type();
      return;

    case Node::kAddr:
      // TODO(gc): `int a[8]; int* b = &a`.
      // This code will not work in C++ but work in C.
      if (lhs_->type()->is_array()) {
        type_ = TypeFactory::get_pointer(lhs_->type()->base());
      } else {
        type_ = TypeFactory::get_pointer(lhs_->type());
      }

      return;

    case Node::kDeref:
      if (!lhs_->type()->base()) {
        CHIBICPP_THROW_ERROR("Deference on non-pointer type");
      }

      type_ = lhs_->type()->base();

      return;

    case Node::kStmtExpr:
      type_ = body_.back()->type();
      break;

    default:
      break;
  }
}

std::unique_ptr<Node> Node::clone() const {
  auto cloned = std::make_unique<Node>(id_);

  cloned->type_ = type_;
  cloned->func_name_ = func_name_;
  cloned->num_ = num_;
  cloned->var_ = var_;

  if (lhs_) cloned->lhs_ = lhs_->clone();
  if (rhs_) cloned->rhs_ = rhs_->clone();
  if (cond_) cloned->cond_ = cond_->clone();
  if (then_) cloned->then_ = then_->clone();
  if (else_) cloned->else_ = else_->clone();
  if (init_) cloned->init_ = init_->clone();
  if (inc_) cloned->inc_ = inc_->clone();

  for (const auto& n : body_) cloned->body_.push_back(n->clone());
  for (const auto& n : args_) cloned->args_.push_back(n->clone());

  return cloned;
}

}  // namespace chibicpp