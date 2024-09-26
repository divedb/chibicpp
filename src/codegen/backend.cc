#include "chibicpp/codegen/backend.hh"

#include <cassert>
#include <iostream>
#include <iterator>

#include "chibicpp/ast/node.hh"
#include "chibicpp/common/error.hh"

namespace chibicpp {

void Backend::visit_global(ObserverPtr<Var> var, AstContext& context) {
  // It's fine to define a `.data` section for each global variable.
  // The assembler will merge them into a single section.
  stream_ << ".data\n";
  stream_ << var->name() << ":\n";

  if (var->is_string_literal()) {
    for (auto c : var->string_literal()) {
      stream_ << "  .byte " << static_cast<int>(c) << '\n';
    }

    stream_ << "  .byte 0\n";
  } else {
    stream_ << "  .zero " << var->type()->size_in_bytes() << '\n';
  }
}

void Backend::visit_program(ObserverPtr<Program> prog, AstContext& context) {
  stream_ << ".intel_syntax noprefix\n";
  prog->accept(*this, context);
}

/// \brief Push function arguments to the stack.
///
/// For example: we need to move rdi register to rbp-8 and move sil register
/// to rbp-12.
///
/// \code
/// void foo(int a, char c);
/// \endcode
///
/// \param var A pointer to function argument.
/// \param idx Index of argument.
void Backend::visit_function_params(ObserverPtr<Var> var, int idx,
                                    AstContext& context) {
  int sz = var->type()->size_in_bytes();

  if (sz == 1) {
    stream_ << "  mov [rbp-" << var->offset() << "], " << kArgReg1[idx] << '\n';
  } else {
    assert(sz == 8);

    stream_ << "  mov [rbp-" << var->offset() << "], " << kArgReg8[idx] << '\n';
  }
}

void Backend::visit_function_body(ObserverPtr<Node> node, AstContext& context) {
  visit_node(node, context);
}

void Backend::visit_function(ObserverPtr<Function> func, AstContext& context) {
  // We need to update the AST context to reflect that we are currently
  // visiting this function.
  context.func = func;

  auto fname = func->name();
  stream_ << ".text\n";
  stream_ << ".global " << fname << '\n';
  stream_ << fname << ":\n";

  // Prologue.
  stream_ << "  push rbp\n";
  stream_ << "  mov rbp, rsp\n";
  stream_ << "  sub rsp, " << func->stack_size() << '\n';

  func->accept(*this, context);

  // Epilogue.
  stream_ << ".L.return." << fname << ":\n";
  stream_ << "  mov rsp, rbp\n";
  stream_ << "  pop rbp\n";
  stream_ << "  ret\n";
}

void Backend::visit_node(ObserverPtr<Node> node, AstContext& context) {
  auto kind = node->kind;

  switch (kind) {
    case NodeKind::kEmpty:
      return;

    case NodeKind::kNum:
      stream_ << "  push " << node->val << '\n';
      return;

    case NodeKind::kExprStmt:
      // Each statement would push the result to stack.
      // We need to compensate here.
      visit_node(node->lhs.get(), context);
      stream_ << "  add rsp, 8\n";
      return;

    case NodeKind::kVar:
    case NodeKind::kMember:
      gen_addr(node, context);

      if (!node->type->is_array()) {
        load(node->type);
      }

      return;

    case NodeKind::kAssign:
      gen_lval(node->lhs.get(), context);
      visit_node(node->rhs.get(), context);
      store(node->type);
      return;

    case NodeKind::kAddr:
      gen_addr(node->lhs.get(), context);
      return;

    case NodeKind::kDeref:
      visit_node(node->lhs.get(), context);

      if (!node->type->is_array()) {
        load(node->type);
      }

      return;

    case NodeKind::kIf: {
      int seq = label_seq_++;

      visit_node(node->cond.get(), context);

      if (node->els) {
        stream_ << "  pop rax\n";
        stream_ << "  cmp rax, 0\n";
        stream_ << "  je  .L.else." << seq << '\n';
        visit_node(node->then.get(), context);
        stream_ << "  jmp .L.end." << seq << '\n';
        stream_ << ".L.else." << seq << ":\n";
        visit_node(node->els.get(), context);
        stream_ << ".L.end." << seq << ":\n";
      } else {
        stream_ << "  pop rax\n";
        stream_ << "  cmp rax, 0\n";
        stream_ << "  je  .L.end." << seq << '\n';
        visit_node(node->then.get(), context);
        stream_ << ".L.end." << seq << ":\n";
      }

      return;
    }

    case NodeKind::kWhile: {
      int seq = label_seq_++;
      stream_ << ".L.begin." << seq << ":\n";
      visit_node(node->cond.get(), context);
      stream_ << "  pop rax\n";
      stream_ << "  cmp rax, 0\n";
      stream_ << "  je  .L.end." << seq << '\n';
      visit_node(node->then.get(), context);
      stream_ << "  jmp .L.begin." << seq << '\n';
      stream_ << ".L.end." << seq << ":\n";

      return;
    }

    case NodeKind::kFor: {
      int seq = label_seq_++;

      if (node->init) {
        visit_node(node->init.get(), context);
      }

      stream_ << ".L.begin." << seq << ":\n";

      if (node->cond) {
        visit_node(node->cond.get(), context);
        stream_ << "  pop rax\n";
        stream_ << "  cmp rax, 0\n";
        stream_ << "  je  .L.end." << seq << '\n';
      }

      visit_node(node->then.get(), context);

      if (node->inc) {
        visit_node(node->inc.get(), context);
      }

      stream_ << "  jmp .L.begin." << seq << '\n';
      stream_ << ".L.end." << seq << ":\n";

      return;
    }

    case NodeKind::kBlock:
    case NodeKind::kStmtExpr:
      for (auto& e : node->body) {
        visit_node(e.get(), context);
      }

      return;

    case NodeKind::kFunCall: {
      int nargs = node->args.size();
      // Push the expression value to stack.
      for (auto& arg : node->args) {
        visit_node(arg.get(), context);
      }

      // Pop the value from stack to registers.
      for (int i = nargs - 1; i >= 0; i--) {
        stream_ << "  pop " << kArgReg8[i] << '\n';
      }

      /// We need to align RSP to a 16 byte boundary before
      /// calling a function because it is an ABI requirement.
      /// RAX is set to 0 for variadic function.
      int seq = label_seq_++;

      stream_ << "  mov rax, rsp\n";
      stream_ << "  and rax, 15\n";
      stream_ << "  jnz .L.call." << seq << '\n';
      stream_ << "  mov rax, 0\n";
      stream_ << "  call " << node->func_name << '\n';
      stream_ << "  jmp .L.end." << seq << '\n';
      stream_ << ".L.call." << seq << ":\n";
      stream_ << "  sub rsp, 8\n";
      stream_ << "  mov rax, 0\n";
      stream_ << "  call " << node->func_name << '\n';
      stream_ << "  add rsp, 8\n";
      stream_ << ".L.end." << seq << ":\n";
      stream_ << "  push rax\n";
      return;
    }

    case NodeKind::kReturn:
      visit_node(node->lhs.get(), context);
      stream_ << "  pop rax\n";
      stream_ << "  jmp .L.return." << context.func->name() << '\n';
      return;

    default:
      break;
  }

  visit_node(node->lhs.get(), context);
  visit_node(node->rhs.get(), context);

  stream_ << "  pop rdi\n";
  stream_ << "  pop rax\n";

  switch (node->kind) {
    case NodeKind::kAdd:
      stream_ << "  add rax, rdi\n";
      break;

    case NodeKind::kPtrAdd:
      stream_ << "  imul rdi, " << node->type->base()->size_in_bytes() << '\n';
      stream_ << "  add rax, rdi\n";
      break;

    case NodeKind::kSub:
      stream_ << "  sub rax, rdi\n";
      break;

    case NodeKind::kPtrSub:
      stream_ << "  imul rdi, " << node->type->base()->size_in_bytes() << '\n';
      stream_ << "  sub rax, rdi\n";
      break;

    case NodeKind::kPtrDiff:
      stream_ << "  sub rax, rdi\n";
      stream_ << "  cqo\n";
      stream_ << "  mov rdi, " << node->lhs->type->base()->size_in_bytes()
              << '\n';
      stream_ << "  idiv rdi\n";
      break;

    case NodeKind::kMul:
      stream_ << "  imul rax, rdi\n";
      break;
    case NodeKind::kDiv:
      stream_ << "  cqo\n";
      stream_ << "  idiv rdi\n";
      break;

    case NodeKind::kEq:
      stream_ << "  cmp rax, rdi\n";
      stream_ << "  sete al\n";
      stream_ << "  movzb rax, al\n";
      break;

    case NodeKind::kNe:
      stream_ << "  cmp rax, rdi\n";
      stream_ << "  setne al\n";
      stream_ << "  movzb rax, al\n";
      break;

    case NodeKind::kLt:
      stream_ << "  cmp rax, rdi\n";
      stream_ << "  setl al\n";
      stream_ << "  movzb rax, al\n";
      break;

    case NodeKind::kLe:
      stream_ << "  cmp rax, rdi\n";
      stream_ << "  setle al\n";
      stream_ << "  movzb rax, al\n";
      break;

    default:
      assert(false);
  }

  stream_ << "  push rax\n";
}

void Backend::gen_lval(ObserverPtr<Node> node, AstContext& context) {
  if (node->type->is_array()) {
    CHIBICPP_THROW_ERROR("Not a lvalue.");
  }

  gen_addr(node, context);
}

/// \brief Pushes the given node's address to the stack.
///
/// \param node
void Backend::gen_addr(ObserverPtr<Node> node, AstContext& context) {
  switch (node->kind) {
    case NodeKind::kVar: {
      auto var = node->var;

      if (var->is_local()) {
        stream_ << "  lea rax, [rbp-" << node->var->offset() << "]\n";
        stream_ << "  push rax\n";
      } else {
        stream_ << "  push offset " << var->name() << '\n';
      }

      return;
    }

    case NodeKind::kDeref:
      visit_node(node->lhs.get(), context);
      return;

    case NodeKind::kMember:
      gen_addr(node->lhs.get(), context);
      stream_ << "  pop rax\n";
      stream_ << "  add rax, " << node->member->offset() << '\n';
      stream_ << "  push rax\n";
      return;

    default:
      // TODO(gc):
      break;
  }

  CHIBICPP_THROW_ERROR("Not an lvalue");
}

void Backend::load(ObserverPtr<Type> type) {
  stream_ << "  pop rax\n";

  if (type->size_in_bytes() == 1) {
    stream_ << "  movsx rax, byte ptr [rax]\n";
  } else {
    stream_ << "  mov rax, [rax]\n";
  }

  stream_ << "  push rax\n";
}

void Backend::store(ObserverPtr<Type> type) {
  stream_ << "  pop rdi\n";
  stream_ << "  pop rax\n";

  if (type->size_in_bytes() == 1) {
    stream_ << "  mov [rax], dil\n";
  } else {
    stream_ << "  mov [rax], rdi\n";
  }

  stream_ << "  push rdi\n";
}

}  // namespace chibicpp