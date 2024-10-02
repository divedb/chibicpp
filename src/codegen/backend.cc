#include "chibicpp/codegen/backend.hh"

#include <cassert>
#include <iostream>
#include <iterator>

#include "chibicpp/ast/context.hh"
#include "chibicpp/ast/node.hh"
#include "chibicpp/common/error.hh"
#include "chibicpp/common/macro.hh"

namespace chibicpp {

void Backend::visit_program(ObserverPtr<Program> prog, AstContext& context) {
  stream_ << ".intel_syntax noprefix\n";

  // Generate assembly code for each global variable.
  for (auto iter = prog->var_begin(); iter != prog->var_end(); ++iter) {
    auto var = iter->get();
    auto type = var->type();
    const auto& name = var->name();

    stream_ << ".globl " << name << '\n';
    stream_ << ".bss\n";
    stream_ << ".align " << type->size_in_bytes() << '\n';
    stream_ << ".type " << name << ", @object\n";
    stream_ << ".size " << name << ", " << type->size_in_bytes() << '\n';
    stream_ << name << ":\n";
    stream_ << "  .zero " << type->size_in_bytes() << '\n';
  }

  // Generate assembly code for each function.
  for (auto iter = prog->func_begin(); iter != prog->func_end(); ++iter) {
    visit_function(iter->get(), context);
  }

  // Generate assembly code for each static variable inside each function.
  for (auto f_iter = prog->func_begin(); f_iter != prog->func_end(); ++f_iter) {
    auto func = f_iter->get();

    for (auto s_iter = func->static_begin(); s_iter != func->static_end();
         ++s_iter) {
      auto var = s_iter->get();
      const auto& name = var->name();
      auto type = var->type();

      stream_ << "  .data\n";
      stream_ << "  .align " << type->size_in_bytes() << '\n';
      stream_ << "  .type " << name << ", @object\n";
      stream_ << "  .size " << name << ", " << type->size_in_bytes() << '\n';
      stream_ << name << ":\n";
      stream_ << "  .zero " << type->size_in_bytes() << '\n';
      // .local
      // .comm
    }
  }
}

void Backend::visit_function(ObserverPtr<Function> func, AstContext& context) {
  // We need to update the AST context to reflect that we are currently
  // visiting this function.
  context.func = func;
  auto fname = func->name();

  // Generate assembly code for string literals in read only section.
  stream_ << ".section .rodata\n";

  for (auto iter = func->string_literal_begin();
       iter != func->string_literal_end(); ++iter) {
    auto var = iter->get();

    stream_ << var->name() << ":\n";

    for (auto c : var->string_literal()) {
      stream_ << "  .byte " << static_cast<int>(c) << '\n';
    }

    stream_ << "  .byte 0\n";

    // stream_ << "  .string \"" << var->string_literal() << "\"\n";
  }

  // Generate assembly code for function.
  stream_ << ".text\n";
  stream_ << ".global " << fname << '\n';
  stream_ << ".type " << fname << ", @function\n";
  stream_ << fname << ":\n";

  // Prologue.
  stream_ << "  push rbp\n";
  stream_ << "  mov rbp, rsp\n";
  stream_ << "  sub rsp, " << func->stack_size() << '\n';

  // Generate assembly code for each function parameter.
  // Push function arguments to the stack.
  // For example: we need to move rdi register to rbp-8 and move sil register
  // to rbp-12.
  //
  // \code
  // void foo(int a, char c);
  // \endcode
  int idx = 0;

  for (auto iter = func->param_begin(); iter != func->param_end(); ++iter) {
    auto var = iter->get();
    int sz = var->type()->size_in_bytes();

    stream_ << "  mov [rbp-" << var->offset() << "], ";

    if (sz == 1) {
      stream_ << kArgReg1[idx];
    } else if (sz == 2) {
      stream_ << kArgReg2[idx];
    } else if (sz == 4) {
      stream_ << kArgReg4[idx];
    } else {
      assert(sz == 8);

      stream_ << kArgReg8[idx];
    }

    stream_ << '\n';

    idx++;
  }

  // Generate assembly code for each node inside function body.
  for (auto iter = func->body_begin(); iter != func->body_end(); ++iter) {
    visit_node(iter->get(), context);
  }

  // Epilogue.
  stream_ << ".L.return." << fname << ":\n";
  stream_ << "  mov rsp, rbp\n";
  stream_ << "  pop rbp\n";
  stream_ << "  ret\n";
}

void Backend::visit_node(ObserverPtr<Node> node, AstContext& context) {
  auto id = node->id();

  switch (id) {
    case Node::kEmpty:
      return;

    case Node::kNum:
      stream_ << "  push " << node->number() << '\n';
      return;

    case Node::kExprStmt:
      // Each statement would push the result to stack.
      // We need to compensate here.
      visit_node(node->lhs(), context);
      stream_ << "  add rsp, 8\n";
      return;

    case Node::kVar:
    case Node::kMember:
      gen_addr(node, context);

      if (!node->type()->is_array()) {
        load(node->type());
      }

      return;

    case Node::kAssign:
      gen_lval(node->lhs(), context);
      visit_node(node->rhs(), context);
      store(node->type());
      return;

    case Node::kAddr:
      gen_addr(node->lhs(), context);
      return;

    case Node::kDeref:
      visit_node(node->lhs(), context);

      if (!node->type()->is_array()) {
        load(node->type());
      }

      return;

    case Node::kIf: {
      int seq = label_seq_++;

      visit_node(node->cond(), context);

      if (node->els()) {
        stream_ << "  pop rax\n";
        stream_ << "  cmp rax, 0\n";
        stream_ << "  je  .L.else." << seq << '\n';
        visit_node(node->then(), context);
        stream_ << "  jmp .L.end." << seq << '\n';
        stream_ << ".L.else." << seq << ":\n";
        visit_node(node->els(), context);
        stream_ << ".L.end." << seq << ":\n";
      } else {
        stream_ << "  pop rax\n";
        stream_ << "  cmp rax, 0\n";
        stream_ << "  je  .L.end." << seq << '\n';
        visit_node(node->then(), context);
        stream_ << ".L.end." << seq << ":\n";
      }

      return;
    }

    case Node::kWhile: {
      int seq = label_seq_++;
      stream_ << ".L.begin." << seq << ":\n";
      visit_node(node->cond(), context);
      stream_ << "  pop rax\n";
      stream_ << "  cmp rax, 0\n";
      stream_ << "  je  .L.end." << seq << '\n';
      visit_node(node->then(), context);
      stream_ << "  jmp .L.begin." << seq << '\n';
      stream_ << ".L.end." << seq << ":\n";

      return;
    }

    case Node::kFor: {
      int seq = label_seq_++;

      if (node->init()) {
        visit_node(node->init(), context);
      }

      stream_ << ".L.begin." << seq << ":\n";

      if (node->cond()) {
        visit_node(node->cond(), context);
        stream_ << "  pop rax\n";
        stream_ << "  cmp rax, 0\n";
        stream_ << "  je  .L.end." << seq << '\n';
      }

      visit_node(node->then(), context);

      if (node->inc()) {
        visit_node(node->inc(), context);
      }

      stream_ << "  jmp .L.begin." << seq << '\n';
      stream_ << ".L.end." << seq << ":\n";

      return;
    }

    case Node::kBlock:
    case Node::kStmtExpr:
      for (auto iter = node->block_begin(); iter != node->block_end(); ++iter) {
        visit_node(iter->get(), context);
      }

      return;

    case Node::kFunCall: {
      int nargs = node->args_size();
      // Push the expression value to stack.
      for (auto iter = node->args_begin(); iter != node->args_end(); ++iter) {
        visit_node(iter->get(), context);
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
      stream_ << "  call " << node->func_name() << '\n';
      stream_ << "  jmp .L.end." << seq << '\n';
      stream_ << ".L.call." << seq << ":\n";
      // Not make sense when char is included
      // stream_ << "  sub rsp, 8\n";
      // stream_ << "  mov rax, 0\n";
      stream_ << "  push rbp\n";
      stream_ << "  mov  rbp, rsp\n";
      stream_ << "  and rsp, 0xFFFFFFFFFFFFFFF0\n";

      stream_ << "  call " << node->func_name() << '\n';

      stream_ << "  mov  rsp, rbp\n";
      stream_ << "  pop rbp\n";

      // stream_ << "  add rsp, 8\n";
      stream_ << ".L.end." << seq << ":\n";
      stream_ << "  push rax\n";
      return;
    }

    case Node::kReturn:
      visit_node(node->lhs(), context);
      stream_ << "  pop rax\n";
      stream_ << "  jmp .L.return." << context.func->name() << '\n';
      return;

    default:
      break;
  }

  visit_node(node->lhs(), context);
  visit_node(node->rhs(), context);

  stream_ << "  pop rdi\n";
  stream_ << "  pop rax\n";

  switch (node->id()) {
    case Node::kAdd:
      stream_ << "  add rax, rdi\n";
      break;

    case Node::kPtrAdd:
      stream_ << "  imul rdi, " << node->type()->base()->size_in_bytes()
              << '\n';
      stream_ << "  add rax, rdi\n";
      break;

    case Node::kSub:
      stream_ << "  sub rax, rdi\n";
      break;

    case Node::kPtrSub:
      stream_ << "  imul rdi, " << node->type()->base()->size_in_bytes()
              << '\n';
      stream_ << "  sub rax, rdi\n";
      break;

    case Node::kPtrDiff:
      stream_ << "  sub rax, rdi\n";
      stream_ << "  cqo\n";
      stream_ << "  mov rdi, " << node->lhs()->type()->base()->size_in_bytes()
              << '\n';
      stream_ << "  idiv rdi\n";
      break;

    case Node::kMul:
      stream_ << "  imul rax, rdi\n";
      break;
    case Node::kDiv:
      stream_ << "  cqo\n";
      stream_ << "  idiv rdi\n";
      break;

    case Node::kEq:
      stream_ << "  cmp rax, rdi\n";
      stream_ << "  sete al\n";
      stream_ << "  movzb rax, al\n";
      break;

    case Node::kNe:
      stream_ << "  cmp rax, rdi\n";
      stream_ << "  setne al\n";
      stream_ << "  movzb rax, al\n";
      break;

    case Node::kLt:
      stream_ << "  cmp rax, rdi\n";
      stream_ << "  setl al\n";
      stream_ << "  movzb rax, al\n";
      break;

    case Node::kLe:
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
  if (node->type()->is_array()) {
    CHIBICPP_THROW_ERROR("Not a lvalue.");
  }

  gen_addr(node, context);
}

/// \brief Pushes the given node's address to the stack.
///
/// \param node
void Backend::gen_addr(ObserverPtr<Node> node, AstContext& context) {
  switch (node->id()) {
    case Node::kVar: {
      auto var = node->var();

      if (var->is_local()) {
        stream_ << "  lea rax, [rbp-" << var->offset() << "]\n";
      } else {
        stream_ << "  lea rax, " << var->name() << "[rip]\n";
      }
      stream_ << "  push rax\n";

      return;
    }

    case Node::kDeref:
      visit_node(node->lhs(), context);
      return;

    case Node::kMember:
      gen_addr(node->lhs(), context);
      stream_ << "  pop rax\n";
      stream_ << "  add rax, " << node->member()->offset() << '\n';
      stream_ << "  push rax\n";
      return;

    default:
      // TODO(gc):
      break;
  }

  CHIBICPP_THROW_ERROR("Not an lvalue");
}

void Backend::load(ObserverPtr<Type> type) {
  auto sz = type->size_in_bytes();

  stream_ << "  pop rax\n";

  if (sz == 1) {
    stream_ << "  movsx rax, byte ptr [rax]\n";
  } else if (sz == 2) {
    stream_ << "  movsx rax, word ptr [rax]\n";
  } else if (sz == 4) {
    stream_ << "  movsxd rax, dword ptr [rax]\n";
  } else {
    assert(sz == 8);

    stream_ << "  mov rax, [rax]\n";
  }

  stream_ << "  push rax\n";
}

void Backend::store(ObserverPtr<Type> type) {
  auto sz = type->size_in_bytes();

  stream_ << "  pop rdi\n";
  stream_ << "  pop rax\n";

  if (sz == 1) {
    stream_ << "  mov [rax], dil\n";
  } else if (sz == 2) {
    stream_ << "  mov [rax], di\n";
  } else if (sz == 4) {
    stream_ << "  mov [rax], edi\n";
  } else {
    assert(sz == 8);

    stream_ << "  mov [rax], rdi\n";
  }

  stream_ << "  push rdi\n";
}

}  // namespace chibicpp