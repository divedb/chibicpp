#include "chibicpp/codegen/backend.hh"

#include <cassert>
#include <iostream>

#include "chibicpp/ast/node.hh"
#include "chibicpp/common/error.hh"

namespace chibicpp {

void Backend::visit_program(Program* prog, AstContext& context) {
  std::cout << ".intel_syntax noprefix\n";
  prog->accept(*this, context);
}

void Backend::visit_function(Function* func, AstContext& context) {
  auto func_name = func->name();
  std::cout << ".global " << func_name << '\n';
  std::cout << func_name << ":\n";

  /// Prologue.
  std::cout << "  push rbp\n";
  std::cout << "  mov rbp, rsp\n";
  std::cout << "  sub rsp, " << func->stack_size() << '\n';

  /// Push arguments to the stack.
  int i = 0;
  for (auto iter = func->param_begin(); iter != func->param_end(); iter++) {
    std::cout << "  mov [rbp-" << (*iter)->offset << "], " << kArgReg[i++]
              << '\n';
  }

  /// Emit code.
  func->accept(*this, context);

  /// Epilogue.
  std::cout << ".L.return." << func_name << ":\n";
  std::cout << "  mov rsp, rbp\n";
  std::cout << "  pop rbp\n";
  std::cout << "  ret\n";
}

void Backend::visit_node(Node* node, AstContext& context) {
  auto kind = node->kind;

  switch (kind) {
    case NodeKind::kEmpty:
      return;

    case NodeKind::kNum:
      std::cout << "  push " << node->val << '\n';
      return;

    case NodeKind::kExprStmt:
      visit_node(node->lhs.get(), context);
      /// TODO(gc): why add 8?
      std::cout << "  add rsp, 8\n";
      return;

    case NodeKind::kVar:
      gen_addr(node, context);
      load();

      return;

    case NodeKind::kAssign:
      gen_addr(node->lhs.get(), context);
      visit_node(node->rhs.get(), context);
      store();
      return;

    case NodeKind::kAddr:
      gen_addr(node->lhs.get(), context);
      return;

    case NodeKind::kDeref:
      visit_node(node->lhs.get(), context);
      load();
      return;

    case NodeKind::kIf: {
      int seq = label_seq_++;

      visit_node(node->cond.get(), context);

      if (node->els) {
        std::cout << "  pop rax\n";
        std::cout << "  cmp rax, 0\n";
        std::cout << "  je  .L.else." << seq << '\n';
        visit_node(node->then.get(), context);
        std::cout << "  jmp .L.end." << seq << '\n';
        std::cout << ".L.else." << seq << ":\n";
        visit_node(node->els.get(), context);
        std::cout << ".L.end." << seq << ":\n";
      } else {
        std::cout << "  pop rax\n";
        std::cout << "  cmp rax, 0\n";
        std::cout << "  je  .L.end." << seq << '\n';
        visit_node(node->then.get(), context);
        std::cout << ".L.end." << seq << ":\n";
      }

      return;
    }

    case NodeKind::kWhile: {
      int seq = label_seq_++;
      std::cout << ".L.begin." << seq << ":\n";
      visit_node(node->cond.get(), context);
      std::cout << "  pop rax\n";
      std::cout << "  cmp rax, 0\n";
      std::cout << "  je  .L.end." << seq << '\n';
      visit_node(node->then.get(), context);
      std::cout << "  jmp .L.begin." << seq << '\n';
      std::cout << ".L.end." << seq << ":\n";

      return;
    }

    case NodeKind::kFor: {
      int seq = label_seq_++;

      if (node->init) {
        visit_node(node->init.get(), context);
      }

      std::cout << ".L.begin." << seq << ":\n";

      if (node->cond) {
        visit_node(node->cond.get(), context);
        std::cout << "  pop rax\n";
        std::cout << "  cmp rax, 0\n";
        std::cout << "  je  .L.end." << seq << '\n';
      }

      visit_node(node->then.get(), context);

      if (node->inc) {
        visit_node(node->inc.get(), context);
      }

      std::cout << "  jmp .L.begin." << seq << '\n';
      std::cout << ".L.end." << seq << ":\n";

      return;
    }

    case NodeKind::kBlock:
      for (auto& e : node->body) {
        visit_node(e.get(), context);
      }

      return;

    case NodeKind::kFunCall: {
      int nargs = node->args.size();

      for (auto& arg : node->args) {
        visit_node(arg.get(), context);
      }

      for (int i = nargs - 1; i >= 0; i--) {
        std::cout << "  pop " << kArgReg[i] << '\n';
      }

      /// We need to align RSP to a 16 byte boundary before
      /// calling a function because it is an ABI requirement.
      /// RAX is set to 0 for variadic function.
      int seq = label_seq_++;

      std::cout << "  mov rax, rsp\n";
      std::cout << "  and rax, 15\n";
      std::cout << "  jnz .L.call." << seq << '\n';
      std::cout << "  mov rax, 0\n";
      std::cout << "  call " << node->func_name << '\n';
      std::cout << "  jmp .L.end." << seq << '\n';
      std::cout << ".L.call." << seq << ":\n";
      std::cout << "  sub rsp, 8\n";
      std::cout << "  mov rax, 0\n";
      std::cout << "  call " << node->func_name << '\n';
      std::cout << "  add rsp, 8\n";
      std::cout << ".L.end." << seq << ":\n";
      std::cout << "  push rax\n";
      return;
    }

    case NodeKind::kReturn:
      visit_node(node->lhs.get(), context);
      std::cout << "  pop rax\n";
      std::cout << "  jmp .L.return." << context.func->name() << '\n';
      return;

    default:
      break;
  }

  visit_node(node->lhs.get(), context);
  visit_node(node->rhs.get(), context);

  std::cout << "  pop rdi\n";
  std::cout << "  pop rax\n";

  switch (node->kind) {
    case NodeKind::kAdd:
      std::cout << "  add rax, rdi\n";
      break;

    case NodeKind::kPtrAdd:
      std::cout << "  imul rdi, 8\n";
      std::cout << "  add rax, rdi\n";
      break;

    case NodeKind::kSub:
      std::cout << "  sub rax, rdi\n";
      break;

    case NodeKind::kPtrSub:
      std::cout << "  imul rdi, 8\n";
      std::cout << "  sub rax, rdi\n";
      break;

    case NodeKind::kPtrDiff:
      std::cout << "  sub rax, rdi\n";
      std::cout << "  cqo\n";
      std::cout << "  mov rdi, 8\n";
      std::cout << "  idiv rdi\n";
      break;

    case NodeKind::kMul:
      std::cout << "  imul rax, rdi\n";
      break;
    case NodeKind::kDiv:
      std::cout << "  cqo\n";
      std::cout << "  idiv rdi\n";
      break;

    case NodeKind::kEq:
      std::cout << "  cmp rax, rdi\n";
      std::cout << "  sete al\n";
      std::cout << "  movzb rax, al\n";
      break;

    case NodeKind::kNe:
      std::cout << "  cmp rax, rdi\n";
      std::cout << "  setne al\n";
      std::cout << "  movzb rax, al\n";
      break;

    case NodeKind::kLt:
      std::cout << "  cmp rax, rdi\n";
      std::cout << "  setl al\n";
      std::cout << "  movzb rax, al\n";
      break;

    case NodeKind::kLe:
      std::cout << "  cmp rax, rdi\n";
      std::cout << "  setle al\n";
      std::cout << "  movzb rax, al\n";
      break;

    default:
      assert(false);
  }

  std::cout << "  push rax\n";
}

/// \brief Pushes the given node's address to the stack.
///
/// \param node
void Backend::gen_addr(Node* node, AstContext& context) {
  switch (node->kind) {
    case NodeKind::kVar:
      std::cout << "  lea rax, [rbp-" << node->var->offset << "]\n";
      std::cout << "  push rax\n";
      return;

    case NodeKind::kDeref:
      visit_node(node->lhs.get(), context);
      return;

    default:
      // TODO(gc):
      break;
  }

  CHIBICPP_THROW_ERROR("Not an lvalue");
}

void Backend::load() {
  std::cout << "  pop rax\n";
  std::cout << "  mov rax, [rax]\n";
  std::cout << "  push rax\n";
}

void Backend::store() {
  std::cout << "  pop rdi\n";
  std::cout << "  pop rax\n";
  std::cout << "  mov [rax], rdi\n";
  std::cout << "  push rdi\n";
}

}  // namespace chibicpp