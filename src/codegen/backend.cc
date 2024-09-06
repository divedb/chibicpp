#include "chibicpp/codegen/backend.hh"

#include <cassert>
#include <iostream>

#include "chibicpp/ast/node.hh"
#include "chibicpp/common/error.hh"

namespace chibicpp {

void Backend::visit_function(Function* func) {
  std::cout << ".intel_syntax noprefix\n";
  std::cout << ".global main\n";
  std::cout << "main:\n";

  /// Prologue.
  std::cout << "  push rbp\n";
  std::cout << "  mov rbp, rsp\n";
  std::cout << "  sub rsp, " << func->stack_size() << '\n';

  /// Emit code.
  func->accept(*this);

  /// Epilogue.
  std::cout << ".L.return:\n";
  std::cout << "  mov rsp, rbp\n";
  std::cout << "  pop rbp\n";
  std::cout << "  ret\n";
}

void Backend::visit_node(Node* node) {
  auto kind = node->kind;

  switch (kind) {
    case NodeKind::kNum:
      std::cout << "  push " << node->val << '\n';
      return;

    case NodeKind::kExprStmt:
      visit_node(node->lhs.get());
      /// TODO(gc): why add 8?
      std::cout << "  add rsp, 8\n";
      return;

    case NodeKind::kVar:
      gen_addr(node);
      load();

      return;

    case NodeKind::kAssign:
      gen_addr(node->lhs.get());
      visit_node(node->rhs.get());
      store();
      return;

    case NodeKind::kIf: {
      int seq = label_seq_++;

      visit_node(node->cond.get());

      if (node->els) {
        std::cout << "  pop rax\n";
        std::cout << "  cmp rax, 0\n";
        std::cout << "  je  .L.else." << seq << '\n';
        visit_node(node->then.get());
        std::cout << "  jmp .L.end." << seq << '\n';
        std::cout << ".L.else." << seq << ":\n";
        visit_node(node->els.get());
        std::cout << ".L.end." << seq << ":\n";
      } else {
        std::cout << "  pop rax\n";
        std::cout << "  cmp rax, 0\n";
        std::cout << "  je  .L.end." << seq << '\n';
        visit_node(node->then.get());
        std::cout << ".L.end." << seq << ":\n";
      }

      return;
    }

    case NodeKind::kWhile: {
      int seq = label_seq_++;
      std::cout << ".L.begin." << seq << ":\n";
      visit_node(node->cond.get());
      std::cout << "  pop rax\n";
      std::cout << "  cmp rax, 0\n";
      std::cout << "  je  .L.end." << seq << '\n';
      visit_node(node->then.get());
      std::cout << "  jmp .L.begin." << seq << '\n';
      std::cout << ".L.end." << seq << ":\n";

      return;
    }

    case NodeKind::kFor: {
      int seq = label_seq_++;

      if (node->init) {
        visit_node(node->init.get());
      }

      std::cout << ".L.begin." << seq << ":\n";

      if (node->cond) {
        visit_node(node->cond.get());
        std::cout << "  pop rax\n";
        std::cout << "  cmp rax, 0\n";
        std::cout << "  je  .L.end." << seq << '\n';
      }

      visit_node(node->then.get());

      if (node->inc) {
        visit_node(node->inc.get());
      }

      std::cout << "  jmp .L.begin." << seq << '\n';
      std::cout << ".L.end." << seq << ":\n";

      return;
    }

    case NodeKind::kBlock:
      for (auto& e : node->body) {
        visit_node(e.get());
      }

      return;

    case NodeKind::kReturn:
      visit_node(node->lhs.get());
      std::cout << "  pop rax\n";
      std::cout << "  jmp .L.return\n";
      return;

    default:
      break;
  }

  visit_node(node->lhs.get());
  visit_node(node->rhs.get());

  std::cout << "  pop rdi\n";
  std::cout << "  pop rax\n";

  switch (node->kind) {
    case NodeKind::kAdd:
      std::cout << "  add rax, rdi\n";
      break;

    case NodeKind::kSub:
      std::cout << "  sub rax, rdi\n";
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
void Backend::gen_addr(Node* node) {
  if (node->kind == NodeKind::kVar) {
    std::cout << "  lea rax, [rbp-" << node->var->offset << "]\n";
    std::cout << "  push rax\n";
    return;
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