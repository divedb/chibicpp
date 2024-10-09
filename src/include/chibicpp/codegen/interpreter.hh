#pragma once

#include <cstdint>
#include <map>
#include <memory>

#include "chibicpp/ast/visitor.hh"

namespace chibicpp {

class Type;

class Interpreter : public AstVisitor {
 public:
  static constexpr int kMemSize = 80 * 1024 * 1024;

  void visit_program(ObserverPtr<Program> prog, AstContext& context) override;
  void visit_function(ObserverPtr<Function> func, AstContext& context) override;
  void visit_node(ObserverPtr<Node> node, AstContext& context) override;

 private:
  void gen_addr(ObserverPtr<Node> node, AstContext& context);
  void gen_lval(ObserverPtr<Node> node, AstContext& context);
  void load(ObserverPtr<Type> type);
  void store(ObserverPtr<Type> type);
  void load_arg(ObserverPtr<Var> var, int idx);

  struct Register {
    uint32_t hi;
    uint32_t lo;
  };

  Register rax_{};
  Register rcx_{};
  Register rdx_{};
  Register rbx_{};
  Register rsp_{};
  Register rbp_{};
  Register rsi_{};
  Register rdi_{};
  Register r8_{};
  Register r9_{};
  Register r10_{};
  Register r11_{};
  Register r12_{};
  Register r13_{};
  Register r14_{};
  Register r15_{};

  std::unique_ptr<unsigned char[]> memory_;
};

}  // namespace chibicpp