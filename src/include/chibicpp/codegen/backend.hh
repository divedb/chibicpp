#pragma once

#include <iosfwd>

#include "chibicpp/ast/visitor.hh"

namespace chibicpp {

class Type;

class Backend : public AstVisitor {
 public:
  static constexpr const char* kArgReg1[]{"dil", "sil", "dl",
                                          "cl",  "r8b", "r9b"};
  static constexpr const char* kArgReg8[]{"rdi", "rsi", "rdx",
                                          "rcx", "r8",  "r9"};

  explicit Backend(std::ostream& stream) : stream_{stream} {}

  void visit_global(ObserverPtr<Var> var, AstContext& context) override;
  void visit_program(ObserverPtr<Program> prog, AstContext& context) override;
  void visit_function_params(ObserverPtr<Var> var, int idx,
                             AstContext& context) override;
  void visit_function_body(ObserverPtr<Node> node,
                           AstContext& context) override;
  void visit_function(ObserverPtr<Function> func, AstContext& context) override;
  void visit_node(ObserverPtr<Node> node, AstContext& context) override;

 private:
  void gen_addr(ObserverPtr<Node> node, AstContext& context);
  void gen_lval(ObserverPtr<Node> node, AstContext& context);
  void load(ObserverPtr<Type> type);
  void store(ObserverPtr<Type> type);
  void load_arg(ObserverPtr<Var> var, int idx);

  int label_seq_{};
  std::ostream& stream_;
};

}  // namespace chibicpp