#pragma once

#include "chibicpp/ast/visitor.hh"

namespace chibicpp {

class Backend : public AstVisitor {
 public:
  static constexpr const char* kArgReg[]{"rdi", "rsi", "rdx",
                                         "rcx", "r8",  "r9"};

  virtual void visit_function(Function* func) override;
  virtual void visit_node(Node* node) override;

 private:
  void gen_addr(Node* node);
  void load();
  void store();

  int label_seq_{};
};

}  // namespace chibicpp