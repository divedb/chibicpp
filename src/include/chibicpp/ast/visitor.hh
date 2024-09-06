#pragma once

namespace chibicpp {

// class Ast;
// class AstContext;

class Function;
class Node;

class AstVisitor {
 public:
  // virtual void consume(Ast& ast, AstContext& ctx) = 0;
  virtual void visit_function(Function* func) = 0;
  virtual void visit_node(Node* node) = 0;
};

}  // namespace chibicpp