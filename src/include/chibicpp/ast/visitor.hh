#pragma once

namespace chibicpp {

// class Ast;
class AstContext;
class Function;
class Node;
class Program;

class AstVisitor {
 public:
  // virtual void consume(Ast& ast, AstContext& ctx) = 0;
  virtual void visit_program(Program* prog, AstContext& context) = 0;
  virtual void visit_function(Function* func, AstContext& context) = 0;
  virtual void visit_node(Node* node, AstContext& context) = 0;
};

}  // namespace chibicpp