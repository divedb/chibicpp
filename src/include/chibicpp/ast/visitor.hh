#pragma once

#include "chibicpp/support/observer_ptr.hh"

namespace chibicpp {

// class Ast;
class AstContext;
class Function;
class Node;
class Program;
class Var;

class AstVisitor {
 public:
  // virtual void consume(Ast& ast, AstContext& ctx) = 0;
  virtual ~AstVisitor() = default;

  /// \brief Visit the entire program.
  ///
  /// \param prog Program structure to be visited.
  /// \param context AST context providing necessary information.
  virtual void visit_program(ObserverPtr<Program> prog,
                             AstContext& context) = 0;

  /// \brief Visit a function definition.
  ///
  /// \param func Function to be visited.
  /// \param context AST context providing necessary information.
  virtual void visit_function(ObserverPtr<Function> func,
                              AstContext& context) = 0;

  /// \brief Visit a general node in the AST.
  ///
  /// \param node Node to be visited.
  /// \param context AST context providing necessary information.
  virtual void visit_node(ObserverPtr<Node> node, AstContext& context) = 0;
};

}  // namespace chibicpp