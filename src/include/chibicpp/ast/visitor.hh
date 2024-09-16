#pragma once

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

  /// \brief Visit global variable defined in file scope.
  ///
  /// \param var Global variable to be visited.
  /// \param context AST context providing necessary information.
  virtual void visit_global(Var* var, AstContext& context) = 0;

  /// \brief Visit the entire program.
  ///
  /// \param prog Program structure to be visited.
  /// \param context AST context providing necessary information.
  virtual void visit_program(Program* prog, AstContext& context) = 0;

  virtual void visit_function_params(Var* var, int idx,
                                     AstContext& context) = 0;
  virtual void visit_function_body(Node* node, AstContext& context) = 0;

  /// \brief Visit a function definition.
  ///
  /// \param func Function to be visited.
  /// \param context AST context providing necessary information.
  virtual void visit_function(Function* func, AstContext& context) = 0;

  /// \brief Visit a general node in the AST.
  ///
  /// \param node Node to be visited.
  /// \param context AST context providing necessary information.
  virtual void visit_node(Node* node, AstContext& context) = 0;
};

}  // namespace chibicpp