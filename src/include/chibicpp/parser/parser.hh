#include <deque>
#include <memory>
#include <vector>

#include "chibicpp/ast/node.hh"
#include "chibicpp/parser/scope.hh"
#include "chibicpp/util/observer_ptr.hh"

namespace chibicpp {

class Function;
class Lexer;
class Node;
class Var;

class Parser {
 public:
  explicit Parser(Lexer& lexer) : lexer_(lexer) {}

  std::unique_ptr<Program> parse_program();

 private:
  /// Parse a function prototype.
  ///
  /// For example, the prototype could be `int foo(int a, int b)`.
  ///
  /// \code
  /// int foo(int a, int b);
  /// \endcode
  ///
  /// \return
  std::unique_ptr<Function::Prototype> parse_function_prototype();

  /// Parse function body.
  std::vector<std::unique_ptr<Node>> parse_function_body();

  std::unique_ptr<Function> parse_function();

  /// stmt ::= "return" expr ";"
  ///        | "if" "(" expr ")" stmt ( "else" stmt )?
  ///        | expr ";"
  ///        |
  ///
  /// \return
  std::unique_ptr<Node> parse_stmt();
  std::unique_ptr<Node> parse_expr();
  std::unique_ptr<Node> parse_assign();
  std::unique_ptr<Node> parse_equality();
  std::unique_ptr<Node> parse_relational();
  std::unique_ptr<Node> parse_add();
  std::unique_ptr<Node> parse_mul();
  std::unique_ptr<Node> parse_unary();
  std::unique_ptr<Node> parse_postfix();
  std::unique_ptr<Node> parse_struct_ref(std::unique_ptr<Node> node);
  std::unique_ptr<Node> parse_expr_stmt();
  std::unique_ptr<Node> parse_declaration();

  ObserverPtr<Type> parse_type_suffix(ObserverPtr<Type> base);
  ObserverPtr<Type> parse_basetype();
  ObserverPtr<Var> parse_func_param();

  /// Parse function parameters.
  ///
  /// The reason to return `vector<Var*>` instead of `vector<unique_ptr<Var>>`
  /// is the `locals` field's type is `vector<unique_ptr<Var>>`. And the
  /// function parameter is also considered as part of locals.
  ///
  /// \return
  std::vector<ObserverPtr<Var>> parse_func_params();

  /// primary ::= "(" expr ")" | ident | num
  /// \return
  std::unique_ptr<Node> parse_primary();
  std::vector<std::unique_ptr<Node>> parse_func_args();
  void parse_global_var();
  std::unique_ptr<Node> parse_stmt_expr();

  ObserverPtr<Type> parse_struct_decl();
  std::unique_ptr<Member> parse_struct_member();

  /// \name Utility method
  /// @{

  /// Generates a unique name for an anonymous struct.
  ///
  /// This function provides a mechanism to generate an anonymous name
  /// for structures that do not have an explicit name. For example:
  ///
  /// \code
  /// struct { int a; int b; } c;
  /// \endcode
  ///
  /// \param lineno The line number where the struct is declared.
  /// \return A string representing the generated anonymous struct name.
  static std::string gen_anonymous_struct_name(int lineno) {
    static int unique_id = 0;

    return "__anonymous_struct__" + std::to_string(unique_id++) + ':' +
           std::to_string(lineno);
  }

  /// Determine whether the next top-level item is a function or a global
  ///        variable by looking ahead input tokens.
  ///
  /// \return `true` if it's a function, otherwise `false`.
  bool is_function();

  bool is_typename();

  /// @}

  Lexer& lexer_;
  VarScope scope_;
};

}  // namespace chibicpp