#include <deque>
#include <memory>
#include <vector>

#include "chibicpp/ast/node.hh"

namespace chibicpp {

class Function;
class Lexer;
class Node;
class Var;

class Parser {
 public:
  explicit Parser(Lexer& lexer) : lexer_(lexer) {}

  std::unique_ptr<Program> program();

 private:
  /// \brief Parse a function prototype.
  ///
  /// For example, the prototype could be `int foo(int a, int b)`.
  /// @code
  /// int foo(int a, int b);
  /// @endcode
  ///
  /// \return
  Function::Prototype parse_function_prototype();

  /// \brief Parse function body.
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
  std::unique_ptr<Node> read_expr_stmt();
  std::unique_ptr<Node> parse_declaration();

  Type* parse_basetype();
  Var* parse_func_param();

  /// \brief Parse function parameters.
  ///
  /// The reason to return `vector<Var*>` instead of `vector<unique_ptr<Var>>`
  /// is the `locals` field's type is `vector<unique_ptr<Var>>`. And the
  /// function parameter is also considered as part of locals.
  ///
  /// \return
  std::vector<Var*> parse_func_params();

  /// \brief primary ::= "(" expr ")" | ident | num
  /// \return
  std::unique_ptr<Node> parse_primary();

  /// \brief Try to search for the specified variable during parsing.
  ///
  /// So far, we have to update the var offset inside main function. Hence the
  /// node must share same `var` with function locals.
  ///
  /// \param token A token of type `kVar`.
  /// \return A pointer to `Var` if it exists, otherwise NULL.
  Var* get_or_create_var(Token const& token, Type* type = nullptr);

  std::vector<std::unique_ptr<Node>> parse_func_args();

  Lexer& lexer_;
  std::deque<std::unique_ptr<Var>> locals_;
};

}  // namespace chibicpp