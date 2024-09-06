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

  std::unique_ptr<Function> program();

 private:
  /// stmt = "return" expr ";"
  ///      | "if" "(" expr ")" stmt ( "else" stmt )?
  ///      | expr ";"
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
  Var* find_var(Token const& token) const;

  std::vector<std::unique_ptr<Node>> parse_func_args();

  Lexer& lexer_;
  std::vector<std::unique_ptr<Var>> locals_;
};

}  // namespace chibicpp