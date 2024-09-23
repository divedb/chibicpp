#include <deque>
#include <memory>
#include <vector>

#include "chibicpp/ast/node.hh"
#include "chibicpp/parser/scope.hh"

namespace chibicpp {

class Function;
class Lexer;
class Node;
class Var;

struct ParserInfo {
  struct CallStack {
    std::string fname;
    int lineno;
    int depth;
  };

  int depth{};
  std::vector<CallStack> call_stacks;
};

class CallStackGuard {
 public:
  explicit CallStackGuard(ParserInfo& pinfo, std::string const& fname,
                          int lineno)
      : pinfo_(pinfo) {
#ifndef NDEBUG
    pinfo_.call_stacks.push_back({fname, lineno, pinfo_.depth});
    ++pinfo_.depth;
#endif
  }

  ~CallStackGuard() {
#ifndef NDEBUG
    --pinfo_.depth;
#endif
  }

 private:
  ParserInfo& pinfo_;
};

class Parser {
 public:
  explicit Parser(Lexer& lexer) : lexer_(lexer) {}

  std::unique_ptr<Program> parse_program();

 private:
  /// \brief Parse a function prototype.
  ///
  /// For example, the prototype could be `int foo(int a, int b)`.
  /// \code
  /// int foo(int a, int b);
  /// \endcode
  ///
  /// \return
  Function::Prototype parse_function_prototype(ParserInfo& pinfo);

  /// \brief Parse function body.
  std::vector<std::unique_ptr<Node>> parse_function_body(ParserInfo& pinfo);

  std::unique_ptr<Function> parse_function(ParserInfo& pinfo);

  /// stmt ::= "return" expr ";"
  ///        | "if" "(" expr ")" stmt ( "else" stmt )?
  ///        | expr ";"
  ///        |
  ///
  /// \return
  std::unique_ptr<Node> parse_stmt(ParserInfo& pinfo);
  std::unique_ptr<Node> parse_expr(ParserInfo& pinfo);
  std::unique_ptr<Node> parse_assign(ParserInfo& pinfo);
  std::unique_ptr<Node> parse_equality(ParserInfo& pinfo);
  std::unique_ptr<Node> parse_relational(ParserInfo& pinfo);
  std::unique_ptr<Node> parse_add(ParserInfo& pinfo);
  std::unique_ptr<Node> parse_mul(ParserInfo& pinfo);
  std::unique_ptr<Node> parse_unary(ParserInfo& pinfo);
  std::unique_ptr<Node> parse_postfix(ParserInfo& pinfo);
  std::unique_ptr<Node> parse_expr_stmt(ParserInfo& pinfo);
  std::unique_ptr<Node> parse_declaration(ParserInfo& pinfo);

  Type* parse_type_suffix(Type* base, ParserInfo& pinfo);
  Type* parse_basetype(ParserInfo& pinfo);
  Var* parse_func_param(ParserInfo& pinfo);

  /// \brief Parse function parameters.
  ///
  /// The reason to return `vector<Var*>` instead of `vector<unique_ptr<Var>>`
  /// is the `locals` field's type is `vector<unique_ptr<Var>>`. And the
  /// function parameter is also considered as part of locals.
  ///
  /// \return
  std::vector<Var*> parse_func_params(ParserInfo& pinfo);

  /// \brief primary ::= "(" expr ")" | ident | num
  /// \return
  std::unique_ptr<Node> parse_primary(ParserInfo& pinfo);
  std::vector<std::unique_ptr<Node>> parse_func_args(ParserInfo& pinfo);
  void parse_global_var(ParserInfo& pinfo);
  std::unique_ptr<Node> parse_stmt_expr(ParserInfo& pinfo);

  /// \name Utility method
  /// @{

  /// \brief Create local variable withe specified `ident` and `type`.
  ///
  /// \param ident The name of the variable.
  /// \param type The type of the variable.
  /// \return A pointer to the created variable.
  Var* create_local_var(std::string const& ident, Type* type);
  Var* create_global_var(std::string const& ident, Type* type);
  Var* create_global_var(std::string const& ident, std::string const& content,
                         Type* type);
  template <typename... Args>
  Var* create_var_impl(std::vector<std::unique_ptr<Var>>& vars,
                       Args&&... args) {
    vars.push_back(std::make_unique<Var>(std::forward<Args>(args)...));
    auto var = vars.back().get();
    scope_->add_var(var);

    return var;
  }

  std::string new_global_label();

  /// \brief Try to search for the specified variable during parsing.
  ///
  /// The variable's lifetime is controlled by `globals_` and `locals_`.
  ///
  /// \param ident The name of the variable.
  /// \return A pointer to `Var` if it exists, otherwise NULL.
  Var* get_var(std::string const& ident);

  /// \brief Determine whether the next top-level item is a function or a global
  ///        variable by looking ahead input tokens.
  ///
  /// \return `true` if it's a function, otherwise `false`.
  bool is_function(ParserInfo& pinfo);

  bool is_typename();

  /// @}

  int global_label_{};
  std::unique_ptr<Scope> scope_;
  Lexer& lexer_;
  std::vector<std::unique_ptr<Var>> globals_;
  std::vector<std::unique_ptr<Var>> locals_;
};

}  // namespace chibicpp