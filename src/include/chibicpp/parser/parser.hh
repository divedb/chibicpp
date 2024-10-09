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

class Parser {
 public:
  /// Constructs a parser using the specified lexer.
  ///
  /// \param lexer The lexer that supplies the tokens.
  explicit Parser(Lexer& lexer) : lexer_{lexer} {}

  std::unique_ptr<Program> parse_program();
  std::unique_ptr<Function> parse_function();
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

  /// stmt ::= "return" expr ";"
  ///        | "if" "(" expr ")" stmt ( "else" stmt )?
  ///        | expr ";"
  ///        |
  ///
  /// \return
  std::unique_ptr<Node> parse_stmt();

  /// while-stmt ::= "while" "(" condition ")" stmt
  std::unique_ptr<Node> parse_while_stmt();
  std::unique_ptr<Node> parse_for_stmt();
  std::unique_ptr<Node> parse_block_stmt();
  std::unique_ptr<Node> parse_typedef_stmt();
  std::unique_ptr<Node> parse_if_stmt();

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

 private:
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
    return "__anonymous_struct__" + std::to_string(lineno);
  }

  /// Determine whether the next top-level item is a function or a global
  ///        variable by looking ahead input tokens.
  ///
  /// \return `true` if it's a function, otherwise `false`.
  bool is_function();

  bool is_typename();

  /// @}
  ObserverPtr<VarScope> scope() const { return context_.cur_scope(); }

  class ScopeContext {
   public:
    ScopeContext()
        : lscope_{std::make_unique<VarScope>(Scope::kFnScope)},
          gscope_{std::make_unique<VarScope>(Scope::kTranslationUnitScope)},
          cur_scope_{gscope_.get()} {}

    void set_local_scope() { cur_scope_ = lscope_.get(); }
    void set_global_scope() { cur_scope_ = gscope_.get(); }
    ObserverPtr<VarScope> cur_scope() const { return cur_scope_; }

    /// Searches for a variable in the current context using the provided
    /// identifier.
    ///
    /// \param ident The name of the variable to search for.
    /// \return A pointer to the var if found; nullptr otherwise.
    ObserverPtr<Var> search_var(const std::string& ident) {
      // If we are inside function, search function scope first.
      if (cur_scope_.get() == lscope_.get()) {
        auto var = lscope_->search_var(ident);

        if (var) {
          return var;
        }
      }

      return gscope_->search_var(ident);
    }

    ObserverPtr<Var> search_tag(const std::string& ident) {
      if (cur_scope_.get() == lscope_.get()) {
        auto tag = lscope_->search_tag(ident);

        if (tag) {
          return tag;
        }
      }

      return gscope_->search_tag(ident);
    }

   private:
    std::unique_ptr<VarScope> lscope_;  ///< Local scope.
    std::unique_ptr<VarScope> gscope_;  ///< Global scope.
    ObserverPtr<VarScope> cur_scope_;   ///< Pointer to current scope.
  };

  Lexer& lexer_;
  ScopeContext context_;
};

}  // namespace chibicpp