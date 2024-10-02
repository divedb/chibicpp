#include "chibicpp/parser/parser.hh"

#include <cassert>

#include "chibicpp/ast/node_dump.hh"
#include "chibicpp/ast/type.hh"
#include "chibicpp/lex/tokenizer.hh"
#include "chibicpp/util/stack_trace.hh"

namespace chibicpp {

/// program ::= function | globle variable
///
/// \return
std::unique_ptr<Program> Parser::parse_program() {
  STACK_GUARD();

  std::vector<std::unique_ptr<Function>> prog;

  while (!lexer_.is_eof()) {
    if (is_function()) {
      set_fn_scope();
      prog.push_back(parse_function());

      // TODO(gc): Trace one function only?
      stack_tracer.clear();
    } else {
      set_pg_scope();
      parse_global_var();
    }
  }

  set_pg_scope();

  return std::make_unique<Program>(std::move(prog),
                                   scope()->release_symbol_table());
}

/// function-prototype ::= basetype ident "(" param* ")"
///
/// \return
std::unique_ptr<Function::Prototype> Parser::parse_function_prototype() {
  STACK_GUARD();

  Token ident;
  auto ret_type = parse_basetype();
  lexer_.expect_identider(ident);
  lexer_.expect("(");
  auto params = parse_func_params();

  return std::make_unique<Function::Prototype>(ret_type, ident.as_str(),
                                               params);
}

std::vector<std::unique_ptr<Node>> Parser::parse_function_body() {
  STACK_GUARD();

  std::vector<std::unique_ptr<Node>> body;

  lexer_.expect("{");
  while (!lexer_.try_consume("}")) {
    auto node = parse_stmt();
    body.push_back(std::move(node));
  }

  return body;
}

/// function ::= basetype ident "(" params? ")" "{" stmt* "}"
/// params   ::= param ("," param)*
/// param    ::= basetype ident
///
/// @return
std::unique_ptr<Function> Parser::parse_function() {
  STACK_GUARD();

  scope()->enter(Scope::kFnScope);
  auto proto = parse_function_prototype();
  auto body = parse_function_body();
  scope()->leave();

  return std::make_unique<Function>(std::move(proto), std::move(body),
                                    scope()->release_symbol_table());
}

/// C99 6.8 Statements and blocks.
///
/// statement ::= labeled-statement
///             | compound-statement
///             | expression-statement
///             | selection-statement
///             | iteration-statement
///             | jump-statement
///
/// C99 6.8.1 Labeled statements.
///
/// labeled-statement ::= identifier:               statement
///                     | case constant-expression: statement
///                     | default:                  statement
///
/// C99 6.8.2 Compound statement
///
/// compound-statement ::= "{" block-item-list? "}"
/// block-item0list    ::= block-item
///                      | block-item-list block-item
/// block-item         ::= declaration
///                      | statement

/// stmt ::= "return" expr ";"
///               | "if" "(" expr ")" stmt ("else" stmt)?
///               | "while" "(" expr ")" stmt
///               | "for" "(" expr? ";" expr? ";" expr? ")" stmt
///               | "{" stmt* "}"
///               | declaration
///               | expr ";"
/// \return
std::unique_ptr<Node> Parser::parse_stmt() {
  STACK_GUARD();

  if (lexer_.try_consume("return")) {
    auto node = Node::make_unary(Node::kReturn, parse_expr());
    lexer_.expect(";");

    return node;
  }

  if (lexer_.try_consume("if")) {
    return parse_if_stmt();
  }

  if (lexer_.try_consume("while")) {
    return parse_while_stmt();
  }

  if (lexer_.try_consume("for")) {
    return parse_for_stmt();
  }

  if (lexer_.try_consume("{")) {
    return parse_block_stmt();
  }

  if (lexer_.try_consume("typedef")) {
    return parse_typedef_stmt();
  }

  if (is_typename()) {
    return parse_declaration();
  }

  auto node = parse_expr_stmt();
  lexer_.expect(";");

  return node;
}

/// iteration-statement ::= "while" "(" expr ")" stmt
///                       | "do" stmt "while" "(" expr ")"
///                       | "for" "("
/// \return
std::unique_ptr<Node> Parser::parse_while_stmt() {
  lexer_.expect("(");
  auto cond = parse_expr();
  lexer_.expect(")");
  auto then = parse_stmt();

  return Node::make_while(std::move(cond), std::move(then));
}

std::unique_ptr<Node> Parser::parse_for_stmt() {
  std::unique_ptr<Node> init;
  std::unique_ptr<Node> cond;
  std::unique_ptr<Node> inc;
  std::unique_ptr<Node> then;

  lexer_.expect("(");

  if (!lexer_.try_consume(";")) {
    init = parse_expr_stmt();
    lexer_.expect(";");
  }

  if (!lexer_.try_consume(";")) {
    cond = parse_expr();
    lexer_.expect(";");
  }

  if (!lexer_.try_consume(")")) {
    inc = parse_expr_stmt();
    lexer_.expect(")");
  }

  then = parse_stmt();

  return Node::make_for(std::move(init), std::move(cond), std::move(inc),
                        std::move(then));
}

std::unique_ptr<Node> Parser::parse_block_stmt() {
  std::vector<std::unique_ptr<Node>> body;

  scope()->enter(Scope::kBlockScope);

  while (!lexer_.try_consume("}")) {
    body.push_back(parse_stmt());
  }

  scope()->leave();

  return Node::make_block(std::move(body));
}

std::unique_ptr<Node> Parser::parse_typedef_stmt() {
  Token ident;
  auto type_def = parse_basetype();
  lexer_.expect_identider(ident);
  type_def = parse_type_suffix(type_def);
  lexer_.expect(";");

  auto type = TypeFactory::get_typedef(ident.as_str());
  (void)scope()->create_typedef(ident.as_str(), type, type_def);

  return Node::make_empty();
}

std::unique_ptr<Node> Parser::parse_if_stmt() {
  lexer_.expect("(");
  auto cond = parse_expr();
  lexer_.expect(")");
  auto then = parse_stmt();
  std::unique_ptr<Node> els;

  if (lexer_.try_consume("else")) {
    els = parse_stmt();
  }

  return Node::make_if(std::move(cond), std::move(then), std::move(els));
}

/// expr ::= assign
///
/// \return
std::unique_ptr<Node> Parser::parse_expr() {
  STACK_GUARD();

  return parse_assign();
}

/// assign ::= equality ("=" assign)?
///
/// \return
std::unique_ptr<Node> Parser::parse_assign() {
  STACK_GUARD();

  auto node = parse_equality();

  if (lexer_.try_consume("=")) {
    node = Node::make_binary(Node::kAssign, std::move(node), parse_assign());
  }

  return node;
}

/// equality ::= relational ("==" relational | "!=" relational)*
///
/// \return
std::unique_ptr<Node> Parser::parse_equality() {
  STACK_GUARD();

  auto node = parse_relational();

  for (;;) {
    if (lexer_.try_consume("==")) {
      node = Node::make_binary(Node::kEq, std::move(node), parse_relational());
    } else if (lexer_.try_consume("!=")) {
      node = Node::make_binary(Node::kNe, std::move(node), parse_relational());
    } else {
      return node;
    }
  }

  /// Not Reachable.
  assert(false);
}

/// relational ::= add ("<" add | "<=" add | ">" add | ">=" add)*
///
/// \return
std::unique_ptr<Node> Parser::parse_relational() {
  STACK_GUARD();

  auto node = parse_add();

  for (;;) {
    if (lexer_.try_consume("<")) {
      node = Node::make_binary(Node::kLt, std::move(node), parse_add());
    } else if (lexer_.try_consume("<=")) {
      node = Node::make_binary(Node::kLe, std::move(node), parse_add());
    } else if (lexer_.try_consume(">")) {
      node = Node::make_binary(Node::kLt, parse_add(), std::move(node));
    } else if (lexer_.try_consume(">=")) {
      node = Node::make_binary(Node::kLe, parse_add(), std::move(node));
    } else {
      return node;
    }
  }

  /// Not Reachable.
  assert(false);
}

/// add ::= mul ("+" mul | "-" mul)*
///
/// \return
std::unique_ptr<Node> Parser::parse_add() {
  STACK_GUARD();

  auto node = parse_mul();

  for (;;) {
    if (lexer_.try_consume("+")) {
      node = Node::make_add(std::move(node), parse_mul());
    } else if (lexer_.try_consume("-")) {
      node = Node::make_sub(std::move(node), parse_mul());
    } else {
      return node;
    }
  }

  __builtin_unreachable();
}

/// mul ::= unary ("*" unary | "/" unary)*
///
/// \return
std::unique_ptr<Node> Parser::parse_mul() {
  STACK_GUARD();

  auto node = parse_unary();

  for (;;) {
    if (lexer_.try_consume("*")) {
      node = Node::make_binary(Node::kMul, std::move(node), parse_unary());
    } else if (lexer_.try_consume("/")) {
      node = Node::make_binary(Node::kDiv, std::move(node), parse_unary());
    } else {
      return node;
    }
  }

  __builtin_unreachable();
}

/// unary ::= ("+" | "-" | "*" | "&")? unary
///                | postfix
///
/// \return
std::unique_ptr<Node> Parser::parse_unary() {
  STACK_GUARD();

  if (lexer_.try_consume("+")) {
    return parse_unary();
  }

  if (lexer_.try_consume("-")) {
    return Node::make_binary(Node::kSub, Node::make_number(0), parse_unary());
  }

  if (lexer_.try_consume("&")) {
    return Node::make_unary(Node::kAddr, parse_unary());
  }

  if (lexer_.try_consume("*")) {
    return Node::make_unary(Node::kDeref, parse_unary());
  }

  return parse_postfix();
}

/// C99 6.5.2 Postfix operators
///
/// postfix-expression ::= primary-expression
///                      | postfix-expression "[" expression "]"
///                      | postfix-expression "(" argument-expression-list? ")"
///                      | postfix-expression "." identifier
///                      | postfix-expression "->" identifier
///                      | postfix-expression "++"
///                      | postfix-expression "--"
///                      | "(" type-name ")" "{" initialize-list "}"
///                      | "(" type-name ")" "{" initialize-list "," "}"
/// \return
std::unique_ptr<Node> Parser::parse_postfix() {
  STACK_GUARD();

  Token token;
  auto node = parse_primary();

  while (true) {
    if (lexer_.try_consume("[")) {
      // x[y] is short for *(x+y)
      auto exp = Node::make_add(std::move(node), parse_expr());
      lexer_.expect("]");
      node = Node::make_unary(Node::kDeref, std::move(exp));

      continue;
    }

    if (lexer_.try_consume(".")) {
      node = parse_struct_ref(std::move(node));

      continue;
    }

    if (lexer_.try_consume("->")) {
      // x->y is short for (*x).y
      node = Node::make_unary(Node::kDeref, std::move(node));
      node = parse_struct_ref(std::move(node));

      continue;
    }

    break;
  }

  return node;
}

std::unique_ptr<Node> Parser::parse_struct_ref(std::unique_ptr<Node> node) {
  STACK_GUARD();

  auto type = node->type();

  if (!type->is_struct()) {
    CHIBICPP_THROW_ERROR("Expect a `struct` got a ", node->type()->id());
  }

  Token ident;
  lexer_.expect_identider(ident);
  const auto& name = ident.as_str();
  auto member = static_cast<const StructType*>(type.get())->find_member(name);

  if (!member) {
    CHIBICPP_THROW_ERROR("No such member: ", name, " in ",
                         static_cast<StructType*>(type.get())->name());
  }

  return Node::make_member(std::move(node), member);
}

/// declaration ::= basetype ident ("[" num "]")* ("=" expr) ";"
///               | basetype ";"
///
/// For example: The following code is legal with warning:
///   warning: declaration does not declare anything [-Wmissing-declarations]
///
/// \code
/// int;
/// struct Person;
/// \endcode
///
/// \return A pointer to node.

// clang-format off
/// C99 6.7 Declarations.
///
/// Syntax
/// declaration            ::= declaration-specifiers  init-declarator-list? ";"
/// declaration-specifiers ::= storage-class-specifier declaration-specifiers?
///                          | type-specifier          declaration-specifiers?
///                          | type-qualifier          declaration-specifiers?
///                          | function-specifier      declaration-specifiers?
/// init-declarator-list   ::= init-declarator
///                          | init-declarator-list "," init-declarator
/// init-declarator        ::= declarator
///                          | declarator = initializer
/// declarator             ::= pointer? direct-declarator
/// direct-declarator      ::= identifier
///                          | "(" declarator ")"
///                          | direct-declarator "[" "static" type-qualifier-list? assignment-expression "]"
///                          | direct-declarator "[" type-qualifier-list  "static" assignment-expression "]"
///                          | direct-declarator "[" type-qualifier-list?  "*" "]"
///                          | direct-declarator "(" parameter-type-list ")"
///                          | direct-declarator "(" identifier-list?    ")"
/// pointer                ::= "*" type-qualifier-list?
///                          | "*" type-qualifier-list? pointer
/// type-qualifier-list    ::= type-qualifier
///                          | type-qualifier-list type-qualifier
/// parameter-type-list    ::= parameter-list
///                          | parameter-list "," "..."
/// parameter-list         ::= parameter-declaration
///                          | parameter-list "," parameter-declaration
/// parameter-declaration  ::= declaration-specifiers declarator
///                          | declaration-specifiers abstract-declarator?
/// identifier-list        ::= identifier
///                          | identifier-list "," identifier
///
/// \code
/// int(a) = 100;
/// \endcode
/// \return
// clang-format on
std::unique_ptr<Node> Parser::parse_declaration() {
  STACK_GUARD();

  auto type = parse_basetype();

  if (lexer_.try_consume(";")) {
    return Node::make_empty();
  }

  // Case 1: `int a[8];`.
  Token ident;
  lexer_.expect_identider(ident);
  type = parse_type_suffix(type);
  auto var = scope()->create_var(ident.as_str(), type);

  if (lexer_.try_consume(";")) {
    return Node::make_empty();
  }

  // Case 2: `int a = 8;`.
  lexer_.expect("=");
  auto lhs = Node::make_var(var);
  auto rhs = parse_expr();
  lexer_.expect(";");

  auto node = Node::make_binary(Node::kAssign, std::move(lhs), std::move(rhs));

  return Node::make_unary(Node::kExprStmt, std::move(node));
}

/// primary ::= "(" "{" stmt-expr-tail
///           | "(" expr ")"
///           | "sizeof" unary
///           | ident func-args?
///           | str
///           | num
std::unique_ptr<Node> Parser::parse_primary() {
  STACK_GUARD();

  if (lexer_.try_consume("(")) {
    if (lexer_.try_consume("{")) {
      return parse_stmt_expr();
    }

    auto node = parse_expr();
    lexer_.expect(")");

    return node;
  }

  Token token;

  if (lexer_.try_consume("sizeof")) {
    auto node = parse_unary();

    return Node::make_number(node->type()->size_in_bytes());
  }

  if (lexer_.try_consume(TokenKind::kIdentifier, token)) {
    // Check function call.
    if (lexer_.try_consume("(")) {
      return Node::make_function_call(token.as_str(), parse_func_args());
    }

    // Variable.
    auto const& ident = token.as_str();
    auto var = scope()->search_var(ident);

    if (!var) {
      CHIBICPP_THROW_ERROR(ident + " is not defined.");
    }

    return Node::make_var(var);
  }

  // String literal.
  if (lexer_.try_consume(TokenKind::kStrLiteral, token)) {
    // Add an extra terminate '\0'.
    auto const& str = token.as_str();
    auto type = TypeFactory::get_array(str.size() + 1, TypeFactory::get_char());
    auto var = scope()->create_string_literal(str, type);

    return Node::make_var(var);
  }

  lexer_.expect_number(token);

  return Node::make_number(token.as_i64());
}

ObserverPtr<Type> Parser::parse_type_suffix(ObserverPtr<Type> base) {
  STACK_GUARD();

  if (!lexer_.try_consume("[")) {
    return base;
  }

  Token num;
  lexer_.expect_number(num);
  lexer_.expect("]");
  base = parse_type_suffix(base);

  return TypeFactory::get_array(num.as_i64(), base);
}

/// basetype        ::= type-specifier "*"*
/// type-speicifier ::= void
///                   | char
///                   | short
///                   | int
///                   | long
///                   | float
///                   | double
///                   | signed
///                   | _Bool
///                   | _Imaginary
///                   | struct-or-union-specifier
///                   | enum-specifier
///                   | typedef-name
/// \return
ObserverPtr<Type> Parser::parse_basetype() {
  STACK_GUARD();

  ObserverPtr<Type> type;

  if (lexer_.try_consume("char")) {
    type = TypeFactory::get_char();
  } else if (lexer_.try_consume("short")) {
    type = TypeFactory::get_signed_short();
  } else if (lexer_.try_consume("int")) {
    type = TypeFactory::get_signed_int();
  } else if (lexer_.try_consume("long")) {
    type = TypeFactory::get_signed_long();
  } else if (lexer_.try_consume("struct")) {
    type = parse_struct_decl();
  } else {
    // The type must be typedef.
    Token ident;

    lexer_.expect_identider(ident);
    auto var = scope()->search_var(ident.as_str());

    assert(var && var->is_typedef());

    type = var->type_def();
  }

  while (lexer_.try_consume("*")) {
    type = TypeFactory::get_pointer(type);
  }

  return type;
}

bool Parser::is_function() {
  STACK_GUARD();

  lexer_.mark();

  (void)parse_basetype();
  bool is_func = lexer_.try_consume(TokenKind::kIdentifier, Token::dummy()) &&
                 lexer_.try_consume("(");

  lexer_.reset();

  return is_func;
}

bool Parser::is_typename() {
  static const char* kTypenames[]{"char", "short", "int", "long", "struct"};

  Token token;

  // Check typename first.
  for (auto kw : kTypenames) {
    if (lexer_.try_peek(kw)) {
      return true;
    }
  }

  // Search typedef.
  if (!lexer_.try_peek(TokenKind::kIdentifier, token)) {
    return false;
  }

  auto var = scope()->search_var(token.as_str());

  if (!var || !var->is_typedef()) {
    return false;
  }

  return true;
}

/// C99 6.8.3 Expression and null statements.
///
/// Syntax
/// expr-stmt ::= expr ";"
///             | ";"
///
/// 1. The expression in an expression statement is evaluated as a void
///    expression for its side effects.
/// 2. A null statement (consisting of just a semicolon) performs no operations.
/// 3. If a function call is evaluated as an expression statement for its side
///    effects only, the discarding of its value may be made explicit by
///    converting the expression to a void expression by means of a cast.
///    \code
///    int p(int);
///    (void)p(0);
///    \endcode
/// 4. A null statement is used to supply an empty loop body to the iteration
///    statement.
///    \code
///    char* s;
///    while (*s++ != '\0')
///      ;
///    \endcode
/// 5. A null statement may also be used to carry a label just before the
///    closing } of a compound statement.
///    \code
///    while (loop1) {
///      while (loop2) {
///        if (want_out)
///          goto end_loop1;
///      }
///    end_loop1: ;
///    }
///    \endcode
/// @return
std::unique_ptr<Node> Parser::parse_expr_stmt() {
  STACK_GUARD();

  return Node::make_unary(Node::kExprStmt, parse_expr());
}

/// func-param ::= basetype args*
///              | empty
///
/// \return
ObserverPtr<Var> Parser::parse_func_param() {
  STACK_GUARD();

  Token ident;
  auto type = parse_basetype();
  lexer_.expect_identider(ident);
  type = parse_type_suffix(type);

  return scope()->create_var(ident.as_str(), type);
}

/// func-params ::= "(" args ")"
/// args        ::= empty
///               | arg (,arg)*
///
/// \return
std::vector<ObserverPtr<Var>> Parser::parse_func_params() {
  STACK_GUARD();

  if (lexer_.try_consume(")")) {
    return {};
  }

  std::vector<ObserverPtr<Var>> params;
  params.push_back(parse_func_param());

  while (!lexer_.try_consume(")")) {
    lexer_.expect(",");
    params.push_back(parse_func_param());
  }

  return params;
}

/// func-args ::= "(" (assign ("," assign)*)? ")"
///
/// \return
std::vector<std::unique_ptr<Node>> Parser::parse_func_args() {
  STACK_GUARD();

  if (lexer_.try_consume(")")) {
    return {};
  }

  std::vector<std::unique_ptr<Node>> args;
  args.push_back(parse_assign());

  while (lexer_.try_consume(",")) {
    args.push_back(parse_assign());
  }

  lexer_.expect(")");

  return args;
}

/// global-var ::= basetype ident "("[" num "]")*" ";"
void Parser::parse_global_var() {
  STACK_GUARD();

  Token ident;
  auto type = parse_basetype();
  lexer_.expect_identider(ident);
  type = parse_type_suffix(type);
  lexer_.expect(";");
  scope()->create_var(ident.as_str(), type);
}

/// stmt-expr ::= "(" "{" stmt stmt* "}" ")"
///
/// Statement expression is a GNU C extension.
///
/// \return
std::unique_ptr<Node> Parser::parse_stmt_expr() {
  STACK_GUARD();

  std::vector<std::unique_ptr<Node>> body;

  scope()->enter(Scope::kBlockScope);
  body.push_back(parse_stmt());

  while (!lexer_.try_consume("}")) {
    body.push_back(parse_stmt());
  }

  lexer_.expect(")");
  scope()->leave();

  // Ensure the last statement in the body is an expression statement.
  if (body.back()->id() != Node::kExprStmt) {
    CHIBICPP_THROW_ERROR(
        "Statement expression returning void is not supported.");
  }

  // for (auto& n : body) {
  //   std::cout << n->type()->to_string() << std::endl;
  // }

  return Node::make_stmt_expr(std::move(body));
}

/// struct-decl ::= "struct" ident
///               | "struct" ident? "{" struct-member "}"
///
/// \return A pointer to the struct type.
ObserverPtr<Type> Parser::parse_struct_decl() {
  STACK_GUARD();

  // Consume the keyword.
  lexer_.expect("struct");

  Token tag;
  bool has_tag = lexer_.try_consume_identifier(tag);

  // Read a struct tag.
  // For example: `struct Person x;`.
  if (has_tag && !lexer_.try_peek("{", Token::dummy())) {
    auto var = find_tag(tag.as_str());

    if (!var) {
      CHIBICPP_THROW_ERROR("Unknown struct type: ", tag.as_str());
    }

    return var->type();
  }

  // Then it must be struct declaration.
  // For example: `struct Person { int age; }`.
  lexer_.expect("{");

  std::vector<std::unique_ptr<Member>> members;

  while (!lexer_.try_consume("}")) {
    members.push_back(parse_struct_member());
  }

  // An anonymous struct name will be generated if a tag name was not given.
  auto tag_name = has_tag ? tag.as_str() : gen_anonymous_struct_name(__LINE__);
  auto type = TypeFactory::get_struct(tag_name, std::move(members));

  // Push it into scope.
  scope()->create_tag(tag_name, type);

  return type;
}

/// struct-member ::= basetype ident ("[" num "]")* ";"
std::unique_ptr<Member> Parser::parse_struct_member() {
  STACK_GUARD();

  Token ident;

  auto type = parse_basetype();
  lexer_.expect_identider(ident);
  type = parse_type_suffix(type);
  lexer_.expect(";");

  return std::make_unique<Member>(ident.as_str(), type);
}

ObserverPtr<Var> Parser::find_tag(const std::string& ident) {
  // Find the function scope first.
  auto var = fn_scope_.search_tag(ident);

  if (var) {
    return var;
  }

  return pg_scope_.search_tag(ident);
}

}  // namespace chibicpp