#include "chibicpp/parser/parser.hh"

#include <cassert>

#include "chibicpp/ast/node_dump.hh"
#include "chibicpp/ast/type.hh"
#include "chibicpp/lex/tokenizer.hh"
#include "chibicpp/util/stack_trace.hh"

namespace chibicpp {

/// \brief program ::= function | globle variable
///
/// \return
std::unique_ptr<Program> Parser::parse_program() {
  STACK_GUARD();

  std::vector<std::unique_ptr<Function>> prog;

  while (!lexer_.is_eof()) {
    if (is_function()) {
      auto func = parse_function();
      prog.push_back(std::move(func));

      // TODO(gc): Trace one function only?
      stack_tracer.clear();
    } else {
      parse_global_var();
    }
  }

  return std::make_unique<Program>(scope_.release_globals(), std::move(prog));
}

/// \brief function-prototype ::= basetype ident "(" param* ")"
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

/// \brief function ::= basetype ident "(" params? ")" "{" stmt* "}"
///        params   ::= param ("," param)*
///        param    ::= basetype ident
///
/// @return
std::unique_ptr<Function> Parser::parse_function() {
  STACK_GUARD();

  scope_.enter(Scope::kFnScope);
  auto proto = parse_function_prototype();
  auto body = parse_function_body();
  scope_.leave();

  return std::make_unique<Function>(
      std::move(proto), std::move(body), scope_.release_locals(),
      scope_.release_string_literals(), scope_.release_statics());
}

/// \brief stmt ::= "return" expr ";"
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
    auto node = Node::make_a_unary(Node::kReturn, parse_expr());
    lexer_.expect(";");

    return node;
  }

  if (lexer_.try_consume("if")) {
    lexer_.expect("(");
    auto cond = parse_expr();
    lexer_.expect(")");
    auto then = parse_stmt();
    std::unique_ptr<Node> els;

    if (lexer_.try_consume("else")) {
      els = parse_stmt();
    }

    return Node::make_a_if(std::move(cond), std::move(then), std::move(els));
  }

  if (lexer_.try_consume("while")) {
    lexer_.expect("(");
    auto cond = parse_expr();
    lexer_.expect(")");
    auto then = parse_stmt();

    return Node::make_a_while(std::move(cond), std::move(then));
  }

  if (lexer_.try_consume("for")) {
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

    return Node::make_a_for(std::move(init), std::move(cond), std::move(inc),
                            std::move(then));
  }

  if (lexer_.try_consume("{")) {
    std::vector<std::unique_ptr<Node>> body;

    scope_.enter(Scope::kBlockScope);

    while (!lexer_.try_consume("}")) {
      body.push_back(parse_stmt());
    }

    scope_.leave();

    return Node::make_a_block(std::move(body));
  }

  if (is_typename()) {
    return parse_declaration();
  }

  auto node = parse_expr_stmt();
  lexer_.expect(";");

  return node;
}

/// \brief expr ::= assign
///
/// \return
std::unique_ptr<Node> Parser::parse_expr() {
  STACK_GUARD();

  return parse_assign();
}

/// \brief assign ::= equality ("=" assign)?
///
/// \return
std::unique_ptr<Node> Parser::parse_assign() {
  STACK_GUARD();

  auto node = parse_equality();

  if (lexer_.try_consume("=")) {
    node = Node::make_a_binary(Node::kAssign, std::move(node), parse_assign());
  }

  return node;
}

/// \brief equality ::= relational ("==" relational | "!=" relational)*
///
/// \return
std::unique_ptr<Node> Parser::parse_equality() {
  STACK_GUARD();

  auto node = parse_relational();

  for (;;) {
    if (lexer_.try_consume("==")) {
      node =
          Node::make_a_binary(Node::kEq, std::move(node), parse_relational());
    } else if (lexer_.try_consume("!=")) {
      node =
          Node::make_a_binary(Node::kNe, std::move(node), parse_relational());
    } else {
      return node;
    }
  }

  /// Not Reachable.
  assert(false);
}

/// \brief relational ::= add ("<" add | "<=" add | ">" add | ">=" add)*
///
/// \return
std::unique_ptr<Node> Parser::parse_relational() {
  STACK_GUARD();

  auto node = parse_add();

  for (;;) {
    if (lexer_.try_consume("<")) {
      node = Node::make_a_binary(Node::kLt, std::move(node), parse_add());
    } else if (lexer_.try_consume("<=")) {
      node = Node::make_a_binary(Node::kLe, std::move(node), parse_add());
    } else if (lexer_.try_consume(">")) {
      node = Node::make_a_binary(Node::kLt, parse_add(), std::move(node));
    } else if (lexer_.try_consume(">=")) {
      node = Node::make_a_binary(Node::kLe, parse_add(), std::move(node));
    } else {
      return node;
    }
  }

  /// Not Reachable.
  assert(false);
}

/// \brief add ::= mul ("+" mul | "-" mul)*
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

/// \brief mul ::= unary ("*" unary | "/" unary)*
///
/// \return
std::unique_ptr<Node> Parser::parse_mul() {
  STACK_GUARD();

  auto node = parse_unary();

  for (;;) {
    if (lexer_.try_consume("*")) {
      node = Node::make_a_binary(Node::kMul, std::move(node), parse_unary());
    } else if (lexer_.try_consume("/")) {
      node = Node::make_a_binary(Node::kDiv, std::move(node), parse_unary());
    } else {
      return node;
    }
  }

  __builtin_unreachable();
}

/// \brief unary ::= ("+" | "-" | "*" | "&")? unary
///                | postfix
///
/// \return
std::unique_ptr<Node> Parser::parse_unary() {
  STACK_GUARD();

  if (lexer_.try_consume("+")) {
    return parse_unary();
  }

  if (lexer_.try_consume("-")) {
    return Node::make_a_binary(Node::kSub, Node::make_a_number(0),
                               parse_unary());
  }

  if (lexer_.try_consume("&")) {
    return Node::make_a_unary(Node::kAddr, parse_unary());
  }

  if (lexer_.try_consume("*")) {
    return Node::make_a_unary(Node::kDeref, parse_unary());
  }

  return parse_postfix();
}

/// \brief C99 6.5.2 Postfix operators
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
      node = Node::make_a_unary(Node::kDeref, std::move(exp));

      continue;
    }

    if (lexer_.try_consume(".")) {
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

  return Node::make_a_member(std::move(node), member);
}

/// \brief declaration ::= basetype ident ("[" num "]")* ("=" expr) ";"
///
/// \return
std::unique_ptr<Node> Parser::parse_declaration() {
  STACK_GUARD();

  Token ident;
  auto type = parse_basetype();
  lexer_.expect_identider(ident);
  type = parse_type_suffix(type);
  auto var = scope_.create_local_var(ident.as_str(), type);

  if (lexer_.try_consume(";")) {
    return std::make_unique<Node>(Node::kEmpty);
  }

  lexer_.expect("=");
  auto lhs = Node::make_a_var(var);
  auto rhs = parse_expr();
  lexer_.expect(";");

  auto node =
      Node::make_a_binary(Node::kAssign, std::move(lhs), std::move(rhs));

  return Node::make_a_unary(Node::kExprStmt, std::move(node));
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

    return Node::make_a_number(node->type()->size_in_bytes());
  }

  if (lexer_.try_consume(TokenKind::kIdentifier, token)) {
    // Check function call.
    if (lexer_.try_consume("(")) {
      return Node::make_a_function_call(token.as_str(), parse_func_args());
    }

    // Variable.
    auto const& ident = token.as_str();
    auto var = scope_.get_var(ident);

    if (!var) {
      CHIBICPP_THROW_ERROR(ident + " is not defined.");
    }

    return Node::make_a_var(var);
  }

  // String literal.
  if (lexer_.try_consume(TokenKind::kStrLiteral, token)) {
    // Add an extra terminate '\0'.
    auto const& str = token.as_str();
    auto type = TypeMgr::get_array(str.size() + 1, TypeMgr::get_char());
    auto var = scope_.create_string_literal(str, type);

    return Node::make_a_var(var);
  }

  lexer_.expect_number(token);

  return Node::make_a_number(token.as_i64());
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

  return TypeMgr::get_array(num.as_i64(), base);
}

/// \brief basetype ::= ("char" | "int") "*"*
///
/// \return
ObserverPtr<Type> Parser::parse_basetype() {
  STACK_GUARD();

  ObserverPtr<Type> type;

  if (lexer_.try_consume("char")) {
    type = TypeMgr::get_char();
  } else if (lexer_.try_consume("int")) {
    type = TypeMgr::get_integer(Type::kInt);
  } else {
    type = parse_struct_decl();
  }

  while (lexer_.try_consume("*")) {
    type = TypeMgr::get_pointer(type);
  }

  return type;
}

bool Parser::is_function() {
  STACK_GUARD();

  lexer_.mark();

  (void)parse_basetype();
  bool is_func =
      lexer_.try_consume(TokenKind::kIdentifier, Token::dummy_eof()) &&
      lexer_.try_consume("(");

  lexer_.reset();

  return is_func;
}

bool Parser::is_typename() {
  static const std::vector<char const*> kTypenames{"char", "int", "struct"};

  return std::find_if(kTypenames.begin(), kTypenames.end(), [this](auto key) {
           return lexer_.try_peek(key, Token::dummy_eof());
         }) != kTypenames.end();
}

std::unique_ptr<Node> Parser::parse_expr_stmt() {
  STACK_GUARD();

  return Node::make_a_unary(Node::kExprStmt, parse_expr());
}

/// \brief func-param ::= basetype args*
///                     | empty
///
/// \return
ObserverPtr<Var> Parser::parse_func_param() {
  STACK_GUARD();

  Token ident;
  auto type = parse_basetype();
  lexer_.expect_identider(ident);
  type = parse_type_suffix(type);

  return scope_.create_local_var(ident.as_str(), type);
}

/// \brief func-params ::= "(" args ")"
///        args        ::= empty
///                      | arg (,arg)*
///
/// @return
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

  // for (auto p : params) {
  //   std::cout << std::quoted(p->name) << ':' << p->type->to_string()
  //             << std::endl;
  // }

  return params;
}

/// \brief func-args ::= "(" (assign ("," assign)*)? ")"
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

/// \brief global-var ::= basetype ident "("[" num "]")*" ";"
void Parser::parse_global_var() {
  STACK_GUARD();

  Token ident;
  auto type = parse_basetype();
  lexer_.expect_identider(ident);
  type = parse_type_suffix(type);
  lexer_.expect(";");
  scope_.create_global_var(ident.as_str(), type);
}

/// \brief stmt-expr ::= "(" "{" stmt stmt* "}" ")"
///
/// Statement expression is a GNU C extension.
///
/// \return
std::unique_ptr<Node> Parser::parse_stmt_expr() {
  STACK_GUARD();

  std::vector<std::unique_ptr<Node>> body;

  scope_.enter(Scope::kBlockScope);
  body.push_back(parse_stmt());

  while (!lexer_.try_consume("}")) {
    body.push_back(parse_stmt());
  }

  lexer_.expect(")");
  scope_.leave();

  // Ensure the last statement in the body is an expression statement.
  if (body.back()->id() != Node::kExprStmt) {
    CHIBICPP_THROW_ERROR(
        "Statement expression returning void is not supported.");
  }

  // for (auto& n : body) {
  //   std::cout << n->type()->to_string() << std::endl;
  // }

  return Node::make_a_stmt_expr(std::move(body));
}

/// \brief struct-decl ::= "struct" "{" struct-member "}"
///
/// \return
ObserverPtr<Type> Parser::parse_struct_decl() {
  STACK_GUARD();

  lexer_.expect("struct");

  Token ident;
  std::string struct_name;

  if (lexer_.try_consume(TokenKind::kIdentifier, ident)) {
    struct_name = ident.as_str();
  } else {
    // TODO(gc): Line number can't obtained in parser.
    struct_name = gen_anonymous_struct_name(0);
  }

  lexer_.expect("{");

  std::vector<std::unique_ptr<Member>> members;

  while (!lexer_.try_consume("}")) {
    members.push_back(parse_struct_member());
  }

  return TypeMgr::get_struct(struct_name, std::move(members));
}

/// \brief struct-member ::= basetype ident ("[" num "]")* ";"
std::unique_ptr<Member> Parser::parse_struct_member() {
  STACK_GUARD();

  Token ident;

  auto type = parse_basetype();
  lexer_.expect_identider(ident);
  type = parse_type_suffix(type);
  lexer_.expect(";");

  return std::make_unique<Member>(ident.as_str(), type);
}

}  // namespace chibicpp