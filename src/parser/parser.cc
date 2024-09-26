#include "chibicpp/parser/parser.hh"

#include <cassert>

#include "chibicpp/ast/node_dump.hh"
#include "chibicpp/ast/type.hh"
#include "chibicpp/lex/tokenizer.hh"

namespace chibicpp {

namespace {

#define TOKENPASTE(x, y) x##y
#define TOKENPASTE2(x, y) TOKENPASTE(x, y)

#define MAKE_CALL_STACK(pinfo) \
  CallStackGuard TOKENPASTE2(guard_, __COUNTER__)(pinfo, __func__, __LINE__)

static ParserInfo pinfo;

std::ostream& ident(std::ostream& os, int depth) {
  return os << std::string(depth * 2, ' ');
}

void print_call_stack() {
  for (auto const& cs : pinfo.call_stacks) {
    ident(std::cout, cs.depth);
    std::cout << '[' << cs.fname << "]:" << cs.lineno << '\n';
  }
}

}  // namespace

static std::unique_ptr<Node> new_add(std::unique_ptr<Node> lhs,
                                     std::unique_ptr<Node> rhs) {
  add_type(lhs.get());
  add_type(rhs.get());

  if (lhs->is_integer() && rhs->is_integer()) {
    return make_a_binary(NodeKind::kAdd, std::move(lhs), std::move(rhs));
  }

  if (lhs->has_base() && rhs->is_integer()) {
    return make_a_binary(NodeKind::kPtrAdd, std::move(lhs), std::move(rhs));
  }

  if (lhs->is_integer() && rhs->has_base()) {
    return make_a_binary(NodeKind::kPtrAdd, std::move(rhs), std::move(lhs));
  }

  // TODO(gc): Add more error information.
  CHIBICPP_THROW_ERROR("Add: invalid operands");

  __builtin_unreachable();
}

static std::unique_ptr<Node> new_sub(std::unique_ptr<Node> lhs,
                                     std::unique_ptr<Node> rhs) {
  add_type(lhs.get());
  add_type(rhs.get());

  if (lhs->is_integer() && rhs->is_integer()) {
    return make_a_binary(NodeKind::kSub, std::move(lhs), std::move(rhs));
  }

  if (lhs->has_base() && rhs->is_integer()) {
    return make_a_binary(NodeKind::kPtrSub, std::move(lhs), std::move(rhs));
  }

  if (lhs->has_base() && rhs->has_base()) {
    return make_a_binary(NodeKind::kPtrDiff, std::move(lhs), std::move(rhs));
  }

  // TODO(gc): Add more error information.
  CHIBICPP_THROW_ERROR("Subtract: invalid operands");

  __builtin_unreachable();
}

/// \brief program ::= function | globle variable
///
/// \return
std::unique_ptr<Program> Parser::parse_program() {
  MAKE_CALL_STACK(pinfo);
  std::set_terminate(print_call_stack);

  std::vector<std::unique_ptr<Function>> prog;

  while (!lexer_.is_eof()) {
    if (is_function()) {
      auto func = parse_function();
      prog.push_back(std::move(func));
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
  MAKE_CALL_STACK(pinfo);

  Token ident;
  auto ret_type = parse_basetype();
  lexer_.expect_identider(ident);
  lexer_.expect("(");
  auto params = parse_func_params();

  return std::make_unique<Function::Prototype>(ret_type, ident.as_str(),
                                               params);
}

std::vector<std::unique_ptr<Node>> Parser::parse_function_body() {
  MAKE_CALL_STACK(pinfo);

  std::vector<std::unique_ptr<Node>> body;

  lexer_.expect("{");
  while (!lexer_.try_consume("}")) {
    MAKE_CALL_STACK(pinfo);
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
  MAKE_CALL_STACK(pinfo);

  scope_.enter(Scope::kFnScope);
  auto proto = parse_function_prototype();
  auto body = parse_function_body();
  scope_.leave();

  // Update the node's type information.
  for (auto& node : body) {
    add_type(node.get());
  }

  return std::make_unique<Function>(std::move(proto), std::move(body),
                                    std::move(scope_.release_locals()));
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
  MAKE_CALL_STACK(pinfo);

  if (lexer_.try_consume("return")) {
    auto node = make_a_unary(NodeKind::kReturn, parse_expr());
    lexer_.expect(";");

    return node;
  }

  if (lexer_.try_consume("if")) {
    auto node = std::make_unique<Node>(NodeKind::kIf);
    lexer_.expect("(");
    node->cond = parse_expr();
    lexer_.expect(")");
    node->then = parse_stmt();

    if (lexer_.try_consume("else")) {
      node->els = parse_stmt();
    }

    return node;
  }

  if (lexer_.try_consume("while")) {
    auto node = std::make_unique<Node>(NodeKind::kWhile);
    lexer_.expect("(");
    node->cond = parse_expr();
    lexer_.expect(")");
    node->then = parse_stmt();

    return node;
  }

  if (lexer_.try_consume("for")) {
    auto node = std::make_unique<Node>(NodeKind::kFor);
    lexer_.expect("(");

    if (!lexer_.try_consume(";")) {
      node->init = parse_expr_stmt();
      lexer_.expect(";");
    }

    if (!lexer_.try_consume(";")) {
      node->cond = parse_expr();
      lexer_.expect(";");
    }

    if (!lexer_.try_consume(")")) {
      node->inc = parse_expr_stmt();
      lexer_.expect(")");
    }

    node->then = parse_stmt();

    return node;
  }

  if (lexer_.try_consume("{")) {
    std::vector<std::unique_ptr<Node>> body;

    scope_.enter(Scope::kBlockScope);
    while (!lexer_.try_consume("}")) {
      body.push_back(parse_stmt());
    }
    scope_.leave();

    auto node = std::make_unique<Node>(NodeKind::kBlock);
    node->body = std::move(body);

    return node;
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
  MAKE_CALL_STACK(pinfo);

  return parse_assign();
}

/// \brief assign ::= equality ("=" assign)?
///
/// \return
std::unique_ptr<Node> Parser::parse_assign() {
  MAKE_CALL_STACK(pinfo);

  auto node = parse_equality();

  if (lexer_.try_consume("=")) {
    node = make_a_binary(NodeKind::kAssign, std::move(node), parse_assign());
  }

  return node;
}

/// \brief equality ::= relational ("==" relational | "!=" relational)*
///
/// \return
std::unique_ptr<Node> Parser::parse_equality() {
  MAKE_CALL_STACK(pinfo);

  auto node = parse_relational();

  for (;;) {
    if (lexer_.try_consume("==")) {
      node = make_a_binary(NodeKind::kEq, std::move(node), parse_relational());
    } else if (lexer_.try_consume("!=")) {
      node = make_a_binary(NodeKind::kNe, std::move(node), parse_relational());
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
  MAKE_CALL_STACK(pinfo);

  auto node = parse_add();

  for (;;) {
    if (lexer_.try_consume("<")) {
      node = make_a_binary(NodeKind::kLt, std::move(node), parse_add());
    } else if (lexer_.try_consume("<=")) {
      node = make_a_binary(NodeKind::kLe, std::move(node), parse_add());
    } else if (lexer_.try_consume(">")) {
      node = make_a_binary(NodeKind::kLt, parse_add(), std::move(node));
    } else if (lexer_.try_consume(">=")) {
      node = make_a_binary(NodeKind::kLe, parse_add(), std::move(node));
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
  MAKE_CALL_STACK(pinfo);

  auto node = parse_mul();

  for (;;) {
    if (lexer_.try_consume("+")) {
      node = new_add(std::move(node), parse_mul());
    } else if (lexer_.try_consume("-")) {
      node = new_sub(std::move(node), parse_mul());
    } else {
      return node;
    }
  }

  /// Not Reachable.
  assert(false);
}

/// \brief mul ::= unary ("*" unary | "/" unary)*
///
/// \return
std::unique_ptr<Node> Parser::parse_mul() {
  MAKE_CALL_STACK(pinfo);

  auto node = parse_unary();

  for (;;) {
    if (lexer_.try_consume("*")) {
      node = make_a_binary(NodeKind::kMul, std::move(node), parse_unary());
    } else if (lexer_.try_consume("/")) {
      node = make_a_binary(NodeKind::kDiv, std::move(node), parse_unary());
    } else {
      return node;
    }
  }

  /// Not Reachable.
  assert(false);
}

/// \brief unary ::= ("+" | "-" | "*" | "&")? unary
///                | postfix
///
/// \return
std::unique_ptr<Node> Parser::parse_unary() {
  MAKE_CALL_STACK(pinfo);

  if (lexer_.try_consume("+")) {
    return parse_unary();
  }

  if (lexer_.try_consume("-")) {
    return make_a_binary(NodeKind::kSub, make_a_number(0), parse_unary());
  }

  if (lexer_.try_consume("&")) {
    return make_a_unary(NodeKind::kAddr, parse_unary());
  }

  if (lexer_.try_consume("*")) {
    return make_a_unary(NodeKind::kDeref, parse_unary());
  }

  return parse_postfix();
}

/// \brief postfix ::= primary ("[" expr "]" | "." ident)*
///
/// \return
std::unique_ptr<Node> Parser::parse_postfix() {
  MAKE_CALL_STACK(pinfo);

  Token token;
  auto node = parse_primary();

  while (true) {
    if (lexer_.try_consume("[")) {
      // x[y] is short for *(x+y)
      auto exp = new_add(std::move(node), parse_expr());
      lexer_.expect("]");
      node = make_a_unary(NodeKind::kDeref, std::move(exp));

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
  MAKE_CALL_STACK(pinfo);

  add_type(node.get());

  auto type = node->type;

  if (!type->is_struct()) {
    CHIBICPP_THROW_ERROR("Expect a `struct` got a ", node->type->id());
  }

  Token ident;
  lexer_.expect_identider(ident);
  auto name = ident.as_str();
  auto member = static_cast<StructType*>(type.get())->find_member(name);

  if (!member) {
    CHIBICPP_THROW_ERROR("No such member: ", name, " in ",
                         static_cast<StructType*>(type.get())->name());
  }

  node = make_a_unary(NodeKind::kMember, std::move(node));
  node->member = member;

  return node;
}

/// \brief declaration ::= basetype ident ("[" num "]")* ("=" expr) ";"
///
/// \return
std::unique_ptr<Node> Parser::parse_declaration() {
  MAKE_CALL_STACK(pinfo);

  Token ident;
  auto type = parse_basetype();
  lexer_.expect_identider(ident);
  type = parse_type_suffix(type);
  auto var = scope_.create_local_var(ident.as_str(), type);

  if (lexer_.try_consume(";")) {
    return std::make_unique<Node>(NodeKind::kEmpty);
  }

  lexer_.expect("=");
  auto lhs = make_a_var(var);
  auto rhs = parse_expr();
  lexer_.expect(";");

  auto node = make_a_binary(NodeKind::kAssign, std::move(lhs), std::move(rhs));

  return make_a_unary(NodeKind::kExprStmt, std::move(node));
}

/// primary ::= "(" "{" stmt-expr-tail
///           | "(" expr ")"
///           | "sizeof" unary
///           | ident func-args?
///           | str
///           | num
/// args    ::= "(" ")"
std::unique_ptr<Node> Parser::parse_primary() {
  MAKE_CALL_STACK(pinfo);

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
    add_type(node.get());

    return make_a_number(node->type->size_in_bytes());
  }

  if (lexer_.try_consume(TokenKind::kIdentifier, token)) {
    // Check function call.
    if (lexer_.try_consume("(")) {
      auto node = std::make_unique<Node>(NodeKind::kFunCall);
      node->func_name = token.as_str();
      node->args = parse_func_args();

      return node;
    }

    // Variable.
    auto const& ident = token.as_str();
    auto var = scope_.get_var(ident);

    if (!var) {
      CHIBICPP_THROW_ERROR(ident + " is not defined.");
    }

    return make_a_var(var);
  }

  // String literal.
  if (lexer_.try_consume(TokenKind::kStrLiteral, token)) {
    // Add an extra terminate '\0'.
    auto const& str = token.as_str();
    auto type = TypeMgr::get_array(str.size() + 1, TypeMgr::get_char());
    auto var = scope_.create_string_literal(str, type);

    return make_a_var(var);
  }

  lexer_.expect_number(token);

  return make_a_number(token.as_i64());
}

ObserverPtr<Type> Parser::parse_type_suffix(ObserverPtr<Type> base) {
  MAKE_CALL_STACK(pinfo);

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
  MAKE_CALL_STACK(pinfo);

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
  MAKE_CALL_STACK(pinfo);

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
  MAKE_CALL_STACK(pinfo);

  return make_a_unary(NodeKind::kExprStmt, parse_expr());
}

/// \brief func-param ::= basetype args*
///                     | empty
///
/// \return
ObserverPtr<Var> Parser::parse_func_param() {
  MAKE_CALL_STACK(pinfo);

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
  MAKE_CALL_STACK(pinfo);

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
  MAKE_CALL_STACK(pinfo);

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
  MAKE_CALL_STACK(pinfo);

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
  MAKE_CALL_STACK(pinfo);

  scope_.enter(Scope::kBlockScope);
  auto node = std::make_unique<Node>(NodeKind::kStmtExpr);
  node->body.push_back(parse_stmt());

  while (!lexer_.try_consume("}")) {
    node->body.push_back(parse_stmt());
  }

  lexer_.expect(")");
  scope_.leave();

  // Ensure the last statement in the body is an expression statement.
  if (node->body.back()->kind != NodeKind::kExprStmt) {
    CHIBICPP_THROW_ERROR(
        "Statement expression returning void is not supported.");
  }

  // The last node is an expression statement, but we need its expression as the
  // return value.
  auto& lhs = node->body.back()->lhs;
  node->body.back() = std::move(lhs);

  return node;
}

/// \brief struct-decl ::= "struct" "{" struct-member "}"
///
/// \param pinfo
/// \return
ObserverPtr<Type> Parser::parse_struct_decl() {
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
  Token ident;

  auto type = parse_basetype();
  lexer_.expect_identider(ident);
  type = parse_type_suffix(type);
  lexer_.expect(";");

  return std::make_unique<Member>(ident.as_str(), type);
}

}  // namespace chibicpp