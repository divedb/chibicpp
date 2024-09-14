#include "chibicpp/parser/parser.hh"

#include <cassert>

#include "chibicpp/ast/node_dump.hh"
#include "chibicpp/ast/type.hh"
#include "chibicpp/lex/tokenizer.hh"

namespace chibicpp {

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

/// \brief program ::= function*
///
/// \return
std::unique_ptr<Program> Parser::program() {
  std::vector<std::unique_ptr<Function>> prog;

  while (!lexer_.is_eof()) {
    locals_.clear();
    auto func = parse_function();
    prog.push_back(std::move(func));
  }

  return std::make_unique<Program>(std::move(prog));
}

Function::Prototype Parser::parse_function_prototype() {
  Token fname;

  Type* ret_type = parse_basetype();
  lexer_.expect_identider(fname);
  lexer_.expect("(");
  auto params = parse_func_params();

  return {ret_type, fname.as_str(), params};
}

std::vector<std::unique_ptr<Node>> Parser::parse_function_body() {
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
  auto proto = parse_function_prototype();
  auto body = parse_function_body();

  // Update the node's type information.
  for (auto& node : body) {
    add_type(node.get());
  }

  return std::make_unique<Function>(proto, std::move(body), std::move(locals_));
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
      node->init = read_expr_stmt();
      lexer_.expect(";");
    }

    if (!lexer_.try_consume(";")) {
      node->cond = parse_expr();
      lexer_.expect(";");
    }

    if (!lexer_.try_consume(")")) {
      node->inc = read_expr_stmt();
      lexer_.expect(")");
    }

    node->then = parse_stmt();

    return node;
  }

  if (lexer_.try_consume("{")) {
    std::vector<std::unique_ptr<Node>> body;

    while (!lexer_.try_consume("}")) {
      body.push_back(parse_stmt());
    }

    auto node = std::make_unique<Node>(NodeKind::kBlock);
    node->body = std::move(body);

    return node;
  }

  Token token;

  if (lexer_.try_peek("int", token)) {
    return parse_declaration();
  }

  auto node = read_expr_stmt();
  lexer_.expect(";");

  return node;
}

/// \brief expr ::= assign
///
/// \return
std::unique_ptr<Node> Parser::parse_expr() { return parse_assign(); }

/// \brief assign ::= equality ("=" assign)?
///
/// \return
std::unique_ptr<Node> Parser::parse_assign() {
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
///                | primary
///
/// \return
std::unique_ptr<Node> Parser::parse_unary() {
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

  return parse_primary();
}

/// \brief declaration ::= basetype ident ("[" num "]")* ("=" expr) ";"
///
/// \return
std::unique_ptr<Node> Parser::parse_declaration() {
  Token identifier;
  Type* type = parse_basetype();
  lexer_.expect_identider(identifier);
  type = parse_type_suffix(type);
  Var* var = get_or_create_var(identifier, type);

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

/// primary ::= "(" expr ")"
///           | ident func-args?
///           | num
/// args    ::= "(" ")"
std::unique_ptr<Node> Parser::parse_primary() {
  if (lexer_.try_consume("(")) {
    auto node = parse_expr();
    lexer_.expect(")");

    return node;
  }

  Token token;

  if (lexer_.try_consume_identifier(token)) {
    // Check function call.
    if (lexer_.try_consume("(")) {
      auto node = std::make_unique<Node>(NodeKind::kFunCall);
      node->func_name = token.as_str();
      node->args = parse_func_args();

      return node;
    }

    // Variable.
    Var* var = get_or_create_var(token);

    assert(var != nullptr);

    return make_a_var(var);
  }

  lexer_.expect_number(token);

  return make_a_number(token.as_i64());
}

Type* Parser::parse_type_suffix(Type* base) {
  if (!lexer_.try_consume("[")) {
    return base;
  }

  Token num;
  lexer_.expect_number(num);
  lexer_.expect("]");

  return TypeMgr::get_array(num.as_i64(), base);
}

/// \brief basetype ::= "int" "*"*
///
/// \return
Type* Parser::parse_basetype() {
  // Support `int` now.
  lexer_.expect("int");
  Type* type = TypeMgr::get_primitive(kInt);

  while (lexer_.try_consume("*")) {
    type = TypeMgr::get_pointer(type);
  }

  return type;
}

Var* Parser::get_or_create_var(Token const& token, Type* type) {
  auto const& name = token.as_str();

  auto iter =
      std::find_if(locals_.begin(), locals_.end(),
                   [&name](auto const& var) { return var->name == name; });

  if (iter != locals_.end()) {
    return iter->get();
  }

  auto var = std::make_unique<Var>(name, type, /*offset*/ 0);
  locals_.push_front(std::move(var));

  return locals_.front().get();
}

std::unique_ptr<Node> Parser::read_expr_stmt() {
  return make_a_unary(NodeKind::kExprStmt, parse_expr());
}

Var* Parser::parse_func_param() {
  Token token;
  Type* type = parse_basetype();
  lexer_.expect_identider(token);
  type = parse_type_suffix(type);

  return get_or_create_var(token, type);
}

/// \brief func-params ::= "(" args ")"
///        args        ::= empty
///                      | arg (,arg)*
///
/// @return
std::vector<Var*> Parser::parse_func_params() {
  if (lexer_.try_consume(")")) {
    return {};
  }

  std::vector<Var*> params;
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

}  // namespace chibicpp