#include "chibicpp/parser/parser.hh"

#include <cassert>

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
    return make_a_binary(NodeKind::kPtrAdd, std::move(lhs), std::move(rhs));
  }

  // TODO(gc): Add more error information.
  CHIBICPP_THROW_ERROR("Add: invalid operands");
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
}

/// \brief program ::= function*
///
/// \return
std::unique_ptr<Program> Parser::program() {
  std::vector<std::unique_ptr<Function>> prog;

  while (!lexer_.is_eof()) {
    auto func = parse_function();
    prog.push_back(std::move(func));
  }

  return std::make_unique<Program>(std::move(prog));
}

/// \brief function ::= ident "(" params? ")" "{" stmt* "}"
///        params   ::= ident ("," indent)*
///
/// @return
std::unique_ptr<Function> Parser::parse_function() {
  Token func_name;
  lexer_.expect_identider(func_name).expect("(");
  auto params = parse_func_params();
  lexer_.expect("{");

  locals_.clear();
  std::vector<std::unique_ptr<Node>> nodes;

  while (!lexer_.try_consume("}")) {
    auto node = parse_stmt();
    nodes.push_back(std::move(node));
  }

  // Update the node's type information.
  for (auto& node : nodes) {
    add_type(node.get());
  }

  return std::make_unique<Function>(func_name.as_str(), std::move(nodes),
                                    std::move(params), std::move(locals_));
}

/// \brief stmt ::= "return" expr ";"
///               | "if" "(" expr ")" stmt ("else" stmt)?
///               | "while" "(" expr ")" stmt
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
    /// Check function call.
    if (lexer_.try_consume("(")) {
      auto node = std::make_unique<Node>(NodeKind::kFunCall);
      node->func_name = token.as_str();
      node->args = parse_func_args();

      return node;
    }

    /// Variable.
    Var* var = find_var(token);

    /// If this variable NOT exists.
    /// TODO(gc): But why it doesn't exist?
    if (var == nullptr) {
      auto owner = std::make_unique<Var>(token.as_str(), 0);
      var = owner.get();
      locals_.push_front(std::move(owner));
    }

    return make_a_var(var);
  }

  lexer_.expect_number(token);

  return make_a_number(token.as_i64());
}

Var* Parser::find_var(Token const& token) const {
  auto iter = std::find_if(
      locals_.begin(), locals_.end(),
      [&token](auto const& var) { return var->name == token.as_str(); });

  if (iter == locals_.end()) {
    return nullptr;
  }

  return (*iter).get();
}

std::unique_ptr<Node> Parser::read_expr_stmt() {
  return make_a_unary(NodeKind::kExprStmt, parse_expr());
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

  Token token;
  std::vector<Var*> params;

  lexer_.expect_identider(token);
  locals_.push_front(std::make_unique<Var>(token.as_str(), 0));
  params.push_back(locals_.front().get());

  while (!lexer_.try_consume(")")) {
    lexer_.expect(",");
    lexer_.expect_identider(token);
    locals_.push_front(std::make_unique<Var>(token.as_str(), 0));
    params.push_back(locals_.front().get());
  }

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