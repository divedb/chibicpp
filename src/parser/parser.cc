#include "chibicpp/parser/parser.hh"

#include <cassert>

#include "chibicpp/lex/tokenizer.hh"

namespace chibicpp {

std::unique_ptr<Function> Parser::program() {
  std::vector<std::unique_ptr<Node>> nodes;

  while (!lexer_.is_eof()) {
    auto node = parse_stmt();
    nodes.push_back(std::move(node));
  }

  return std::make_unique<Function>(std::move(nodes), std::move(locals_));
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
      node = make_a_binary(NodeKind::kAdd, std::move(node), parse_mul());
    } else if (lexer_.try_consume("-")) {
      node = make_a_binary(NodeKind::kSub, std::move(node), parse_mul());
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

/// \brief unary ::= ("+" | "-")? unary
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

  return parse_primary();
}

std::unique_ptr<Node> Parser::parse_primary() {
  if (lexer_.try_consume("(")) {
    auto node = parse_expr();
    lexer_.expect(")");

    return node;
  }

  Token token;

  if (lexer_.try_consume_identifier(token)) {
    Var* var = find_var(token);

    /// If this variable NOT exists.
    /// TODO(gc): But why it doesn't exist?
    if (var == nullptr) {
      auto owner = std::make_unique<Var>(token.as_str(), 0);
      var = owner.get();
      locals_.push_back(std::move(owner));
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

}  // namespace chibicpp