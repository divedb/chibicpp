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

  // TODO(gc): what's scope for program?
  scope_ = std::make_unique<Scope>(0, 0);
  std::vector<std::unique_ptr<Function>> prog;

  while (!lexer_.is_eof()) {
    if (is_function(pinfo)) {
      auto func = parse_function(pinfo);
      prog.push_back(std::move(func));
    } else {
      parse_global_var(pinfo);
    }
  }

  return std::make_unique<Program>(std::move(globals_), std::move(prog));
}

Function::Prototype Parser::parse_function_prototype(ParserInfo& pinfo) {
  MAKE_CALL_STACK(pinfo);

  Token fname;
  Type* ret_type = parse_basetype(pinfo);
  lexer_.expect_identider(fname);
  lexer_.expect("(");
  auto params = parse_func_params(pinfo);

  return {ret_type, fname.as_str(), params};
}

std::vector<std::unique_ptr<Node>> Parser::parse_function_body(
    ParserInfo& pinfo) {
  MAKE_CALL_STACK(pinfo);

  std::vector<std::unique_ptr<Node>> body;

  lexer_.expect("{");
  while (!lexer_.try_consume("}")) {
    MAKE_CALL_STACK(pinfo);
    auto node = parse_stmt(pinfo);
    body.push_back(std::move(node));
  }

  return body;
}

/// \brief function ::= basetype ident "(" params? ")" "{" stmt* "}"
///        params   ::= param ("," param)*
///        param    ::= basetype ident
///
/// @return
std::unique_ptr<Function> Parser::parse_function(ParserInfo& pinfo) {
  MAKE_CALL_STACK(pinfo);
  locals_.clear();

  // Save the current scope and create a new scope with incremented depth.
  auto saved_scope = std::move(scope_);
  scope_ = std::make_unique<Scope>(saved_scope->depth() + 1, Scope::kFnScope,
                                   saved_scope.get());

  auto proto = parse_function_prototype(pinfo);
  auto body = parse_function_body(pinfo);

  scope_ = std::move(saved_scope);

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
std::unique_ptr<Node> Parser::parse_stmt(ParserInfo& pinfo) {
  MAKE_CALL_STACK(pinfo);

  if (lexer_.try_consume("return")) {
    auto node = make_a_unary(NodeKind::kReturn, parse_expr(pinfo));
    lexer_.expect(";");

    return node;
  }

  if (lexer_.try_consume("if")) {
    auto node = std::make_unique<Node>(NodeKind::kIf);
    lexer_.expect("(");
    node->cond = parse_expr(pinfo);
    lexer_.expect(")");
    node->then = parse_stmt(pinfo);

    if (lexer_.try_consume("else")) {
      node->els = parse_stmt(pinfo);
    }

    return node;
  }

  if (lexer_.try_consume("while")) {
    auto node = std::make_unique<Node>(NodeKind::kWhile);
    lexer_.expect("(");
    node->cond = parse_expr(pinfo);
    lexer_.expect(")");
    node->then = parse_stmt(pinfo);

    return node;
  }

  if (lexer_.try_consume("for")) {
    auto node = std::make_unique<Node>(NodeKind::kFor);
    lexer_.expect("(");

    if (!lexer_.try_consume(";")) {
      node->init = parse_expr_stmt(pinfo);
      lexer_.expect(";");
    }

    if (!lexer_.try_consume(";")) {
      node->cond = parse_expr(pinfo);
      lexer_.expect(";");
    }

    if (!lexer_.try_consume(")")) {
      node->inc = parse_expr_stmt(pinfo);
      lexer_.expect(")");
    }

    node->then = parse_stmt(pinfo);

    return node;
  }

  if (lexer_.try_consume("{")) {
    auto saved_scope = std::move(scope_);
    scope_ = std::make_unique<Scope>(saved_scope->depth() + 1,
                                     Scope::kBlockScope, saved_scope.get());
    std::vector<std::unique_ptr<Node>> body;

    while (!lexer_.try_consume("}")) {
      body.push_back(parse_stmt(pinfo));
    }

    scope_ = std::move(saved_scope);

    auto node = std::make_unique<Node>(NodeKind::kBlock);
    node->body = std::move(body);

    return node;
  }

  if (is_typename()) {
    return parse_declaration(pinfo);
  }

  auto node = parse_expr_stmt(pinfo);
  lexer_.expect(";");

  return node;
}

/// \brief expr ::= assign
///
/// \return
std::unique_ptr<Node> Parser::parse_expr(ParserInfo& pinfo) {
  MAKE_CALL_STACK(pinfo);

  return parse_assign(pinfo);
}

/// \brief assign ::= equality ("=" assign)?
///
/// \return
std::unique_ptr<Node> Parser::parse_assign(ParserInfo& pinfo) {
  MAKE_CALL_STACK(pinfo);

  auto node = parse_equality(pinfo);

  if (lexer_.try_consume("=")) {
    node =
        make_a_binary(NodeKind::kAssign, std::move(node), parse_assign(pinfo));
  }

  return node;
}

/// \brief equality ::= relational ("==" relational | "!=" relational)*
///
/// \return
std::unique_ptr<Node> Parser::parse_equality(ParserInfo& pinfo) {
  MAKE_CALL_STACK(pinfo);

  auto node = parse_relational(pinfo);

  for (;;) {
    if (lexer_.try_consume("==")) {
      node = make_a_binary(NodeKind::kEq, std::move(node),
                           parse_relational(pinfo));
    } else if (lexer_.try_consume("!=")) {
      node = make_a_binary(NodeKind::kNe, std::move(node),
                           parse_relational(pinfo));
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
std::unique_ptr<Node> Parser::parse_relational(ParserInfo& pinfo) {
  MAKE_CALL_STACK(pinfo);

  auto node = parse_add(pinfo);

  for (;;) {
    if (lexer_.try_consume("<")) {
      node = make_a_binary(NodeKind::kLt, std::move(node), parse_add(pinfo));
    } else if (lexer_.try_consume("<=")) {
      node = make_a_binary(NodeKind::kLe, std::move(node), parse_add(pinfo));
    } else if (lexer_.try_consume(">")) {
      node = make_a_binary(NodeKind::kLt, parse_add(pinfo), std::move(node));
    } else if (lexer_.try_consume(">=")) {
      node = make_a_binary(NodeKind::kLe, parse_add(pinfo), std::move(node));
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
std::unique_ptr<Node> Parser::parse_add(ParserInfo& pinfo) {
  MAKE_CALL_STACK(pinfo);

  auto node = parse_mul(pinfo);

  for (;;) {
    if (lexer_.try_consume("+")) {
      node = new_add(std::move(node), parse_mul(pinfo));
    } else if (lexer_.try_consume("-")) {
      node = new_sub(std::move(node), parse_mul(pinfo));
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
std::unique_ptr<Node> Parser::parse_mul(ParserInfo& pinfo) {
  MAKE_CALL_STACK(pinfo);

  auto node = parse_unary(pinfo);

  for (;;) {
    if (lexer_.try_consume("*")) {
      node = make_a_binary(NodeKind::kMul, std::move(node), parse_unary(pinfo));
    } else if (lexer_.try_consume("/")) {
      node = make_a_binary(NodeKind::kDiv, std::move(node), parse_unary(pinfo));
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
std::unique_ptr<Node> Parser::parse_unary(ParserInfo& pinfo) {
  MAKE_CALL_STACK(pinfo);

  if (lexer_.try_consume("+")) {
    return parse_unary(pinfo);
  }

  if (lexer_.try_consume("-")) {
    return make_a_binary(NodeKind::kSub, make_a_number(0), parse_unary(pinfo));
  }

  if (lexer_.try_consume("&")) {
    return make_a_unary(NodeKind::kAddr, parse_unary(pinfo));
  }

  if (lexer_.try_consume("*")) {
    return make_a_unary(NodeKind::kDeref, parse_unary(pinfo));
  }

  return parse_postfix(pinfo);
}

/// \brief postfix ::= primary ("[" expr "]")*
///
/// \return
std::unique_ptr<Node> Parser::parse_postfix(ParserInfo& pinfo) {
  MAKE_CALL_STACK(pinfo);

  Token token;
  auto node = parse_primary(pinfo);

  while (lexer_.try_consume("[")) {
    // x[y] is short for *(x+y)
    auto exp = new_add(std::move(node), parse_expr(pinfo));
    lexer_.expect("]");
    node = make_a_unary(NodeKind::kDeref, std::move(exp));
  }

  return node;
}

/// \brief declaration ::= basetype ident ("[" num "]")* ("=" expr) ";"
///
/// \return
std::unique_ptr<Node> Parser::parse_declaration(ParserInfo& pinfo) {
  MAKE_CALL_STACK(pinfo);

  Token ident;
  Type* type = parse_basetype(pinfo);
  lexer_.expect_identider(ident);
  type = parse_type_suffix(type, pinfo);
  Var* var = create_local_var(ident.as_str(), type);

  if (lexer_.try_consume(";")) {
    return std::make_unique<Node>(NodeKind::kEmpty);
  }

  lexer_.expect("=");
  auto lhs = make_a_var(var);
  auto rhs = parse_expr(pinfo);
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
std::unique_ptr<Node> Parser::parse_primary(ParserInfo& pinfo) {
  MAKE_CALL_STACK(pinfo);

  if (lexer_.try_consume("(")) {
    if (lexer_.try_consume("{")) {
      return parse_stmt_expr(pinfo);
    }

    auto node = parse_expr(pinfo);
    lexer_.expect(")");

    return node;
  }

  Token token;

  if (lexer_.try_consume("sizeof")) {
    auto node = parse_unary(pinfo);
    add_type(node.get());

    return make_a_number(node->type->size_in_bytes());
  }

  if (lexer_.try_consume(TokenKind::kIdentifier, token)) {
    // Check function call.
    if (lexer_.try_consume("(")) {
      auto node = std::make_unique<Node>(NodeKind::kFunCall);
      node->func_name = token.as_str();
      node->args = parse_func_args(pinfo);

      return node;
    }

    // Variable.
    auto const& ident = token.as_str();
    Var* var = get_var(ident);

    if (var == nullptr) {
      CHIBICPP_THROW_ERROR(ident + " is not defined.");
    }

    return make_a_var(var);
  }

  // String literal.
  if (lexer_.try_consume(TokenKind::kStrLiteral, token)) {
    // Add an extra terminate '\0'.
    auto const& str = token.as_str();
    auto type =
        TypeMgr::get_array(str.size() + 1, TypeMgr::get_primitive(kChar));
    auto var = create_global_var(new_global_label(), str, type);

    return make_a_var(var);
  }

  lexer_.expect_number(token);

  return make_a_number(token.as_i64());
}

Type* Parser::parse_type_suffix(Type* base, ParserInfo& pinfo) {
  MAKE_CALL_STACK(pinfo);

  if (!lexer_.try_consume("[")) {
    return base;
  }

  Token num;
  lexer_.expect_number(num);
  lexer_.expect("]");
  base = parse_type_suffix(base, pinfo);

  return TypeMgr::get_array(num.as_i64(), base);
}

/// \brief basetype ::= ("char" | "int") "*"*
///
/// \return
Type* Parser::parse_basetype(ParserInfo& pinfo) {
  MAKE_CALL_STACK(pinfo);

  Type* type;

  if (lexer_.try_consume("char")) {
    type = TypeMgr::get_primitive(kChar);
  } else {
    lexer_.expect("int");
    type = TypeMgr::get_primitive(kInt);
  }

  while (lexer_.try_consume("*")) {
    type = TypeMgr::get_pointer(type);
  }

  return type;
}

inline Var* Parser::create_local_var(std::string const& ident, Type* type) {
  return create_var_impl(locals_, ident, type, /*offset*/ 0);
}

inline Var* Parser::create_global_var(std::string const& ident, Type* type) {
  return create_var_impl(globals_, ident, type);
}

inline Var* Parser::create_global_var(std::string const& ident,
                                      std::string const& content, Type* type) {
  return create_var_impl(globals_, ident, content, type);
}

inline std::string Parser::new_global_label() {
  return ".L.data." + std::to_string(global_label_++);
}

Var* Parser::get_var(std::string const& ident) {
  return scope_->search_var(ident);
}

bool Parser::is_function(ParserInfo& pinfo) {
  MAKE_CALL_STACK(pinfo);

  lexer_.mark();

  (void)parse_basetype(pinfo);
  bool is_func =
      lexer_.try_consume(TokenKind::kIdentifier, Token::dummy_eof()) &&
      lexer_.try_consume("(");

  lexer_.reset();

  return is_func;
}

bool Parser::is_typename() {
  static const std::vector<char const*> kTypenames{"char", "int"};

  return std::find_if(kTypenames.begin(), kTypenames.end(), [this](auto key) {
           return lexer_.try_peek(key, Token::dummy_eof());
         }) != kTypenames.end();
}

std::unique_ptr<Node> Parser::parse_expr_stmt(ParserInfo& pinfo) {
  MAKE_CALL_STACK(pinfo);

  return make_a_unary(NodeKind::kExprStmt, parse_expr(pinfo));
}

Var* Parser::parse_func_param(ParserInfo& pinfo) {
  MAKE_CALL_STACK(pinfo);

  Token ident;
  Type* type = parse_basetype(pinfo);
  lexer_.expect_identider(ident);
  type = parse_type_suffix(type, pinfo);

  return create_local_var(ident.as_str(), type);
}

/// \brief func-params ::= "(" args ")"
///        args        ::= empty
///                      | arg (,arg)*
///
/// @return
std::vector<Var*> Parser::parse_func_params(ParserInfo& pinfo) {
  MAKE_CALL_STACK(pinfo);

  if (lexer_.try_consume(")")) {
    return {};
  }

  std::vector<Var*> params;
  params.push_back(parse_func_param(pinfo));

  while (!lexer_.try_consume(")")) {
    lexer_.expect(",");
    params.push_back(parse_func_param(pinfo));
  }

  // for (auto p : params) {
  //   std::cout << std::quoted(p->name) << ':' << p->type->to_string()
  //             << std::endl;
  // }

  return params;
}

/// \brief func-args ::= "(" (assign ("," assign)*)? ")"
/// \return
std::vector<std::unique_ptr<Node>> Parser::parse_func_args(ParserInfo& pinfo) {
  MAKE_CALL_STACK(pinfo);

  if (lexer_.try_consume(")")) {
    return {};
  }

  std::vector<std::unique_ptr<Node>> args;
  args.push_back(parse_assign(pinfo));

  while (lexer_.try_consume(",")) {
    args.push_back(parse_assign(pinfo));
  }

  lexer_.expect(")");

  return args;
}

/// \brief global-var ::= basetype ident "("[" num "]")*" ";"
void Parser::parse_global_var(ParserInfo& pinfo) {
  MAKE_CALL_STACK(pinfo);

  Token ident;
  auto type = parse_basetype(pinfo);
  lexer_.expect_identider(ident);
  type = parse_type_suffix(type, pinfo);
  lexer_.expect(";");
  create_global_var(ident.as_str(), type);
}

/// \brief stmt-expr ::= "(" "{" stmt stmt* "}" ")"
///
/// Statement expression is a GNU C extension.
///
/// \return
std::unique_ptr<Node> Parser::parse_stmt_expr(ParserInfo& pinfo) {
  MAKE_CALL_STACK(pinfo);

  auto saved_scope = std::move(scope_);
  scope_ = std::make_unique<Scope>(saved_scope->depth() + 1, Scope::kBlockScope,
                                   saved_scope.get());

  auto node = std::make_unique<Node>(NodeKind::kStmtExpr);
  node->body.push_back(parse_stmt(pinfo));

  while (!lexer_.try_consume("}")) {
    node->body.push_back(parse_stmt(pinfo));
  }

  lexer_.expect(")");
  scope_ = std::move(saved_scope);

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

}  // namespace chibicpp