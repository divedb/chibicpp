#include "chibicpp/parser/parser.hh"

#include <gtest/gtest.h>

#include <map>
#include <string>

#include "chibicpp/ast/node.hh"
#include "chibicpp/lex/tokenizer.hh"

using namespace std;
using namespace chibicpp;

class ParserFixture : public ::testing::Test {
 protected:
  std::unique_ptr<Lexer> lexer;
  std::unique_ptr<Parser> parser;

  void set_input(const char* input) {
    lexer = std::make_unique<Lexer>(input, strlen(input));
    parser = std::make_unique<Parser>(*lexer);
  }
};

TEST_F(ParserFixture, EmptyWhileStmt) {
  set_input("while (1);");

  auto node = parser->parse_while_stmt();

  EXPECT_NE(nullptr, node);
  EXPECT_EQ(Node::kWhile, node->id());
  EXPECT_EQ(Node::kEmpty, node->then()->id());

  set_input("while (1) {}");

  node = parser->parse_while_stmt();

  EXPECT_NE(nullptr, node);
  EXPECT_EQ(Node::kWhile, node->id());
  EXPECT_EQ(Node::kBlock, node->then()->id());
  EXPECT_EQ(0, node->then()->block_size());
}

TEST_F(ParserFixture, NonEmptyWhileStmt) {
  set_input("while (1) { int a = 42; }");

  auto node = parser->parse_while_stmt();

  EXPECT_NE(nullptr, node);
  EXPECT_EQ(Node::kWhile, node->id());
  EXPECT_EQ(Node::kBlock, node->then()->id());
  EXPECT_EQ(1, node->then()->block_size());

  auto stmt = node->then()->block_begin();

  EXPECT_EQ(Node::kExprStmt, stmt->get()->id());
  EXPECT_EQ(Node::kAssign, stmt->get()->lhs()->id());

  auto assign = stmt->get()->lhs();

  // a = 42
  EXPECT_EQ(Node::kVar, assign->lhs()->id());
  EXPECT_EQ(Node::kNum, assign->rhs()->id());
  EXPECT_EQ("a", assign->lhs()->var()->name());
  EXPECT_EQ(42, assign->rhs()->number());
}

int main(int argc, char** argv) {
  ::testing::InitGoogleTest(&argc, argv);

  return RUN_ALL_TESTS();
}