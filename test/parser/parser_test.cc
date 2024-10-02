#include "chibicpp/parser/parser.hh"

#include <gtest/gtest.h>

#include <map>
#include <string>

#include "chibicpp/ast/ast_context.hh"
#include "chibicpp/ast/visitor.hh"
#include "chibicpp/common/macro.hh"
#include "chibicpp/lex/tokenizer.hh"

using namespace std;
using namespace chibicpp;

TEST(Function, LocalVarCount) {
  auto input = "int main() { int a; int z; a=3; z=5; return a+z; }";
  Lexer lexer{input, std::strlen(input)};
  Parser parser{lexer};

  Counter counter;
  AstContext context;
  auto prog = parser.parse_program();

  counter.visit_program(prog.get(), context);

  EXPECT_EQ(0, counter.global_var_count());
  EXPECT_EQ(2, counter.local_var_count("main"));
}

int main(int argc, char** argv) {
  ::testing::InitGoogleTest(&argc, argv);

  return RUN_ALL_TESTS();
}