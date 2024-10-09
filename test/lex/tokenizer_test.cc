#include "chibicpp/lex/tokenizer.hh"

#include <gtest/gtest.h>

using namespace chibicpp;

TEST(NEXT, GlobalVar) {
  auto input = R"(
    char* a;
    int b;
    int c;
  )";

  std::string str = input;
  BasicTokenizer tokenizer{str.c_str(), str.size()};
  std::vector<Token> tokens{
      Token{TokenKind::kReserved, {}, "char"},
      Token{TokenKind::kReserved, {}, "*"},
      Token{TokenKind::kIdentifier, {}, "a"},
      Token{TokenKind::kReserved, {}, ";"},
      Token{TokenKind::kReserved, {}, "int"},
      Token{TokenKind::kIdentifier, {}, "b"},
      Token{TokenKind::kReserved, {}, ";"},
      Token{TokenKind::kReserved, {}, "int"},
      Token{TokenKind::kIdentifier, {}, "c"},
      Token{TokenKind::kReserved, {}, ";"},
  };

  for (const auto& tok : tokens) {
    EXPECT_TRUE(tokenizer.next().equals(tok)) << tok;
  }

  auto n = tokenizer.next();

  EXPECT_TRUE(n.equals(Token::dummy())) << n;
}

TEST(Next, LocalVar) {
  auto input = R"(
    int foo() {
      return 42;
    }

    int main() {
      int a = 42;
      int b = 101;
      int z = a + b;

      char* a1 = "hello";
      short c;
      char d;

      return 0;
    }
  )";

  std::string str = input;
  BasicTokenizer tokenizer{str.c_str(), str.size()};

  std::vector<Token> tokens{
      Token{TokenKind::kReserved, {}, "int"},
      Token{TokenKind::kIdentifier, {}, "foo"},
      Token{TokenKind::kReserved, {}, "("},
      Token{TokenKind::kReserved, {}, ")"},
      Token{TokenKind::kReserved, {}, "{"},
      Token{TokenKind::kReserved, {}, "return"},
      Token{TokenKind::kNum, {}, 42L},
      Token{TokenKind::kReserved, {}, ";"},
      Token{TokenKind::kReserved, {}, "}"},
      Token{TokenKind::kReserved, {}, "int"},
      Token{TokenKind::kIdentifier, {}, "main"},
      Token{TokenKind::kReserved, {}, "("},
      Token{TokenKind::kReserved, {}, ")"},
      Token{TokenKind::kReserved, {}, "{"},
      Token{TokenKind::kReserved, {}, "int"},
      Token{TokenKind::kIdentifier, {}, "a"},
      Token{TokenKind::kReserved, {}, "="},
      Token{TokenKind::kNum, {}, 42L},
      Token{TokenKind::kReserved, {}, ";"},
      Token{TokenKind::kReserved, {}, "int"},
      Token{TokenKind::kIdentifier, {}, "b"},
      Token{TokenKind::kReserved, {}, "="},
      Token{TokenKind::kNum, {}, 101L},
      Token{TokenKind::kReserved, {}, ";"},
      Token{TokenKind::kReserved, {}, "int"},
      Token{TokenKind::kIdentifier, {}, "z"},
      Token{TokenKind::kReserved, {}, "="},
      Token{TokenKind::kIdentifier, {}, "a"},
      Token{TokenKind::kReserved, {}, "+"},
      Token{TokenKind::kIdentifier, {}, "b"},
      Token{TokenKind::kReserved, {}, ";"},
      Token{TokenKind::kReserved, {}, "char"},
      Token{TokenKind::kReserved, {}, "*"},
      Token{TokenKind::kIdentifier, {}, "a1"},
      Token{TokenKind::kReserved, {}, "="},
      Token{TokenKind::kStrLiteral, {}, "hello"},
      Token{TokenKind::kReserved, {}, ";"},
      Token{TokenKind::kReserved, {}, "short"},
      Token{TokenKind::kIdentifier, {}, "c"},
      Token{TokenKind::kReserved, {}, ";"},
      Token{TokenKind::kReserved, {}, "char"},
      Token{TokenKind::kIdentifier, {}, "d"},
      Token{TokenKind::kReserved, {}, ";"},
      Token{TokenKind::kReserved, {}, "return"},
      Token{TokenKind::kNum, {}, 0L},
      Token{TokenKind::kReserved, {}, ";"},
      Token{TokenKind::kReserved, {}, "}"},
  };

  for (const auto& tok : tokens) {
    EXPECT_TRUE(tokenizer.next().equals(tok)) << tok;
  }

  auto n = tokenizer.next();

  EXPECT_TRUE(n.equals(Token::dummy())) << n;
}

int main(int argc, char** argv) {
  ::testing::InitGoogleTest(&argc, argv);

  return RUN_ALL_TESTS();
}