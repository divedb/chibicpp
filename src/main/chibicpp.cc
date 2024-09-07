#include <cassert>

#include "chibicpp/codegen/backend.hh"
#include "chibicpp/lex/tokenizer.hh"
#include "chibicpp/parser/parser.hh"

int main(int argc, char** argv) {
  using namespace chibicpp;

  if (argc != 2) {
    CHIBICPP_THROW_ERROR(argv[0], ": invalid number of arguments");
  }

  /// Parse program.
  Lexer lexer(argv[1], std::strlen(argv[1]));
  Parser parser(lexer);
  auto prog = parser.program();

  /// Update function local variable offset and generate code.
  Backend codegen;
  AstContext context;
  codegen.visit_program(prog.get(), context);

  return 0;
}