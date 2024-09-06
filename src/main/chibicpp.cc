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
  auto func = parser.program();

  assert(func != nullptr);

  /// Update function local variable offset and generate code.
  func->update_offset();
  Backend codegen;
  codegen.visit_function(func.get());

  return 0;
}