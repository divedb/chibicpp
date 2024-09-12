#include <cassert>

#include "chibicpp/codegen/backend.hh"
#include "chibicpp/lex/tokenizer.hh"
#include "chibicpp/parser/parser.hh"
#include "chibicpp/util/stack_trace.hh"

static void print_stack_trace() {
  std::cout << chibicpp::stacktrace(true) << std::endl;
}

int main(int argc, char** argv) {
  using namespace chibicpp;

  if (argc != 2) {
    CHIBICPP_THROW_ERROR(argv[0], ": invalid number of arguments");
  }

  // std::set_terminate(print_stack_trace);

  // Parse program.
  Lexer lexer(argv[1], std::strlen(argv[1]));
  Parser parser(lexer);
  auto prog = parser.program();

  // for (auto iter = prog->func_begin(); iter != prog->func_end(); iter++) {
  //   (*iter)->dump_var_with_typeinfo(std::cout);
  // }

  // Update function local variable offset and generate code.
  Backend codegen;
  AstContext context;
  codegen.visit_program(prog.get(), context);

  return 0;
}