#include <cassert>
#include <cstdlib>
#include <cstring>
#include <fstream>

#include "chibicpp/codegen/backend.hh"
#include "chibicpp/lex/tokenizer.hh"
#include "chibicpp/parser/parser.hh"
#include "chibicpp/util/stack_trace.hh"

static void print_stack_trace() {
  std::cout << chibicpp::stacktrace(true) << std::endl;
}

// static std::string read_all(char const* filename) {
//   std::ifstream ifs(filename, std::ios::binary | std::ios::ate);

//   if (!ifs.is_open()) {
//     std::cerr << "Fail to open: " << filename << std::endl;

//     exit(1);
//   }

//   std::streamsize size = ifs.tellg();
//   ifs.seekg(0, std::ios::beg);
//   std::string buffer(size, 0);

//   if (!ifs.read(&buffer[0], size)) {
//     std::cerr << "Error reading file: " << filename << std::endl;

//     exit(1);
//   }

//   ifs.close();

//   return buffer;
// }

// Returns the contents of a given file.
static std::string read_file(char* path) {
  // Open and read the file.
  FILE* fp = fopen(path, "r");
  if (!fp) fprintf(stderr, "cannot open %s: %s", path, strerror(errno));

  int filemax = 10 * 1024 * 1024;
  char* buf = static_cast<char*>(malloc(filemax));
  size_t size = fread(buf, 1, filemax - 2, fp);
  if (!feof(fp)) fprintf(stderr, "%s: file too large", path);

  // Make sure that the string ends with "\n\0".
  // if (size == 0 || buf[size - 1] != '\n') buf[size++] = '\n';
  // buf[size] = '\0';
  return std::string{buf, size};
}

int main(int argc, char** argv) {
  using namespace chibicpp;

  if (argc != 2) {
    CHIBICPP_THROW_ERROR(argv[0], ": invalid number of arguments");
  }

  // std::set_terminate(print_stack_trace);

  // Parse program.
  std::string buf = read_file(argv[1]);
  Lexer lexer(buf.c_str(), buf.size());
  Parser parser(lexer);
  auto prog = parser.parse_program();

  // Update function local variable offset and generate code.
  AstContext context;
  Backend codegen(std::cout);
  codegen.visit_program(prog.get(), context);

  return 0;
}