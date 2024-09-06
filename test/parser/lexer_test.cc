#include <cstdlib>
#include <fstream>

#include "chibicpp/lex/tokenizer.hh"

using namespace std;
using namespace chibicpp;

static void usage(char const* prog_name) {
  cout << "usage: " << prog_name << " input" << endl;

  exit(1);
}

int main(int argc, char** argv) {
  if (argc < 2) {
    usage(argv[0]);

    return 1;
  }

  Lexer lexer(argv[1], strlen(argv[1]));

  for (auto token : lexer) {
    std::cout << token << std::endl;
  }

  return 0;
}