#include "chibicpp/lex/tokenizer.hh"

#include <cstdlib>
#include <fstream>

using namespace std;
using namespace chibicpp;

static void usage(char const* prog_name) {
  cout << "usage: " << prog_name << " filename" << endl;

  exit(1);
}

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
  if (argc < 2) {
    usage(argv[0]);

    return 1;
  }

  char* filename = argv[1];
  string data = read_file(filename);
  Tokenizer stream(data.c_str(), data.size());

  while (true) {
    Token token = stream.next();
    cout << token << endl;

    if (token.kind() == TokenKind::kEOF) {
      break;
    }
  }
}