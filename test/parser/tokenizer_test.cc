#include "chibicpp/lex/tokenizer.hh"

#include <cstdlib>
#include <fstream>

using namespace std;
using namespace chibicpp;

static void usage(char const* prog_name) {
  cout << "usage: " << prog_name << " filename" << endl;

  exit(1);
}

static string read_all(char const* filename) {
  ifstream ifs(filename, std::ios::binary | std::ios::ate);

  if (!ifs.is_open()) {
    cerr << "Fail to open: " << filename << endl;

    exit(1);
  }

  std::streamsize size = ifs.tellg();
  ifs.seekg(0, std::ios::beg);
  std::string buffer(size, 0);

  if (!ifs.read(&buffer[0], size)) {
    cerr << "Error reading file: " << filename << endl;

    exit(1);
  }

  ifs.close();

  return buffer;
}

int main(int argc, char** argv) {
  if (argc < 2) {
    usage(argv[0]);

    return 1;
  }

  char const* filename = argv[1];
  string data = read_all(filename);
  Tokenizer stream(data.c_str(), data.size());

  while (true) {
    Token token = stream.next();
    cout << token << endl;

    if (token.kind() == TokenKind::kEOF) {
      break;
    }
  }
}