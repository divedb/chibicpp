#pragma once

#include <cxxabi.h>
#include <execinfo.h>

#include <string>

namespace chibicpp {

inline std::string stacktrace(bool demangle) {
  const int kMaxFrames = 200;

  std::string stack;
  void* frame[kMaxFrames];
  int nptrs = ::backtrace(frame, kMaxFrames);
  char** strings = ::backtrace_symbols(frame, nptrs);

  if (strings) {
    size_t len = 256;
    char* demangled = demangle ? static_cast<char*>(::malloc(len)) : nullptr;

    /// Skip the 0-th, which is this function.
    for (int i = 1; i < nptrs; i++) {
      if (demangle) {
        /// https://panthema.net/2008/0901-stacktrace-demangled/
        /// bin/exception_test(_ZN3Bar4testEv+0x79) [0x401909]
        char* left_par = nullptr;
        char* plus = nullptr;

        for (char* p = strings[i]; *p; ++p) {
          if (*p == '(') {
            left_par = p;
          } else if (*p == '+') {
            plus = p;
          }
        }

        if (left_par && plus) {
          *plus = '\0';
          int status = 0;
          char* ret =
              abi::__cxa_demangle(left_par + 1, demangled, &len, &status);
          *plus = '+';

          if (status == 0) {
            demangled = ret;
            stack.append(strings[i], left_par + 1);
            stack.append(demangled);
            stack.append(plus);
            stack.push_back('\n');
            continue;
          }
        }
      }

      stack.append(strings[i]);
      stack.push_back('\n');
    }

    free(demangled);
    free(strings);
  }

  return stack;
}

}  // namespace chibicpp