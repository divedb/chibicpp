#pragma once

#include <cxxabi.h>
#include <execinfo.h>

#include <iosfwd>
#include <string>
#include <vector>

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

class StackTrace {
 public:
  /// \brief Clear the stack trace.
  void clear() {
    depth_ = 0;
    frames_.clear();
  }

  /// \brief Dumps the stack trace to the specified output stream.
  ///
  /// \param os The output stream.
  void dump(std::ostream& os) const;

 private:
  friend class StackGuard;

  struct Frame {
    std::string fname;  ///< Function name.
    int lineno;         ///< Line number in source file.
    int depth;          ///< Function call depth.
  };

  /// \brief Enters a new frame in the stack trace.
  ///
  /// \param fname The name of the function being entered.
  /// \param lineno The line number in the source file where the function is
  ///               called.
  void enter(const std::string& fname, int lineno) {
    frames_.emplace_back(fname, lineno, depth_);
    ++depth_;
  }

  /// \brief Leaves the current frame in the stack trace.
  void leave() { --depth_; }

  int depth_{};
  std::vector<Frame> frames_;
};

class StackGuard {
 public:
  explicit StackGuard(StackTrace& st, const std::string& fname, int lineno)
      : st_{st} {
    st_.enter(fname, lineno);
  }

  ~StackGuard() { st_.leave(); }

 private:
  StackTrace& st_;
};

extern StackTrace stack_tracer;

#define TOKENPASTE(x, y) x##y
#define TOKENPASTE2(x, y) TOKENPASTE(x, y)

#define STACK_GUARD()                                                     \
  StackGuard TOKENPASTE2(guard_, __COUNTER__)(stack_tracer, __FUNCTION__, \
                                              __LINE__)

}  // namespace chibicpp
