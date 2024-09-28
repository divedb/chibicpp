#include "chibicpp/util/stack_trace.hh"

#include <iostream>

#include "chibicpp/common/error.hh"

namespace chibicpp {

std::string error;
StackTrace stack_tracer;

namespace {

std::ostream& ident(std::ostream& os, int depth) {
  return os << std::string(depth, ' ');
}

}  // namespace

void StackTrace::dump(std::ostream& os) const {
  for (auto const& frame : frames_) {
    ident(os, frame.depth);
    os << '[' << frame.fname << "]:" << frame.lineno << '\n';
  }

  os << error << std::endl;
}

}  // namespace chibicpp
