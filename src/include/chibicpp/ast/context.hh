#pragma once

#include "chibicpp/support/observer_ptr.hh"

namespace chibicpp {

class Function;

class AstContext {
 public:
  ObserverPtr<Function> func;  ///< Inside a function.
};

}  // namespace chibicpp