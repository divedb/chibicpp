#pragma once

#include "chibicpp/util/observer_ptr.hh"

namespace chibicpp {

class Function;

class AstContext {
 public:
  ObserverPtr<Function> func;  ///< Inside a function.
  /// Scope
};

}  // namespace chibicpp