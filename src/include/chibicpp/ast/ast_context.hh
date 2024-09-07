#pragma once

namespace chibicpp {

class Function;

class AstContext {
 public:
  Function* func;  ///< Inside a function.
  /// Scope
};

}  // namespace chibicpp