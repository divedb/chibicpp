#include "chibicpp/ast/node.hh"

#include "chibicpp/ast/type.hh"

namespace chibicpp {

bool Node::is_integer() const { return type->is_integer(); }
bool Node::has_base() const { return type->base() != nullptr; }

}  // namespace chibicpp