#include "chibicpp/parser/scope.hh"

#include "chibicpp/ast/node.hh"

namespace chibicpp {

ObserverPtr<Var> Scope::search_var(const std::string& name) const {
  // Variables are added to the scope in sequential order.
  // To find the most recently added variable with the given name,
  // we need to search the list in reverse order.
  auto iter = std::find_if(vars_.rbegin(), vars_.rend(),
                           [&name](auto var) { return var->name() == name; });

  if (iter != vars_.rend()) {
    return iter->get();
  }

  // If not found in the current scope, check parent scopes.
  if (any_parent_) {
    return any_parent_->search_var(name);
  }

  return nullptr;
}

}  // namespace chibicpp