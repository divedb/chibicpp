#pragma once

#include <cassert>
#include <memory>
#include <string>
#include <vector>

#include "chibicpp/util/observer_ptr.hh"

namespace chibicpp {

class Var;
class Type;

/// \brief A scope is a transient data structure that is used while parsing the
///        program.  It assists with resolving identifiers to the appropriate
///        declaration.
class Scope {
 public:
  /// \brief These are bitfields that are or'd together when creating a scope,
  ///        which defines the sorts of things the scope contains.
  enum ScopeFlags {
    /// \brief This indicates that the scope corresponds to a function, which
    ///        means that labels are set here.
    kFnScope = 0x01,

    /// \brief This is a while, do, switch, for, etc that can have break
    ///        statements embedded into it.
    kBreakScope = 0x02,

    /// \brief This is a while, do, for, which can have continue statements
    ///        embedded into it.
    kContinueScope = 0x04,

    /// \brief This is a scope that can contain a declaration.  Some scopes just
    ///        contain loop constructs but don't contain decls.
    kDeclScope = 0x08,

    /// \brief The controlling scope in a if/switch/while/for statement.
    kControlScope = 0x10,

    /// \brief The scope of a struct/union/class definition.
    kClassScope = 0x20,

    /// \brief This is a scope that corresponds to a block/closure object.
    ///        Blocks serve as top-level scopes for some objects like labels,
    ///        they also prevent things like break and continue.  BlockScopes
    ///        always have the FnScope and DeclScope flags set as well.
    kBlockScope = 0x40,

    /// \brief This is a scope that corresponds to the template parameters of a
    ///        C++ template. Template parameter scope starts at the 'template'
    ///        keyword and ends when the template declaration ends.
    kTemplateParamScope = 0x80,

    /// \brief This is a scope that corresponds to the parameters within a
    ///        function prototype.
    kFunctionPrototypeScope = 0x100,

    /// \brief This is a scope that corresponds to the parameters within a
    ///        function prototype for a function declaration (as opposed to any
    ///        other kind of function declarator). Always has
    ///        FunctionPrototypeScope set as well.
    kFunctionDeclarationScope = 0x200,

    /// \brief This is a scope that corresponds to a switch statement.
    kSwitchScope = 0x1000,

    /// \brief This is the scope of a C++ try statement.
    kTryScope = 0x2000,

    /// \brief This is the scope for a function-level C++ try or catch scope.
    kFnTryCatchScope = 0x4000,

    /// \brief This is the scope for the whole translation unit.
    ///
    /// TODO(gc): Do we need this scope?
    kTranslationUnitScope = 0x8000
  };

  Scope(unsigned short depth, unsigned short flags,
        ObserverPtr<Scope> parent = nullptr)
      : any_parent_{parent}, depth_{depth}, flags_{flags} {}

  ObserverPtr<Scope> parent() { return any_parent_; }
  unsigned short depth() const { return depth_; }
  unsigned short flags() const { return flags_; }

  void add_var(ObserverPtr<Var> var) { vars_.push_back(var); }
  ObserverPtr<Var> search_var(const std::string& name) const;

 private:
  /// The parent scope for this scope.  This is null for the
  /// translation-unit scope.
  ObserverPtr<Scope> any_parent_;

  /// Depth - This is the depth of this scope.  The translation-unit scope has
  /// depth 0.
  unsigned short depth_;

  /// Flags - This contains a set of ScopeFlags, which indicates how the scope
  /// interrelates with other control flow statements.
  unsigned short flags_;

  /// PrototypeDepth - This is the number of function prototype scopes
  /// enclosing this scope, including this scope.
  ///
  /// void foo(void (*bar)(int, double))
  ///
  unsigned short prototype_depth_;

  /// PrototypeIndex - This is the number of parameters currently
  /// declared in this scope.
  unsigned short prototype_index_;

  /// FnParent - If this scope has a parent scope that is a function body, this
  /// pointer is non-null and points to it.  This is used for label processing.
  ObserverPtr<Scope> fn_parent_;

  /// BreakParent/ContinueParent - This is a direct link to the innermost
  /// BreakScope/ContinueScope which contains the contents of this scope
  /// for control flow purposes (and might be this scope itself), or null
  /// if there is no such scope.
  ObserverPtr<Scope> break_parent_;
  ObserverPtr<Scope> continue_parent_;

  /// BlockParent - This is a direct link to the immediately containing
  /// BlockScope if this scope is not none, or null if there is none.
  ObserverPtr<Scope> block_parent_;

  /// TemplateParamParent - This is a direct link to the
  /// immediately containing template parameter scope. In the
  /// case of nested templates, template parameter scopes can have
  /// other template parameter scopes as parents.
  ObserverPtr<Scope> template_param_parent_;

  std::vector<ObserverPtr<Var>> vars_;
};

class VarScope {
 public:
  VarScope()
      : cur_scope_{std::make_unique<Scope>(0, Scope::kTranslationUnitScope)} {}

  /// \brief Create a local variable with the specified identifier and type.
  ///
  /// For example, the following code demonstrates creating a local
  /// variable when parser sees the variable `x` inside function `foo`:
  ///
  /// \code
  /// VarScope scope;
  /// scope.create_local_var("x", TypeMgr::get_primitive(int));
  /// \endcode
  ///
  /// \param ident The name of the variable.
  /// \param type The type of the variable.
  /// \return A pointer to the newly created variable.
  ObserverPtr<Var> create_local_var(const std::string& ident,
                                    ObserverPtr<Type> type) {
    return create_var_impl(locals_, ident, type, /*offset*/ 0);
  }

  /// \brief Creates a global variable with the specified identifier and type.
  ///
  /// \param ident The name of the variable.
  /// \param type The type of the variable.
  /// \return A pointer to the newly created global variable.
  ObserverPtr<Var> create_global_var(const std::string& ident,
                                     ObserverPtr<Type> type) {
    return create_var_impl(globals_, ident, type);
  }

  /// \brief Creates a string literal as a global variable with the specified
  ///        content and type.
  ///
  /// \param content The string content of the literal.
  /// \param type The type of the variable.
  /// \return A pointer to the newly created string literal variable.
  ObserverPtr<Var> create_string_literal(const std::string& content,
                                         ObserverPtr<Type> type) {
    return create_var_impl(globals_, create_global_label(), content, type);
  }

  std::vector<std::unique_ptr<Var>> release_globals() {
    auto ret = std::move(globals_);
    globals_.clear();

    return ret;
  }

  std::vector<std::unique_ptr<Var>> release_locals() {
    auto ret = std::move(locals_);
    locals_.clear();

    return ret;
  }

  /// \brief Search for variable by the specified name.
  ///
  /// \param ident The name of the variable.
  /// \return A pointer to `Var` if it exists, nullptr otherwise.
  ObserverPtr<Var> get_var(const std::string& ident) {
    assert(cur_scope_);

    auto var = cur_scope_->search_var(ident);

    return var;
  }

  /// \brief Enters a new scope defined by the specified flag.
  ///
  /// This method saves the current scope and transitions to a new scope
  /// based on the provided scope flag.
  ///
  /// \param flag The type of the new scope.
  void enter(Scope::ScopeFlags flag) {
    int depth = cur_scope_->depth();
    ObserverPtr<Scope> parent = cur_scope_.get();

    scope_stack_.push_back(std::move(cur_scope_));
    cur_scope_ = std::make_unique<Scope>(depth + 1, flag, parent);
  }

  /// \brief Exits the current scope.
  ///
  /// This method restores the previous scope, effectively leaving the
  /// current context and returning to the outer scope.
  void leave() {
    assert(!scope_stack_.empty());

    cur_scope_ = std::move(scope_stack_.back());
    scope_stack_.pop_back();
  }

  /// \brief Get the depth of current scope.
  ///
  /// \return The depth of current scope.
  int depth() const { return cur_scope_->depth(); }

  /// \brief Get the type of current scope
  ///
  /// \return The type of current scope
  int flags() const { return cur_scope_->flags(); }

 private:
  std::string create_global_label() {
    return ".L.data." + std::to_string(global_label_++);
  }

  template <typename... Args>
  ObserverPtr<Var> create_var_impl(std::vector<std::unique_ptr<Var>>& vars,
                                   Args&&... args) {
    vars.push_back(std::make_unique<Var>(std::forward<Args>(args)...));
    auto var = make_observer(vars.back().get());
    cur_scope_->add_var(var);

    return var;
  }

  /// Global label used for generating labels for string literals.
  int global_label_{};

  /// Pointer to the current scope being processed.
  std::unique_ptr<Scope> cur_scope_;

  /// Stack of scopes. When entering a new scope, the current scope is pushed
  /// onto this stack.
  /// TODO(gc): We may use scope's parent to retrieve previous scope?
  std::vector<std::unique_ptr<Scope>> scope_stack_;

  /// Collection of all global variables.
  std::vector<std::unique_ptr<Var>> globals_;

  /// Collection of all local variables within functions, blocks, etc.
  std::vector<std::unique_ptr<Var>> locals_;
};

}  // namespace chibicpp