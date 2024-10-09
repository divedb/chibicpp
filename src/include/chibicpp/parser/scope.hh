#pragma once

#include <cassert>
#include <memory>
#include <string>
#include <vector>

#include "chibicpp/parser/symbol_table.hh"

namespace chibicpp {

class Var;
class Type;

/// A scope is a transient data structure that is used while parsing the
/// program.  It assists with resolving identifiers to the appropriate
/// declaration.
class Scope {
 public:
  /// These are bitfields that are or'd together when creating a scope, which
  /// defines the sorts of things the scope contains.
  enum Flag {
    /// This indicates that the scope corresponds to a function, which means
    /// that labels are set here.
    kFnScope = 0x01,

    /// This is a while, do, switch, for, etc that can have break statements
    /// embedded into it.
    kBreakScope = 0x02,

    /// This is a while, do, for, which can have continue statements embedded
    /// into it.
    kContinueScope = 0x04,

    /// This is a scope that can contain a declaration.  Some scopes just
    /// contain loop constructs but don't contain decls.
    kDeclScope = 0x08,

    /// The controlling scope in a if/switch/while/for statement.
    kControlScope = 0x10,

    /// The scope of a struct/union/class definition.
    kClassScope = 0x20,

    /// This is a scope that corresponds to a block/closure object. Blocks serve
    /// as top-level scopes for some objects like labels, they also prevent
    /// things like break and continue.  BlockScopes always have the FnScope and
    /// DeclScope flag set as well.
    kBlockScope = 0x40,

    /// This is a scope that corresponds to the template parameters of a C++
    /// template. Template parameter scope starts at the 'template' keyword and
    /// ends when the template declaration ends.
    kTemplateParamScope = 0x80,

    /// This is a scope that corresponds to the parameters within a function
    /// prototype.
    kFunctionPrototypeScope = 0x100,

    /// This is a scope that corresponds to the parameters within a function
    /// prototype for a function declaration (as opposed to any other kind of
    /// function declarator). Always has FunctionPrototypeScope set as well.
    kFunctionDeclarationScope = 0x200,

    /// This is a scope that corresponds to a switch statement.
    kSwitchScope = 0x1000,

    /// This is the scope of a C++ try statement.
    kTryScope = 0x2000,

    /// This is the scope for a function-level C++ try or catch scope.
    kFnTryCatchScope = 0x4000,

    /// This is the scope for the whole translation unit.
    ///
    /// TODO(gc): Do we need this scope?
    kTranslationUnitScope = 0x8000
  };

  /// Construct a scope with the specified parameters.
  ///
  /// \param flag The flag of the scope.
  /// \param depth The depth level of the scope (default is 0).
  /// \param parent A pointer to parent scope, if any (default is null).
  Scope(Flag flag, unsigned short depth = 0,
        ObserverPtr<Scope> parent = nullptr)
      : any_parent_{parent}, depth_{depth}, flag_{flag} {}

  ObserverPtr<Scope> parent() { return any_parent_; }
  unsigned short depth() const { return depth_; }
  unsigned short flag() const { return flag_; }

  /// Searches for a variable by name within the scope.
  ///
  /// \param name The name of the variable to search for.
  /// \return Pointer to the variable if found, nullptr otherwise.
  ObserverPtr<Var> search_var(const std::string& name) const;

  /// Append the variable into current scope.
  ///
  /// \param var A pointer to the variable.
  void append_var(ObserverPtr<Var> var) { vars_.push_back(var); }

  /// Clears all variables in the current scope, and recursively clears the
  /// parent scope if it exists.
  void clear() {
    vars_.clear();

    if (any_parent_) {
      any_parent_->clear();
    }
  }

 private:
  /// The parent scope for this scope.  This is null for the
  /// translation-unit scope.
  ObserverPtr<Scope> any_parent_;

  /// Depth - This is the depth of this scope.  The translation-unit scope has
  /// depth 0.
  unsigned short depth_;

  /// flag - This contains a set of Flag, which indicates how the scope
  /// interrelates with other control flow statements.
  unsigned short flag_;

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

  /// Keep track the variables in current scope.
  /// The is a view of the variable inside symbol table.
  std::vector<ObserverPtr<Var>> vars_;
};

class ScopeStack {
 public:
  ScopeStack(Scope::Flag flag) : cur_scope_{std::make_unique<Scope>(flag)} {}

  /// Enters a new scope defined by the specified flag.
  ///
  /// \param flag The type of the new scope.
  void enter(Scope::Flag flag) {
    int depth = cur_scope_->depth();
    ObserverPtr<Scope> parent = cur_scope_.get();

    scope_stack_.push_back(std::move(cur_scope_));
    cur_scope_ = std::make_unique<Scope>(flag, depth + 1, parent);
  }

  /// Exits the current scope.
  void leave() {
    assert(!scope_stack_.empty());

    cur_scope_ = std::move(scope_stack_.back());
    scope_stack_.pop_back();
  }

  /// \return Current scope.
  ObserverPtr<Scope> get_scope() const { return cur_scope_.get(); }

 private:
  /// Pointer to the current scope being processed.
  std::unique_ptr<Scope> cur_scope_;

  /// Stack of scopes. When entering a new scope, the current scope is pushed
  /// onto this stack.
  /// TODO(gc): We may use scope's parent to retrieve previous scope?
  std::vector<std::unique_ptr<Scope>> scope_stack_;
};

class VarScope {
 public:
  explicit VarScope(Scope::Flag flag)
      : sym_table_{std::make_unique<SymbolTable>()},
        var_stack_{flag},
        tag_stack_{flag},
        static_stack_{flag} {}

  /// Create a variable with the specified identifier and type.
  ///
  /// For example, the following code demonstrates creating a variable of type
  /// `int` when parser sees the variable `x` inside function:
  ///
  /// \code
  /// FunctionScope scope;
  /// scope.create_var("x", TypeMgr::get_signed_int());
  /// \endcode
  ///
  /// \param ident The name of the variable.
  /// \param type The type of the variable.
  /// \return A pointer to the newly created variable.
  ObserverPtr<Var> create_var(const std::string& ident,
                              ObserverPtr<Type> type) {
    auto var = sym_table_->create_var(ident, type, /*offset*/ 0);
    var_stack_.get_scope()->append_var(var);

    return var;
  }

  /// Create a tag with the specified identifier and type.
  ///
  /// For example, the following code demonstrates creating a tag:
  ///
  /// \code
  /// void foo () {
  ///   struct Person { int age; };
  /// }
  ///
  /// int main() {
  ///   FunctionScope fscope;
  ///   auto tag = fscope.create_tag("Person", TypeMgr::get_struct(...));
  /// }
  /// \endcode
  ///
  /// \param ident The name of the tag.
  /// \param type The type of the tag.
  /// \return A pointer to the newly created tag.
  ObserverPtr<Var> create_tag(const std::string& ident,
                              ObserverPtr<Type> type) {
    auto var = sym_table_->create_tag(ident, type);
    tag_stack_.get_scope()->append_var(var);

    return var;
  }

  /// Creates a string literal as a global variable with the specified content
  /// and type.
  ///
  /// \param content The string content of the literal.
  /// \param type The type of the variable.
  /// \return A pointer to the newly created string literal variable.
  ObserverPtr<Var> create_string_literal(const std::string& content,
                                         ObserverPtr<Type> type) {
    return sym_table_->create_string_literal(content, type);
  }

  ObserverPtr<Var> create_typedef(const std::string& name,
                                  ObserverPtr<Type> type,
                                  ObserverPtr<Type> type_def) {
    return sym_table_->create_typedef(name, type, type_def);
  }

  /// Search for the variable by name specified by the parameter.
  ///
  /// \param ident The name of the variable.
  /// \return A pointer to the variable if found, nullptr otherwise.
  ObserverPtr<Var> search_var(const std::string& ident) const {
    return var_stack_.get_scope()->search_var(ident);
  }

  /// Search for the tag by name specified by the parameter.
  ///
  /// \param ident The name of the tag.
  /// \return A pointer to the tag if found, nullptr otherwise.
  ObserverPtr<Var> search_tag(const std::string& ident) const {
    return tag_stack_.get_scope()->search_var(ident);
  }

  /// Enter a new scope.
  ///
  /// \param flag The type of scope.
  void enter(Scope::Flag flag) {
    var_stack_.enter(flag);
    tag_stack_.enter(flag);
    static_stack_.enter(flag);
  }

  /// Leave current scope.
  void leave() {
    static_stack_.leave();
    tag_stack_.leave();
    var_stack_.leave();
  }

  std::unique_ptr<SymbolTable> release_symbol_table() {
    // Need to clear scope stack.
    var_stack_.get_scope()->clear();
    tag_stack_.get_scope()->clear();
    static_stack_.get_scope()->clear();

    auto tmp = std::make_unique<SymbolTable>();
    std::swap(tmp, sym_table_);

    return tmp;
  }

 protected:
  /// The symbol table keep track of all the variables within the function.
  std::unique_ptr<SymbolTable> sym_table_;

 private:
  /// Scope for variables declared or defined within the function.
  ScopeStack var_stack_;
  /// Scope for structures declared within the function.
  ScopeStack tag_stack_;
  /// Scope for static variables declared or defined within the function.
  ScopeStack static_stack_;
};

}  // namespace chibicpp