#pragma once

#include <string>
#include <vector>

namespace chibicpp {

class Var;

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
    /// C++ template. Template parameter scope starts at the 'template' keyword
    /// and ends when the template declaration ends.
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
    kFnTryCatchScope = 0x4000
  };

  Scope(unsigned short depth, unsigned short flags, Scope* parent = nullptr)
      : any_parent_{parent}, depth_{depth}, flags_{flags} {}

  Scope* parent() { return any_parent_; }
  const Scope* parent() const { return any_parent_; }
  unsigned short depth() const { return depth_; }
  unsigned short flags() const { return flags_; }

  void add_var(Var* var) { vars_.push_back(var); }
  Var* search_var(const std::string& name);
  const Var* search_var(const std::string& name) const;

 private:
  /// The parent scope for this scope.  This is null for the
  /// translation-unit scope.
  Scope* any_parent_;

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
  Scope* fn_parent_;

  /// BreakParent/ContinueParent - This is a direct link to the innermost
  /// BreakScope/ContinueScope which contains the contents of this scope
  /// for control flow purposes (and might be this scope itself), or null
  /// if there is no such scope.
  Scope* break_parent_;
  Scope* continue_parent_;

  /// BlockParent - This is a direct link to the immediately containing
  /// BlockScope if this scope is not none, or null if there is none.
  Scope* block_parent_;

  /// TemplateParamParent - This is a direct link to the
  /// immediately containing template parameter scope. In the
  /// case of nested templates, template parameter scopes can have
  /// other template parameter scopes as parents.
  Scope* template_param_parent_;

  std::vector<Var*> vars_;
};

}  // namespace chibicpp