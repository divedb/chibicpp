#pragma once

#include <memory>
#include <string>
#include <vector>

#include "chibicpp/support/observer_ptr.hh"

namespace chibicpp {

class Type;
class Var;

class SymbolTableIterator;

class SymbolTable {
 public:
  ObserverPtr<Var> create_var(const std::string& ident, ObserverPtr<Type> type,
                              int offset) {
    return create_var(vars_, ident, type, offset);
  }

  ObserverPtr<Var> create_tag(const std::string& ident,
                              ObserverPtr<Type> type) {
    return create_var(tags_, ident, type, /*offset*/ 0);
  }

  ObserverPtr<Var> create_string_literal(const std::string& content,
                                         ObserverPtr<Type> type) {
    return create_var(string_literals_, create_label_constant(), content, type);
  }

  ObserverPtr<Var> create_typedef(const std::string& name,
                                  ObserverPtr<Type> type,
                                  ObserverPtr<Type> type_def) {
    return create_var(vars_, name, type, type_def);
  }

 private:
  friend class SymbolTableIterator;

  /// Create a new variable in the table.
  ///
  /// \param ...args Constructor arguments for the variable.
  /// \return Pointer to the newly created variable.
  template <typename... Args>
  static ObserverPtr<Var> create_var(std::vector<std::unique_ptr<Var>>& owners,
                                     Args&&... args) {
    owners.push_back(std::make_unique<Var>(std::forward<Args>(args)...));

    return owners.back().get();
  }

  static std::string create_label_constant() {
    return ".LC" + std::to_string(label_const_seq_++);
  }

  /// The label constant label generates assembly code in .rodata section.
  /// Used for generating labels for string literals.
  static int label_const_seq_;

  /// Keep track of variables.
  std::vector<std::unique_ptr<Var>> vars_;
  /// Keep track of string literals.
  std::vector<std::unique_ptr<Var>> string_literals_;
  /// Keep track of tags.
  std::vector<std::unique_ptr<Var>> tags_;
  /// Keep track of statics.
  std::vector<std::unique_ptr<Var>> statics_;
};

class SymbolTableIterator {
 public:
  explicit SymbolTableIterator(std::unique_ptr<SymbolTable> sym_table)
      : sym_table_{std::move(sym_table)} {}

  auto string_literal_begin() { return sym_table_->string_literals_.begin(); }
  auto string_literal_begin() const {
    return sym_table_->string_literals_.begin();
  }
  auto string_literal_end() { return sym_table_->string_literals_.end(); }
  auto string_literal_end() const { return sym_table_->string_literals_.end(); }

  auto static_begin() { return sym_table_->statics_.begin(); }
  auto static_begin() const { return sym_table_->statics_.begin(); }
  auto static_end() { return sym_table_->statics_.end(); }
  auto static_end() const { return sym_table_->statics_.end(); }

  auto var_begin() { return sym_table_->vars_.begin(); }
  auto var_begin() const { return sym_table_->vars_.begin(); }
  auto var_end() { return sym_table_->vars_.end(); }
  auto var_end() const { return sym_table_->vars_.end(); }

  auto var_rbegin() { return sym_table_->vars_.rbegin(); }
  auto var_rbegin() const { return sym_table_->vars_.rbegin(); }
  auto var_rend() { return sym_table_->vars_.rend(); }
  auto var_rend() const { return sym_table_->vars_.rend(); }

 private:
  std::unique_ptr<SymbolTable> sym_table_;
};

}  // namespace chibicpp