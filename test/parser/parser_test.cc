#include "chibicpp/parser/parser.hh"

#include <gtest/gtest.h>

#include <map>
#include <string>

#include "chibicpp/ast/ast_context.hh"
#include "chibicpp/ast/visitor.hh"
#include "chibicpp/common/macro.hh"
#include "chibicpp/lex/tokenizer.hh"
#include "chibicpp/util/observer_ptr.hh"

using namespace std;
using namespace chibicpp;

class Counter : public AstVisitor {
 public:
  int global_var_count() const { return nglobals_; }

  int local_var_count(const string& fname) const {
    auto iter = fn_locals_.find(fname);

    if (iter == fn_locals_.end()) {
      return 0;
    }

    return iter->second;
  }

  void visit_global(ObserverPtr<Var> var, AstContext& context) override {
    chibicpp_ignore(var, context);

    nglobals_++;
  }

  void visit_program(ObserverPtr<Program> prog, AstContext& context) override {
    chibicpp_ignore(prog, context);

    prog->accept(*this, context);
  }

  void visit_function(ObserverPtr<Function> func,
                      AstContext& context) override {
    auto fname = func->name();
    fn_locals_[fname] = func->local_var_count();

    func->accept(*this, context);
  }

  void visit_function_params(ObserverPtr<Var> var, int idx,
                             AstContext& context) override {
    chibicpp_ignore(var, idx, context);
  }

  void visit_function_body(ObserverPtr<Node> node,
                           AstContext& context) override {
    chibicpp_ignore(node, context);
  }

  void visit_node(ObserverPtr<Node> node, AstContext& context) override {
    chibicpp_ignore(node, context);
  }

 private:
  int nglobals_{};              ///< The number of globals.
  map<string, int> fn_locals_;  ///< The number of locals inside function.
};

TEST(Function, LocalVarCount) {
  auto input = "int main() { int a; int z; a=3; z=5; return a+z; }";
  Lexer lexer{input, std::strlen(input)};
  Parser parser{lexer};

  Counter counter;
  AstContext context;
  auto prog = parser.parse_program();

  counter.visit_program(prog.get(), context);

  EXPECT_EQ(0, counter.global_var_count());
  EXPECT_EQ(2, counter.local_var_count("main"));
}

int main(int argc, char** argv) {
  ::testing::InitGoogleTest(&argc, argv);

  return RUN_ALL_TESTS();
}