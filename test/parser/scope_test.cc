#include "chibicpp/parser/scope.hh"

#include <gtest/gtest.h>

#include "chibicpp/ast/node.hh"
#include "chibicpp/ast/type.hh"

using namespace std;
using namespace chibicpp;

TEST(Scope, BlockScope) {
  // int main () {
  //     int a;
  //     int b;
  //     {
  //         int c;
  //         int d;
  //     }
  // }

  VarScope scope;
  auto type = TypeMgr::get_integer(Type::kInt);

  scope.enter(Scope::kFnScope);
  scope.create_local_var("a", type);
  scope.create_local_var("b", type);

  scope.enter(Scope::kBlockScope);
  scope.create_local_var("c", type);
  scope.create_local_var("d", type);

  EXPECT_TRUE(static_cast<bool>(scope.get_var("d")));
  EXPECT_TRUE(static_cast<bool>(scope.get_var("c")));
  EXPECT_TRUE(static_cast<bool>(scope.get_var("b")));
  EXPECT_TRUE(static_cast<bool>(scope.get_var("a")));

  scope.leave();

  EXPECT_FALSE(static_cast<bool>(scope.get_var("d")));
  EXPECT_FALSE(static_cast<bool>(scope.get_var("c")));
  EXPECT_TRUE(static_cast<bool>(scope.get_var("b")));
  EXPECT_TRUE(static_cast<bool>(scope.get_var("a")));

  scope.leave();

  EXPECT_FALSE(static_cast<bool>(scope.get_var("d")));
  EXPECT_FALSE(static_cast<bool>(scope.get_var("c")));
  EXPECT_FALSE(static_cast<bool>(scope.get_var("b")));
  EXPECT_FALSE(static_cast<bool>(scope.get_var("a")));
}

int main(int argc, char** argv) {
  ::testing::InitGoogleTest(&argc, argv);

  return RUN_ALL_TESTS();
}