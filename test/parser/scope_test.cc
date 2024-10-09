#include "chibicpp/parser/scope.hh"

#include <gtest/gtest.h>

#include "chibicpp/ast/node.hh"
#include "chibicpp/ast/type.hh"

using namespace std;
using namespace chibicpp;

TEST(VarBlockScope, EnterAndLeave) {
  // int main () {
  //     int a;
  //     int b;
  //     {
  //         int c;
  //         int d;
  //     }
  // }

  VarScope scope{Scope::kTranslationUnitScope};
  auto type = TypeFactory::get_signed_int();

  scope.enter(Scope::kFnScope);
  scope.create_var("a", type);
  scope.create_var("b", type);

  scope.enter(Scope::kBlockScope);
  scope.create_var("c", type);
  scope.create_var("d", type);

  auto d = scope.search_var("d");
  EXPECT_TRUE(static_cast<bool>(d));
  EXPECT_EQ("d", d->name());

  auto c = scope.search_var("c");
  EXPECT_TRUE(static_cast<bool>(c));
  EXPECT_EQ("c", c->name());

  auto b = scope.search_var("b");
  EXPECT_TRUE(static_cast<bool>(b));
  EXPECT_EQ("b", b->name());

  auto a = scope.search_var("a");
  EXPECT_TRUE(static_cast<bool>(a));
  EXPECT_EQ("a", a->name());

  // Leave block scope.
  scope.leave();

  EXPECT_FALSE(static_cast<bool>(scope.search_var("d")));
  EXPECT_FALSE(static_cast<bool>(scope.search_var("c")));

  b = scope.search_var("b");
  EXPECT_TRUE(static_cast<bool>(b));
  EXPECT_EQ("b", b->name());

  a = scope.search_var("a");
  EXPECT_TRUE(static_cast<bool>(a));
  EXPECT_EQ("a", a->name());

  // Leave function scope.
  scope.leave();

  EXPECT_FALSE(static_cast<bool>(scope.search_var("d")));
  EXPECT_FALSE(static_cast<bool>(scope.search_var("c")));
  EXPECT_FALSE(static_cast<bool>(scope.search_var("b")));
  EXPECT_FALSE(static_cast<bool>(scope.search_var("a")));
}

TEST(TagBlockScope, EnterAndLeave) {
  // int main() {
  //   struct Person {
  //     int a;
  //     int b;
  //   };

  //   {
  //     struct Person {
  //       short a;
  //       short b;
  //     };

  //     struct Person p;
  //     printf("%ld\n", sizeof(p.a));
  //   }
  // }
  // auto int_type = TypeFactory::get_signed_int();
  // auto short_type = TypeFactory::get_signed_short();
  // auto person1 = TypeFactory::get_struct("Person", int_type, int_type);
  // auto person2 = TypeFactory::get_struct("Person", short_type, short_type);
}

int main(int argc, char** argv) {
  ::testing::InitGoogleTest(&argc, argv);

  return RUN_ALL_TESTS();
}