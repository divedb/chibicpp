#include "chibicpp/ast/type.hh"

#include <gtest/gtest.h>

using namespace chibicpp;

TEST(IntType, Equals) {
  auto i1 = TypeMgr::get_integer(Type::kInt);
  auto i2 = TypeMgr::get_integer(Type::kInt);
  auto i3 = TypeMgr::get_integer(Type::kInt | Type::kSigned);
  auto i4 = TypeMgr::get_integer(Type::kInt | Type::kUnsigned);

  EXPECT_EQ(i1, i2);
  EXPECT_EQ(i1, i3);
  EXPECT_TRUE(i1->equals(i2));
  EXPECT_TRUE(i1->is_signed());
  EXPECT_FALSE(i1->is_unsigned());
  EXPECT_TRUE(i1->equals(i3));
  EXPECT_NE(i1, i4);
  EXPECT_FALSE(i1->equals(i4));
  EXPECT_TRUE(i4->is_unsigned());

  auto c1 = TypeMgr::get_char();
  auto c2 = TypeMgr::get_char();

  EXPECT_EQ(c1, c2);
  EXPECT_TRUE(c1->equals(c2));

  auto c3 = TypeMgr::get_signed_char();
  auto c4 = TypeMgr::get_unsigned_char();

  EXPECT_NE(c1, c3);
  EXPECT_NE(c1, c4);
  EXPECT_NE(c3, c4);
  EXPECT_FALSE(c1->equals(c3));
  EXPECT_FALSE(c1->equals(c4));
  EXPECT_FALSE(c3->equals(c4));
}

TEST(Array, Equals) {
  // int a1[1];
  // int a2[1];
  // int a3[2];
  auto a1 = TypeMgr::get_array(1, TypeMgr::get_integer(Type::kInt));
  auto a2 = TypeMgr::get_array(1, TypeMgr::get_integer(Type::kInt));
  auto a3 = TypeMgr::get_array(2, TypeMgr::get_integer(Type::kInt));

  EXPECT_EQ(a1, a2);
  EXPECT_TRUE(a1->equals(a2));
  EXPECT_NE(a1, a3);
  EXPECT_FALSE(a1->equals(a3));

  // int a4[1][2];
  // int a5[1][2];
  // int a6[1][3];
  auto a4 = TypeMgr::get_array(2, a1);
  auto a5 = TypeMgr::get_array(2, a2);
  auto a6 = TypeMgr::get_array(3, a1);

  EXPECT_EQ(a4, a5);
  EXPECT_TRUE(a4->equals(a5));
  EXPECT_NE(a4, a6);
  EXPECT_FALSE(a4->equals(a6));
}

struct Bar {
  char c;
  short s;
};

struct Foo {
  char c;
  short s;
  int i;
};

TEST(Struct, Equals) {
  auto c = TypeMgr::get_char();
  auto s = TypeMgr::get_integer(Type::kShort);
  auto i = TypeMgr::get_integer(Type::kInt);

  auto st1 = TypeMgr::get_struct("Bar", c, s);
  auto st2 = TypeMgr::get_struct("Bar", c, s);
  auto st3 = TypeMgr::get_struct("Bar", c);

  EXPECT_TRUE(st1->is_struct());
  EXPECT_EQ(st1, st2);
  EXPECT_TRUE(st1->equals(st2));
  EXPECT_EQ(st1, st3);

  auto st4 = TypeMgr::get_struct("Foo", c, s, i);

  EXPECT_NE(st1, st4);
  EXPECT_FALSE(st1->equals(st4));
}

TEST(Pointer, Equals) {
  auto c = TypeMgr::get_char();
  auto s = TypeMgr::get_integer(Type::kShort);
  auto i = TypeMgr::get_integer(Type::kInt);

  auto pc = TypeMgr::get_pointer(c);
  auto ps = TypeMgr::get_pointer(s);
  auto pi = TypeMgr::get_pointer(i);

  EXPECT_TRUE(pc->is_pointer());
  EXPECT_FALSE(pc->equals(ps));
  EXPECT_FALSE(pc->equals(pi));
}

TEST(Struct, Size) {
  // struct { int a; } x;
  auto i = TypeMgr::get_integer(Type::kInt);
  auto st1 = TypeMgr::get_struct("Bar" + std::to_string(__LINE__), i);

  EXPECT_EQ(i->size_in_bytes(), st1->size_in_bytes());

  // struct { int a; char c; } x;
  auto c = TypeMgr::get_char();
  auto st2 = TypeMgr::get_struct("Bar" + std::to_string(__LINE__), i, c);

  EXPECT_EQ(i->size_in_bytes() + c->size_in_bytes(), st2->size_in_bytes());
}

int main(int argc, char** argv) {
  ::testing::InitGoogleTest(&argc, argv);

  return RUN_ALL_TESTS();
}