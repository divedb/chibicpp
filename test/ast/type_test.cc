#include "chibicpp/ast/type.hh"

#include <gtest/gtest.h>

using namespace chibicpp;

TEST(IntType, Layout) {
  auto i1 = TypeFactory::get_signed_int();

  EXPECT_EQ(alignof(int), i1->alignment());
  EXPECT_EQ(sizeof(int), i1->size_in_bytes());

  auto c1 = TypeFactory::get_signed_char();

  EXPECT_EQ(alignof(signed char), c1->alignment());
  EXPECT_EQ(sizeof(signed char), c1->size_in_bytes());

  auto s = TypeFactory::get_signed_short();

  EXPECT_EQ(alignof(signed short), s->alignment());
  EXPECT_EQ(sizeof(signed short), s->size_in_bytes());

  auto l = TypeFactory::get_signed_long();

  EXPECT_EQ(alignof(signed long), l->alignment());
  EXPECT_EQ(sizeof(signed long), l->size_in_bytes());

  auto ll = TypeFactory::get_signed_long_long();

  EXPECT_EQ(alignof(signed long long), ll->alignment());
  EXPECT_EQ(sizeof(signed long long), ll->size_in_bytes());
}

TEST(FloatType, Layout) {
  auto f1 = TypeFactory::get_float();
  auto f2 = TypeFactory::get_float();

  EXPECT_TRUE(f1->is_float());
  EXPECT_FALSE(f1->is_signed());
  EXPECT_FALSE(f1->is_unsigned());
  EXPECT_FALSE(f1->is_integer());
  EXPECT_FALSE(f1->is_pointer());
  EXPECT_EQ(f1, f2);
  EXPECT_EQ(sizeof(float), f1->size_in_bytes());
  EXPECT_EQ(alignof(float), f1->alignment());

  auto d1 = TypeFactory::get_double();

  EXPECT_TRUE(d1->is_double());
  EXPECT_FALSE(d1->is_signed());
  EXPECT_FALSE(d1->is_unsigned());
  EXPECT_FALSE(d1->is_integer());
  EXPECT_FALSE(d1->is_pointer());
  EXPECT_NE(f1, d1);
  EXPECT_EQ(sizeof(double), d1->size_in_bytes());
  EXPECT_EQ(alignof(double), d1->alignment());
}

TEST(Pointer, Layout) {
  // int* pi;
  // short* ps;
  // char** ppc;
  auto pi = TypeFactory::get_pointer(TypeFactory::get_signed_int());
  auto ps = TypeFactory::get_pointer(TypeFactory::get_signed_short());
  auto ppc = TypeFactory::get_pointer(
      TypeFactory::get_pointer(TypeFactory::get_char()));

  EXPECT_TRUE(pi->is_pointer());
  EXPECT_TRUE(ps->is_pointer());
  EXPECT_TRUE(ppc->is_pointer());

  EXPECT_NE(pi, ps);
  EXPECT_NE(pi, ppc);
  EXPECT_NE(ps, ppc);

  EXPECT_EQ(sizeof(void*), pi->size_in_bytes());
  EXPECT_EQ(sizeof(void*), ps->size_in_bytes());
  EXPECT_EQ(sizeof(void*), ppc->size_in_bytes());
  EXPECT_EQ(alignof(void*), pi->alignment());
  EXPECT_EQ(alignof(void*), ps->alignment());
  EXPECT_EQ(alignof(void*), ppc->alignment());
}

TEST(Array, Layout) {
  // int a[3];
  // int b[3][4];
  auto a = TypeFactory::get_array(3, TypeFactory::get_signed_int());
  auto b = TypeFactory::get_array(4, a);

  EXPECT_TRUE(a->is_array());
  EXPECT_TRUE(b->is_array());

  EXPECT_EQ(alignof(int), a->alignment());
  EXPECT_EQ(alignof(int), b->alignment());
  EXPECT_EQ(sizeof(int) * 3, a->size_in_bytes());
  EXPECT_EQ(sizeof(int) * 12, b->size_in_bytes());
}

TEST(Struct, Layout) {
  // struct { int a; } x;
  auto i = TypeFactory::get_signed_int();
  auto st1 = TypeFactory::get_struct("Bar", i);

  EXPECT_EQ(i->alignment(), st1->alignment());
  EXPECT_EQ(i->size_in_bytes(), st1->size_in_bytes());

  // struct {
  //   int a;
  //   char c;
  // } x;
  auto c = TypeFactory::get_char();
  auto st2 = TypeFactory::get_struct("Bar", i, c);

  EXPECT_EQ(i->alignment(), st2->alignment());
  EXPECT_EQ(align_to(i->size_in_bytes() + c->size_in_bytes(), st2->alignment()),
            st2->size_in_bytes());

  EXPECT_NE(st1, st2);
}

TEST(IntType, Equals) {
  auto i1 = TypeFactory::get_signed_int();
  auto i2 = TypeFactory::get_signed_int();
  auto i3 = TypeFactory::get_signed_int();
  auto i4 = TypeFactory::get_unsigned_int();

  EXPECT_EQ(i1, i2);
  EXPECT_EQ(i1, i3);
  EXPECT_TRUE(i1->equals(i2));
  EXPECT_TRUE(i1->is_signed());
  EXPECT_FALSE(i1->is_unsigned());
  EXPECT_TRUE(i1->equals(i3));
  EXPECT_NE(i1, i4);
  EXPECT_FALSE(i1->equals(i4));
  EXPECT_TRUE(i4->is_unsigned());

  auto c1 = TypeFactory::get_char();
  auto c2 = TypeFactory::get_char();

  EXPECT_EQ(c1, c2);
  EXPECT_TRUE(c1->equals(c2));
  EXPECT_TRUE(c1->is_integer());
  EXPECT_FALSE(c1->is_signed());
  EXPECT_FALSE(c1->is_unsigned());

  auto c3 = TypeFactory::get_signed_char();
  auto c4 = TypeFactory::get_unsigned_char();

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
  auto a1 = TypeFactory::get_array(1, TypeFactory::get_signed_int());
  auto a2 = TypeFactory::get_array(1, TypeFactory::get_signed_int());
  auto a3 = TypeFactory::get_array(2, TypeFactory::get_signed_int());

  EXPECT_EQ(a1, a2);
  EXPECT_TRUE(a1->equals(a2));
  EXPECT_NE(a1, a3);
  EXPECT_FALSE(a1->equals(a3));

  // int a4[1][2];
  // int a5[1][2];
  // int a6[1][3];
  auto a4 = TypeFactory::get_array(2, a1);
  auto a5 = TypeFactory::get_array(2, a2);
  auto a6 = TypeFactory::get_array(3, a1);

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
  auto c = TypeFactory::get_char();
  auto s = TypeFactory::get_signed_short();
  auto i = TypeFactory::get_signed_int();

  auto st1 = TypeFactory::get_struct("Bar", c, s);
  auto st2 = TypeFactory::get_struct("Bar", c, s);
  auto st3 = TypeFactory::get_struct("Bar", c);

  EXPECT_TRUE(st1->is_struct());
  EXPECT_EQ(st1, st2);
  EXPECT_TRUE(st1->equals(st2));
  EXPECT_NE(st1, st3);

  auto st4 = TypeFactory::get_struct("Foo", c, s, i);

  EXPECT_NE(st1, st4);
  EXPECT_FALSE(st1->equals(st4));
}

TEST(Pointer, Equals) {
  auto c = TypeFactory::get_char();
  auto s = TypeFactory::get_signed_short();
  auto i = TypeFactory::get_signed_int();

  auto pc = TypeFactory::get_pointer(c);
  auto ps = TypeFactory::get_pointer(s);
  auto pi = TypeFactory::get_pointer(i);

  EXPECT_TRUE(pc->is_pointer());
  EXPECT_FALSE(pc->equals(ps));
  EXPECT_FALSE(pc->equals(pi));
}

int main(int argc, char** argv) {
  ::testing::InitGoogleTest(&argc, argv);

  return RUN_ALL_TESTS();
}