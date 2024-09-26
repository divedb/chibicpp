#include "chibicpp/util/observer_ptr.hh"

#include <gtest/gtest.h>

#include <memory>
#include <string>

using namespace chibicpp;

class Person {
 public:
  Person() = default;
  Person(const std::string& name, int age, int salary)
      : name_{name}, age_{age}, salary_{salary} {}

  virtual ~Person() = default;

  virtual std::string info() const { return name_; }

  std::string name() const { return name_; }
  int age() const { return age_; }
  int salary() const { return salary_; };

 protected:
  std::string name_;
  int age_;
  int salary_;
};

class SoftwareEngineer : public Person {
 public:
  SoftwareEngineer(const std::string& title, const std::string& name, int age,
                   int salary)
      : Person{name, age, salary}, title_{title} {}

  std::string info() const override { return name_ + ':' + title_; }

 private:
  std::string title_;
};

TEST(Constructor, Default) {
  ObserverPtr<int> p1;

  EXPECT_TRUE(p1.get() == nullptr);
  EXPECT_FALSE(static_cast<bool>(p1));

  ObserverPtr<int> p2{nullptr};

  EXPECT_TRUE(p2.get() == nullptr);
  EXPECT_FALSE(static_cast<bool>(p2));
}

TEST(Constructor, MemberAccess) {
  auto person = std::make_unique<Person>("Scott Herb", 40, 150000);
  ObserverPtr<Person> p{person.get()};

  EXPECT_TRUE(p.get() != nullptr);
  EXPECT_TRUE(static_cast<bool>(p));

  EXPECT_EQ(p->name(), "Scott Herb");
  EXPECT_EQ(p->age(), 40);
  EXPECT_EQ(p->salary(), 150000);

  p.release();

  EXPECT_TRUE(p.get() == nullptr);
  EXPECT_FALSE(static_cast<bool>(p));
}

TEST(Constructor, Inheritance) {
  auto engineer = std::make_unique<SoftwareEngineer>("Compiler Designer",
                                                     "Scott Herb", 40, 150000);
  ObserverPtr<Person> p{engineer.get()};

  EXPECT_EQ(p->name(), "Scott Herb");
  EXPECT_EQ(p->age(), 40);
  EXPECT_EQ(p->salary(), 150000);

  p = engineer.get();

  EXPECT_EQ(p->name(), "Scott Herb");
  EXPECT_EQ(p->age(), 40);
  EXPECT_EQ(p->salary(), 150000);

  auto freshman = std::make_unique<SoftwareEngineer>("Frontend Designer",
                                                     "Jack", 25, 10000);

  p = freshman.get();

  EXPECT_EQ(p->name(), "Jack");
  EXPECT_EQ(p->age(), 25);
  EXPECT_EQ(p->salary(), 10000);
  EXPECT_EQ(p->info(), "Jack:Frontend Designer");
}

int main(int argc, char** argv) {
  ::testing::InitGoogleTest(&argc, argv);

  return RUN_ALL_TESTS();
}