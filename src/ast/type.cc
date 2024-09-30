#include "chibicpp/ast/type.hh"

#include <cassert>
#include <iostream>

#include "chibicpp/ast/node.hh"
#include "chibicpp/common/error.hh"

namespace chibicpp {

Type::Type(ID id, ObserverPtr<Type> base)
    : layout_{type_id_to_layout(id)}, id_{id}, base_{base} {}

std::string Type::to_string() const {
  std::string str;

  static std::map<Type::ID, std::string> primitive_type_descriptor{
      {kSigned, "signed"}, {kUnsigned, "unsigned"},  {kShort, "short"},
      {kLong, "long"},     {kLongLong, "long long"}, {kChar, "char"},
      {kInt, "int"},       {kFloat, "float"},        {kDouble, "double"}};

  for (auto key : {kSigned, kUnsigned, kShort, kLong, kLongLong, kChar, kInt,
                   kFloat, kDouble}) {
    if (id() & key) {
      str += primitive_type_descriptor[key];
    }
  }

  if (is_pointer()) {
    str = base_->to_string() + '*';
  }

  return str;
}

void Member::dump(std::ostream& os) const {
  os << "[name  ]: " << name_ << '\n';
  os << "[type  ]: " << type_->to_string() << '\n';
  os << "[index ]: " << index_ << '\n';
  os << "[offset]: " << offset_ << '\n';
}

}  // namespace chibicpp