#include "chibicpp/ast/type.hh"

#include <cassert>

#include "chibicpp/ast/node.hh"
#include "chibicpp/common/error.hh"

namespace chibicpp {

std::string Type::to_string() const {
  std::string str;

  static std::map<TypeID, std::string> primitive_type_descriptor{
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

}  // namespace chibicpp