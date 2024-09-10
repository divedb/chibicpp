#pragma once

namespace chibicpp {

enum class TypeKind { kInt, kPointer };

struct Type {
  TypeKind kind;
  Type* base;
};

class Node;

bool is_integer(Type* type);
void add_type(Node* node);

}  // namespace chibicpp