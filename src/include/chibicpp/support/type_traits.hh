#pragma once

#include <cstddef>

namespace chibicpp {

template <typename T>
struct TypeTraits {
 public:
  static constexpr size_t size = sizeof(T);
  static constexpr size_t alignment = alignof(T);
};

}  // namespace chibicpp