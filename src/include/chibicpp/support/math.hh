#pragma once

#include <cstddef>

namespace chibicpp {

template <typename T>
constexpr size_t align_to() {
  return (sizeof(T) + alignof(T) - 1) & ~(alignof(T) - 1);
}

constexpr size_t align_to(size_t size, size_t alignment) {
  return (size + alignment - 1) & (~(alignment - 1));
}

}  // namespace chibicpp