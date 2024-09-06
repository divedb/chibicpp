#pragma once

#include <sstream>
#include <stdexcept>

namespace chibicpp {

#define CHIBICPP_THROW_ERROR(...) _throw_error(__FILE__, __LINE__, __VA_ARGS__)

template <typename... Args>
inline void _throw_error(char const* filename, const size_t lineno,
                         Args&&... args) {
  std::ostringstream oss;

  oss << "[" << filename << "@" << lineno << "]: ";
  (oss << ... << std::forward<Args>(args));

  throw std::runtime_error(oss.str());
}

}  // namespace chibicpp