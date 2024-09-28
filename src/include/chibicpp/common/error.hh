#pragma once

#include <sstream>
#include <stdexcept>

#define CHIBICPP_THROW_ERROR(...) _throw_error(__FILE__, __LINE__, __VA_ARGS__)

namespace chibicpp {

extern std::string error;

template <typename... Args>
inline void _throw_error(char const* filename, const size_t lineno,
                         Args&&... args) {
  std::ostringstream oss;

  oss << "[" << filename << "@" << lineno << "]: ";
  (oss << ... << std::forward<Args>(args));

  error = oss.str();

  throw std::runtime_error(error);
}

}  // namespace chibicpp