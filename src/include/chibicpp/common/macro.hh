#pragma once

namespace chibicpp {

template <typename... Args>
inline void chibicpp_ignore([[maybe_unused]] Args&&... expr) {}

}  // namespace chibicpp