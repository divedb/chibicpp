#include "chibicpp/support/mem_alloc.hh"

#include <new>

void* chibicpp::allocate_buffer(size_t size, size_t alignment) {
  return ::operator new(size, std::align_val_t(alignment));
}

void chibicpp::deallocate_buffer(void* ptr, size_t size, size_t alignment) {
  ::operator delete(ptr, size, std::align_val_t(alignment));
}
