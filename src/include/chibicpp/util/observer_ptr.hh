#pragma once

#include <cstddef>
#include <functional>  // for std::hash
#include <type_traits>
#include <utility>

namespace chibicpp {

/// \brief `ObserverPtr` is a non-owning pointer. It stores a pointer to a
///        second object, known as the watched object. An `ObserverPtr` may also
///        have no watched object.
///        An observer is not responsible in any way for the watched object;
///        there is no inherent relationship between an observer and the object
///        it watches.
///        It is intended as a near drop-in replacement for raw pointer types,
///        with the advantage that, as a vocabulary type, it indicates its
///        intended use without need for detailed analysis by code readers.
template <typename T>
class ObserverPtr {
 public:
  using element_type = T;

  /// @name Constructor
  /// @{

  /// \brief Constructs an `ObserverPtr` that has no corresponding watched
  ///        object.
  constexpr ObserverPtr() noexcept = default;
  constexpr ObserverPtr(std::nullptr_t) noexcept {}

  /// \brief Constructs an `ObserverPtr` that watches p.
  ///
  /// \param p A pointer to the object to be observed.
  constexpr ObserverPtr(element_type* p) noexcept : ptr_{p} {}

  /// \brief Constructs an `ObserverPtr` that watches other.get(). This overload
  ///        participates in overload resolution only if U* is convertible to
  ///        element_type*.
  ///
  /// \tparam U Other type watched.
  /// \param other Other `ObserverPtr`.
  template <typename U, typename = std::enable_if_t<
                            std::is_convertible_v<U*, element_type*> &&
                            !std::is_same_v<T, U>>>
  constexpr ObserverPtr(ObserverPtr<U> other) : ptr_{other.get()} {}

  constexpr ObserverPtr(const ObserverPtr&) = default;
  constexpr ObserverPtr(ObserverPtr&&) noexcept = default;

  /// @}

  constexpr ObserverPtr& operator=(const ObserverPtr&) = default;
  constexpr ObserverPtr& operator=(ObserverPtr&&) noexcept = default;

  template <typename U, typename = std::enable_if_t<
                            std::is_convertible_v<U*, element_type*> &&
                            !std::is_same_v<ObserverPtr<T>, U>>>
  constexpr ObserverPtr& operator=(U* p) {
    ptr_ = p;

    return *this;
  }

  /// @name Modifiers
  /// @{

  /// \brief Stop watching the watched object, if any. get() returns nullptr
  ///        after the call.
  ///
  /// \return A pointer to the previously watched object, or nullptr if there
  ///         was no watched object, i.e. the value which would be returned by
  ///         get() before the call.
  constexpr element_type* release() noexcept {
    return std::exchange(ptr_, nullptr);
  }

  /// \brief Set *this to watch the object pointed to by p. get() returns p
  ///        after the call.
  /// \param p Pointer to a new object to watch.
  constexpr void reset(element_type* p = nullptr) noexcept { ptr_ = p; }

  /// \brief Swaps the watched object of *this and another observer_ptr object
  ///        other, by invoking swap on the stored pointers of *this and other.
  ///
  /// \param other Another `ObserverPtr` object to swap the watched object with.
  constexpr void swap(ObserverPtr& other) noexcept {
    std::swap(ptr_, other.ptr_);
  }

  /// @}

  /// @name Observers
  /// @{

  /// \brief Get the pointer to the currently watched object.
  ///
  /// \return A pointer to the watched object or nullptr if no object is
  ///         watched.
  constexpr element_type* get() const noexcept { return ptr_; }

  /// \brief Checks whether *this has an associated watched object.
  ///
  /// \return `true` if *this has an associated watched object, false otherwise.
  constexpr explicit operator bool() const noexcept { return ptr_ != nullptr; }

  /// \brief Dereferences the observed object.
  ///
  /// The behavior of operator* is undefined if get() == nullptr.
  ///
  /// \return The object watched by *this, equivalent to *get().
  constexpr std::add_lvalue_reference<element_type> operator*() const {
    assert(ptr_);

    return *ptr_;
  }

  /// \brief Get the pointer to the watched object.
  ///
  /// \return A pointer to the object watched by *this, i.e. get().
  constexpr element_type* operator->() const { return ptr_; }

  /// @}

  /// \brief Provides an explicit conversion to the type of the stored pointer.
  ///
  /// \return A pointer to the watched object, i.e., get().
  constexpr explicit operator element_type*() const noexcept { return ptr_; }

  template <typename U>
  friend bool operator==(const ObserverPtr<T>& lhs, const ObserverPtr<U>& rhs) {
    return lhs.get() == rhs.get();
  }

  template <typename U>
  friend bool operator!=(const ObserverPtr<T>& lhs, const ObserverPtr<U>& rhs) {
    return !(lhs == rhs);
  }

 private:
  element_type* ptr_{};
};

/// \brief Creates an `ObserverPtr` object, deducing the template argument from
///        the type of the function argument.
///
/// \tparam T The type of the object to be observed.
/// \param p Pointer to the object to be watched by the `ObserverPtr` object.
/// \return A `ObserverPtr` object.
template <typename T>
ObserverPtr<T> make_observer(T* p) noexcept {
  return ObserverPtr{p};
}

template <typename T>
void swap(ObserverPtr<T>& lhs, ObserverPtr<T>& rhs) noexcept {
  lhs.swap(rhs);
}

}  // namespace chibicpp

namespace std {

template <typename T>
struct hash<chibicpp::ObserverPtr<T>> {
  size_t operator()(const chibicpp::ObserverPtr<T>& ptr) const noexcept {
    return hash<typename chibicpp::ObserverPtr<T>::element_type*>()(ptr.get());
  }
};

}  // namespace std
