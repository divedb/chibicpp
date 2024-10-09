//===-- llvm/ADT/bit.h - C++20 <bit> ----------------------------*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
///
/// \file
/// This file implements the C++20 <bit> header.
///
//===----------------------------------------------------------------------===//

#pragma once

#include <type_traits>

namespace chibicpp {

enum class Endianness {
  big,
  little,
#if defined(BYTE_ORDER) && defined(BIG_ENDIAN) && BYTE_ORDER == BIG_ENDIAN
  native = big
#else
  native = little
#endif
};

// This implementation of bit_cast is different from the C++20 one in two ways:
//  - It isn't constexpr because that requires compiler support.
//  - It requires trivially-constructible To, to avoid UB in the implementation.
template <
    typename To, typename From,
    typename = std::enable_if_t<sizeof(To) == sizeof(From)>,
    typename = std::enable_if_t<std::is_trivially_constructible<To>::value>,
    typename = std::enable_if_t<std::is_trivially_copyable<To>::value>,
    typename = std::enable_if_t<std::is_trivially_copyable<From>::value>>
[[nodiscard]] inline To bit_cast(const From& from) noexcept {
#if __has_builtin(__builtin_bit_cast)
  return __builtin_bit_cast(To, from);
#else
  To to;
  std::memcpy(&to, &from, sizeof(To));
  return to;
#endif
}

/// Reverses the bytes in the given integer value V.
template <typename T, typename = std::enable_if_t<std::is_integral_v<T>>>
[[nodiscard]] constexpr T byteswap(T v) noexcept {
  if constexpr (sizeof(T) == 1) {
    return v;
  } else if constexpr (sizeof(T) == 2) {
    uint16_t uv = v;
#if defined(_MSC_VER) && !defined(_DEBUG)
    // The DLL version of the runtime lacks these functions (bug!?), but in a
    // release build they're replaced with BSWAP instructions anyway.
    return _byteswap_ushort(uv);
#else
    uint16_t hi = uv << 8;
    uint16_t lo = uv >> 8;
    return hi | lo;
#endif
  } else if constexpr (sizeof(T) == 4) {
    uint32_t uv = v;
#if __has_builtin(__builtin_bswap32)
    return __builtin_bswap32(uv);
#elif defined(_MSC_VER) && !defined(_DEBUG)
    return _byteswap_ulong(uv);
#else
    uint32_t byte0 = uv & 0x000000FF;
    uint32_t byte1 = uv & 0x0000FF00;
    uint32_t Byte2 = uv & 0x00FF0000;
    uint32_t byte3 = uv & 0xFF000000;
    return (byte0 << 24) | (byte1 << 8) | (byte2 >> 8) | (byte3 >> 24);
#endif
  } else if constexpr (sizeof(T) == 8) {
    uint64_t uv = v;
#if __has_builtin(__builtin_bswap64)
    return __builtin_bswap64(uv);
#elif defined(_MSC_VER) && !defined(_DEBUG)
    return _byteswap_uint64(uv);
#else
    uint64_t hi = chibicpp::byteswap<uint32_t>(uv);
    uint32_t lo = chibicpp::byteswap<uint32_t>(uv >> 32);
    return (hi << 32) | lo;
#endif
  } else {
    static_assert(!sizeof(T*), "Don't know how to handle the given type.");

    return 0;
  }
}

template <typename T, typename = std::enable_if_t<std::is_unsigned_v<T>>>
[[nodiscard]] constexpr inline bool has_single_bit(T value) noexcept {
  return (value != 0) && ((value & (value - 1)) == 0);
}

namespace detail {

template <typename T, std::size_t SizeOfT>
struct TrailingZerosCounter {
  static unsigned count(T val) {
    if (!val) return std::numeric_limits<T>::digits;
    if (val & 0x1) return 0;

    // Bisection method.
    unsigned zero_bits = 0;
    T shift = std::numeric_limits<T>::digits >> 1;
    T mask = std::numeric_limits<T>::max() >> shift;

    while (shift) {
      if ((val & mask) == 0) {
        val >>= shift;
        zero_bits |= shift;
      }

      shift >>= 1;
      mask >>= shift;
    }

    return zero_bits;
  }
};

#if defined(__GNUC__) || defined(_MSC_VER)
template <typename T>
struct TrailingZerosCounter<T, 4> {
  static unsigned count(T val) {
    if (val == 0) return 32;

#if __has_builtin(__builtin_ctz) || defined(__GNUC__)
    return __builtin_ctz(val);
#elif defined(_MSC_VER)
    unsigned long Index;
    _BitScanForward(&Index, val);
    return Index;
#endif
  }
};

#if !defined(_MSC_VER) || defined(_M_X64)
template <typename T>
struct TrailingZerosCounter<T, 8> {
  static unsigned count(T val) {
    if (val == 0) return 64;

#if __has_builtin(__builtin_ctzll) || defined(__GNUC__)
    return __builtin_ctzll(val);
#elif defined(_MSC_VER)
    unsigned long Index;
    _BitScanForward64(&Index, val);
    return Index;
#endif
  }
};
#endif

#endif

}  // namespace detail

/// Count number of 0's from the least significant bit to the most
///   stopping at the first 1.
///
/// Only unsigned integral types are allowed.
///
/// Returns std::numeric_limits<T>::digits on an input of 0.
template <typename T>
[[nodiscard]] int countr_zero(T Val) {
  static_assert(std::is_unsigned_v<T>,
                "Only unsigned integral types are allowed.");
  return chibicpp::detail::TrailingZerosCounter<T, sizeof(T)>::count(Val);
}

namespace detail {
template <typename T, std::size_t SizeOfT>
struct LeadingZerosCounter {
  static unsigned count(T Val) {
    if (!Val) return std::numeric_limits<T>::digits;

    // Bisection method.
    unsigned ZeroBits = 0;
    for (T Shift = std::numeric_limits<T>::digits >> 1; Shift; Shift >>= 1) {
      T Tmp = Val >> Shift;
      if (Tmp)
        Val = Tmp;
      else
        ZeroBits |= Shift;
    }
    return ZeroBits;
  }
};

#if defined(__GNUC__) || defined(_MSC_VER)
template <typename T>
struct LeadingZerosCounter<T, 4> {
  static unsigned count(T Val) {
    if (Val == 0) return 32;

#if __has_builtin(__builtin_clz) || defined(__GNUC__)
    return __builtin_clz(Val);
#elif defined(_MSC_VER)
    unsigned long Index;
    _BitScanReverse(&Index, Val);
    return Index ^ 31;
#endif
  }
};

#if !defined(_MSC_VER) || defined(_M_X64)
template <typename T>
struct LeadingZerosCounter<T, 8> {
  static unsigned count(T Val) {
    if (Val == 0) return 64;

#if __has_builtin(__builtin_clzll) || defined(__GNUC__)
    return __builtin_clzll(Val);
#elif defined(_MSC_VER)
    unsigned long Index;
    _BitScanReverse64(&Index, Val);
    return Index ^ 63;
#endif
  }
};
#endif
#endif
}  // namespace detail

}  // namespace chibicpp