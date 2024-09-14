#pragma once

#include <cstring>
#include <iomanip>
#include <iostream>
#include <string>

namespace chibicpp {

/// Reference
/// https://en.cppreference.com/w/cpp/language/translation_phases
///
/// Translation phases
/// Phase 1: Mapping source characters
/// Phase 2: Splicing lines
///   1).If the first translation character is byte order mark (U+FEFF), it is
///      deleted. (since C++23)Whenever backslash (\) appears at the end of a
///      line (immediately followed by zero or more whitespace characters other
///      than new-line followed by(since C++23) the newline character), these
///      characters are deleted, combining two physical source lines into one
///      logical source line. This is a single-pass operation; a line ending in
///      two backslashes followed by an empty line does not combine three lines
///      into one.
///   2).If a non-empty source file does not end with a newline character after
///      this step (end-of-line backslashes are no longer splices at this
///      point), a terminating newline character is added.
/// Phase 3: Lexing
/// Phase 4: Preprocessing
///   1) The preprocessor is executed.
///   2) Each file introduced with the #include directive goes through phases 1
///      through 4, recursively.
///   3) At the end of this phase, all preprocessor directives are removed from
///      the source.
/// Phase 5: Determining common string literal encodings
/// Phase 6: Concatenating string literals
/// Phase 7: Compiling
///   Compilation takes place: each preprocessing token is converted to a token.
///   The tokens are syntactically and semantically analyzed and translated as a
///   translation unit.
/// Phase 8: Instantiating templates (For C++)
/// Phase 9: Linking

/// 1. Keywords
/// 2. Identifiers
/// 3. Universal character names
/// 4. Constants
/// 5. String literals
/// 6. Punctuators
/// 7. Header names
/// 8. Preprocessing numbers
/// 9. Comments
enum class TokenKind {
  kReserved,    ///< Keywords or punctuators.
  kIdentifier,  ///< Identifiers.
  kNum,         ///< Integer literals.

  kEOF,
};

inline std::string token_kind_to_string(TokenKind kind) {
  if (kind == TokenKind::kReserved) {
    return "Keyword or Punctuator";
  } else if (kind == TokenKind::kIdentifier) {
    return "Identifier";
  } else if (kind == TokenKind::kNum) {
    return "Number";
  } else if (kind == TokenKind::kEOF) {
    return "EOF";
  } else {
    return "Unknown";
  }
}

struct SourceLocation {
  int x_pos{1};
  int y_pos{1};
};

struct Token {
 public:
  /// @name
  /// Constructors.

  /// @{

  Token() : kind_(TokenKind::kEOF) {}

  explicit Token(SourceLocation location) : Token(TokenKind::kEOF, location) {}

  Token(TokenKind kind, SourceLocation location, char const* str, size_t len)
      : Token(kind, location) {
    u_.str.data = str;
    u_.str.len = len;
  }

  Token(TokenKind kind, SourceLocation location, int64_t i64)
      : Token(kind, location) {
    u_.i64 = i64;
  }

  Token(TokenKind kind, SourceLocation location, double f64)
      : Token(kind, location) {
    u_.f64 = f64;
  }

  Token(TokenKind kind, SourceLocation location)
      : kind_(kind), location_(location) {}

  /// @}

  constexpr TokenKind kind() const { return kind_; }
  constexpr SourceLocation location() const { return location_; }
  char const* as_cstr(int& len) const {
    len = u_.str.len;

    return u_.str.data;
  }
  std::string as_str() const { return std::string{u_.str.data, u_.str.len}; }
  int64_t as_i64() const { return u_.i64; }
  double as_f64() const { return u_.f64; }

  friend std::ostream& operator<<(std::ostream& os, Token const& token) {
    auto kind = token.kind();
    auto location = token.location();

    os << location.y_pos << ':' << location.x_pos << ':';

    switch (kind) {
      case TokenKind::kNum:
        os << token.as_i64();
        break;
      case TokenKind::kEOF:
        os << "EOF";
        break;
      default:
        os << std::quoted(token.as_str());
        break;
    }

    return os;
  }

  friend bool operator==(Token const& lhs, Token const& rhs) {
    if (lhs.kind() != rhs.kind()) {
      return false;
    }

    auto kind = lhs.kind();

    if (kind == TokenKind::kNum) {
      return lhs.as_i64() == rhs.as_i64();
    }

    return lhs.u_.str.len == rhs.u_.str.len &&
           std::strncmp(lhs.u_.str.data, rhs.u_.str.data, rhs.u_.str.len) == 0;
  }

  friend bool operator!=(Token const& lhs, Token const& rhs) {
    return !(lhs == rhs);
  }

 private:
  TokenKind kind_;
  SourceLocation location_;

  // The reason we don't use variant is that
  // each member of the union is trivial.
  // And variant has an extra index member to indicate current data type.
  union {
    struct {
      char const* data;
      size_t len;
    } str;
    int64_t i64;
    double f64;
  } u_;
};

}  // namespace chibicpp