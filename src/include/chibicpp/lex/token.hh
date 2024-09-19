#pragma once

#include <cstring>
#include <iomanip>
#include <iostream>
#include <string>
#include <tuple>
#include <variant>

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
  kStrLiteral,  ///< String Literal.
  kNum,         ///< Integer literals.

  kEOF,
};

inline std::string token_kind_to_string(TokenKind kind) {
  if (kind == TokenKind::kReserved) {
    return "KEYWORD | PUNCTUATOR";
  } else if (kind == TokenKind::kIdentifier) {
    return "IDENTIFIER";
  } else if (kind == TokenKind::kStrLiteral) {
    return "STRING LITERAL";
  } else if (kind == TokenKind::kNum) {
    return "NUMBER";
  } else if (kind == TokenKind::kEOF) {
    return "EOF";
  } else {
    return "UNKOWN";
  }
}

struct SourceLocation {
  int x_pos{1};
  int y_pos{1};

  friend std::ostream& operator<<(std::ostream& os, SourceLocation loc) {
    return os << loc.y_pos << ':' << loc.x_pos;
  }
};

class Token {
 public:
  /// \brief A utility method to provide a dummy eof token.
  ///
  /// \return A token with type of EOF.
  static Token& dummy_eof() {
    static Token token;

    return token;
  }

  /// @name Constructors.
  /// @{

  /// \brief Construct a EOF token.
  Token() : kind_(TokenKind::kEOF) {}

  /// \brief Constructs a token with a string value.
  ///
  /// This constructor initializes a token with the specified kind, source
  /// location, and a string.
  ///
  /// \param kind The kind of the token.
  /// \param location The source location of the token.
  /// \param str A string.
  Token(TokenKind kind, SourceLocation location, std::string const& str)
      : kind_{kind}, location_{location}, data_{str} {}

  /// \brief Constructs a token with a 64-bit integer value.
  ///
  /// \param kind The kind of the token.
  /// \param location The source location of the token.
  /// \param i64 The 64-bit integer value to be associated with the token.
  Token(TokenKind kind, SourceLocation location, int64_t i64)
      : kind_{kind}, location_{location}, data_{i64} {}

  /// \brief Constructs a token with a 64-bit floating-point value.
  ///
  /// \param kind The kind of the token.
  /// \param location The source location of the token.
  /// \param f64 The 64-bit floating-point value to be associated with the
  ///            token.
  Token(TokenKind kind, SourceLocation location, double f64)
      : kind_{kind}, location_{location}, data_{f64} {}

  /// @}

  /// \brief Get the kind of token.
  ///
  /// \return Token kind.
  constexpr TokenKind kind() const { return kind_; }

  /// \brief Get the source location of the token.
  ///
  /// \return Source location of the token.
  constexpr SourceLocation location() const { return location_; }

  /// \brief Get the token's string representation as a C-string along with its
  ///        length.
  ///
  /// Note: The C-string data inside the returned tuple shares the same lifetime
  ///       as this token, meaning it remains valid as long as the token is not
  ///       modified or destroyed.
  ///
  /// \return A tuple containing the C-string and its length.
  std::tuple<char const*, int> as_cstr() const {
    auto const& str = std::get<std::string>(data_);

    return {str.data(), str.length()};
  }

  /// \brief Get the token's string representation as an `std::string`.
  ///
  /// \return A string representation of the token.
  std::string const& as_str() const { return std::get<std::string>(data_); }

  /// \brief Get the token's value as an `int64_t`.
  ///
  /// \return An integer with type `int64_t`.
  int64_t as_i64() const { return std::get<int64_t>(data_); }

  /// \brief Get the token's value as a `double`.
  ///
  /// \return A number with type `double`.
  double as_f64() const { return std::get<double>(data_); }

  friend std::ostream& operator<<(std::ostream& os, Token const& token) {
    auto kind = token.kind();
    auto location = token.location();

    if (kind == TokenKind::kEOF) {
      return os << "EOF";
    }

    os << '[' << location.y_pos << ':' << location.x_pos << ']' << ':';

    switch (kind) {
      case TokenKind::kNum:
        os << token.as_i64();
        break;
      default:
        os << std::quoted(token.as_str());
        break;
    }

    os << " => " << token_kind_to_string(kind);

    return os;
  }

 private:
  TokenKind kind_;
  SourceLocation location_;

  std::variant<std::string, int64_t, double> data_;
};

}  // namespace chibicpp