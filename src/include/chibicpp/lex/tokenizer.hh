#pragma once

#include <algorithm>
#include <cstring>
#include <iomanip>
#include <iostream>
#include <string>
#include <string_view>
#include <vector>

#include "chibicpp/common/error.hh"
#include "chibicpp/lex/token.hh"

using namespace std::literals;

namespace chibicpp {

namespace {

/// \brief Checks if a character is an alphabetic letter or an underscore.
///
/// This function determines if the given character is an alphabetic letter
/// (a-z, A-Z) or an underscore ('_').
///
/// \param ch The character to check.
/// \return `true` if the character is an alphabetic letter or an underscore,
///         otherwise `false`.
bool is_alpha(int ch) { return std::isalpha(ch) || ch == '_'; }

/// \brief Checks if a character is alphanumeric or an underscore.
///
/// This function determines if the given character is either an alphabetic
/// letter (a-z, A-Z), a digit (0-9), or an underscore ('_').
///
/// \param ch The character to check.
/// \return `true` if the character is an alphabetic letter, a digit, or an
///         underscore, otherwise `false`.
bool is_alnum(int ch) { return is_alpha(ch) || std::isdigit(ch); }

}  // namespace

class Tokenizer {
 public:
  static const std::vector<std::string> kKeywords;

  explicit Tokenizer(char const* content, size_t size)
      : begin_(content), end_(content + size), pos_(content) {}

  /// Get next token.
  ///
  /// \return Type of `kEOF` token if eof has been reached, otherwise next
  ///         token.
  Token next() {
    skip_whitespace();

    if (is_eof()) {
      return Token::dummy_eof();
    }

    return parse_token();
  }

  constexpr bool is_eof() const { return pos_ >= end_; }

 private:
  struct LocationGuard {
    LocationGuard(char const* init_pos, char const** init_ppos,
                  SourceLocation& init_loc)
        : pos(init_pos), ppos(init_ppos), location(init_loc) {}

    ~LocationGuard() { location.x_pos += dist(); }

    int dist() const {
      auto new_pos = *ppos;

      return new_pos - pos;
    }

    char const* pos;
    char const** ppos;
    SourceLocation& location;
  };

  /// \brief Check if current position starts with a keyword.
  ///
  /// This function checks if the current position (`pos`) in the input matches
  /// any of the keywords stored in the `kKeywords` vector.
  ///
  /// Note: The `max_size` defines the maximum length of the string to be
  /// compared with the keywords.
  ///
  /// \param pos The starting position of the string to compare.
  /// \param max_size The maximum length of the string to check.
  /// \return The index of the keyword if it exists, otherwise return -1.
  static int index_of_keyword(char const* pos, size_t max_size) {
    auto iter = std::find_if(kKeywords.begin(), kKeywords.end(),
                             [pos, max_size](auto const& str) {
                               if (max_size < str.size()) {
                                 return false;
                               }

                               return std::equal(str.begin(), str.end(), pos);
                             });

    if (iter == kKeywords.end()) {
      return -1;
    }

    return std::distance(kKeywords.begin(), iter);
  }

  Token parse_number() {
    char* end_ptr;
    LocationGuard guard{pos_, &pos_, location_};
    int64_t v = std::strtol(pos_, &end_ptr, /*base*/ 10);
    pos_ = end_ptr;

    return {TokenKind::kNum, guard.location, v};
  }

  static char parse_escape_char(char ch) {
    switch (ch) {
      case 'a':
        return '\a';
      case 'b':
        return '\b';
      case 't':
        return '\t';
      case 'n':
        return '\n';
      case 'v':
        return '\v';
      case 'f':
        return '\f';
      case 'r':
        // ESC
        // \e[32mHello, World!\e[0m
        return '\r';
      case 'e':
        return 27;
      case '0':
        return 0;
      default:
        return ch;
    }
  }

  Token parse_string_literal() {
    char ch;
    std::string buf;
    LocationGuard guard{pos_, &pos_, location_};

    // Skip open question mark.
    ++pos_;

    // TODO(gc): Do we need to limit the maximum length of string literal?
    while (!is_eof() && (ch = *pos_) != '"') {
      if (ch == '\\') {
        // This depends on the given data passed inside constructor.
        // If the data is '\0' terminated, we don't need to check `is_eof` here.
        if (is_eof()) {
          break;
        }

        // Read next character.
        ch = *++pos_;
        buf += parse_escape_char(ch);
      } else {
        buf += ch;
      }

      pos_++;
    }

    if (is_eof()) {
      CHIBICPP_THROW_ERROR("Unclosed string literal", " @ ", location_);
    }

    // Skip closed question mark.
    pos_++;

    return {TokenKind::kStrLiteral, guard.location, buf};
  }

  Token parse_reserved() {
    // 041 ``!'' 042 ``"'' 043 ``#'' 044 ``$'' 045 ``%''
    // 046 ``&'' 047 ``''' 050 ``('' 051 ``)'' 052 ``*''
    // 053 ``+'' 054 ``,'' 055 ``-'' 056 ``.'' 057 ``/''
    // 072 ``:'' 073 ``;'' 074 ``<'' 075 ``='' 076 ``>''
    // 077 ``?'' 100 ``@'' 133 ``['' 134 ``\'' 135 ``]''
    // 136 ``^'' 137 ``_'' 140 ```'' 173 ``{'' 174 ``|''
    // 175 ``}'' 176 ``~''
    char ch = *pos_;
    LocationGuard guard{pos_, &pos_, location_};

    pos_++;

    switch (ch) {
      case '.':
        (void)advance_if_match("..", 2);
        break;

      case '-':
        // Continue to verify next character, greedy match.
        // Valid cases:
        // --
        // ->
        // -=
        for (auto ch : "->="sv) {
          if (advance_if_match(ch)) {
            break;
          }
        }
        break;

      case '+':
        for (auto ch : "+="sv) {
          if (advance_if_match(ch)) {
            break;
          }
        }
        break;

      case '&':
        for (auto ch : "&="sv) {
          if (advance_if_match(ch)) {
            break;
          }
        }

        break;

      case '|':
        for (auto ch : "|="sv) {
          if (advance_if_match(ch)) {
            break;
          }
        }

        break;

      case '<':
        for (auto str : {"<=", "<", "="}) {
          if (advance_if_match(str, std::strlen(str))) {
            break;
          }
        }

        break;

      case '>':
        for (auto str : {">=", ">", "="}) {
          if (advance_if_match(str, std::strlen(str))) {
            break;
          }
        }

        break;

      case '#':
        (void)advance_if_match('#');

        break;

      case '!':
      case '*':
      case '/':
      case '%':
      case '^':
      case '=':
        (void)advance_if_match('=');
        break;

      default:
        // Possible characters that reach this point: (), {}, [], etc.
        break;
    }

    std::string buf{guard.pos, static_cast<size_t>(guard.dist())};

    return {TokenKind::kReserved, guard.location, buf};
  }

  Token parse_identifier() {
    LocationGuard guard{pos_, &pos_, location_};
    int index = index_of_keyword(pos_, available());

    if (index != -1) {
      pos_ += kKeywords[index].size();

      return {TokenKind::kReserved, guard.location, kKeywords[index]};
    } else {
      char ch;
      std::string buf;

      while (!is_eof() && is_alnum((ch = *pos_))) {
        buf += ch;
        pos_++;
      }

      return {TokenKind::kIdentifier, guard.location, buf};
    }
  }

  Token parse_token() {
    char ch = *pos_;

    if (std::isdigit(ch)) {
      return parse_number();
    } else if (ch == '"') {
      return parse_string_literal();
    } else if (std::ispunct(ch)) {
      return parse_reserved();
    } else if (is_alpha(ch)) {
      return parse_identifier();
    } else {
      // Encounter an unknown character.
      CHIBICPP_THROW_ERROR("Invalid token: ", ch, " @ ", location_);

      __builtin_unreachable();
    }
  }

  /// @brief Skip the whitespace with side effect.
  ///
  /// Note: The side effect is to update the location.
  void skip_whitespace() {
    char ch;

    while (!is_eof() && std::isspace(ch = *pos_)) {
      update_source_location(ch);
      pos_++;
    }
  }

  void update_source_location(char ch) {
    if (ch == '\n') {
      location_.x_pos = 1;
      location_.y_pos++;
    } else {
      location_.x_pos++;
    }
  }

  /// Compare the specified `str` with the content starting from `pos_`. If
  /// the content matches `str`, advance the `pos_` pointer by `len`.
  ///
  /// \param str A pointer to string.
  /// \param len Size of the string.
  /// \return `true` if the content matches `str`; otherwise return `false`.
  bool advance_if_match(char const* str, size_t len) {
    if (available() < len || std::strncmp(pos_, str, len) != 0) {
      return false;
    }

    pos_ += len;

    return true;
  }

  bool advance_if_match(char ch) {
    if (available() == 0 || *pos_ != ch) {
      return false;
    }

    pos_++;

    return true;
  }

  /// The number of available bytes in memory buffer.
  ///
  /// \return Avaiable bytes.
  size_t available() const { return end_ - pos_; }

  char const* begin_;
  char const* end_;
  char const* pos_;
  SourceLocation location_{};
};

/// \brief Extracts all the tokens from the specified memory
///        buffer.
class Lexer {
 public:
  static constexpr size_t kInvalidMark = static_cast<size_t>(-1);

  explicit Lexer(char const* content, size_t size)
      : idx_(0), mark_(kInvalidMark) {
    Tokenizer tok(content, size);

    while (!tok.is_eof()) {
      tokens_.push_back(tok.next());
    }
  }

  /// \brief Mark the current index position for future resetting.
  ///
  /// Saves the current index (`idx_`) to `mark_`, allowing the position
  /// to be restored later by calling `reset()`.
  void mark() { mark_ = idx_; }

  /// \brief Reset the index to the previously marked position. If no valid
  /// mark
  ///        has been set, it throws an error.
  ///
  /// \throws Error if no mark has been set (mark_ is equal to kInvalidMark).
  void reset() {
    if (mark_ == kInvalidMark) {
      CHIBICPP_THROW_ERROR("Invalid mark.");
    }

    idx_ = mark_;
  }

  /// @name Peek.
  /// @{

  /// \brief Attempt to peek at the current token and check if it matches the
  ///        specified token kind.
  ///
  /// \param kind The expected token kind to match.
  /// \param out_token Output token that will be assigned the token if
  /// matched. \return `true` if the current token matches the specified kind,
  /// otherwise
  ///         `false`.
  bool try_peek(TokenKind kind, Token& out_token) {
    if (is_eof() || tokens_[idx_].kind() != kind) {
      return false;
    }

    out_token = tokens_[idx_];

    return true;
  }

  /// \brief Attempt to peek at the current token and check if it matches a
  ///        reserved keyword or punctuator.
  ///
  /// \param op The expected string (keyword or punctuator) to match.
  /// \param out_token Output token that will be assigned the token if
  /// matched. \return `true` if the current token matches the specified
  /// string,
  ///         otherwise `false`.
  bool try_peek(char const* op, Token& out_token) {
    if (!try_peek(TokenKind::kReserved, out_token)) {
      return false;
    }

    auto str = out_token.as_str();

    // If size or content is not matched.
    if (strlen(op) != str.size() ||
        std::strncmp(op, str.data(), str.size()) != 0) {
      return false;
    }

    return true;
  }

  /// @}

  /// @name Consume.
  /// @{

  bool try_consume(TokenKind kind, Token& out_token) {
    if (!try_peek(kind, out_token)) {
      return false;
    }

    idx_++;

    return true;
  }

  /// \brief Try to consume the specified token.
  ///
  /// Tokenizer tries to compare the specified `token` with current token in
  /// buffer, if they are matched, current token will be consumed, otherwise,
  /// nothing happens.
  ///
  /// \param op
  /// \return `true` if succeed to consume this token, otherwise `false`.
  bool try_consume(char const* op) {
    if (!try_peek(op, Token::dummy_eof())) {
      return false;
    }

    idx_++;

    return true;
  }

  /// @}

  /// @name Expect.
  /// @{

  void expect(TokenKind kind, Token& out_token) {
    if (!try_consume(kind, out_token)) {
      std::string err = "Expect " + token_kind_to_string(kind) + " but got ";

      if (is_eof()) {
        err += "EOF";
      } else {
        err += token_kind_to_string(tokens_[idx_].kind());
      }

      CHIBICPP_THROW_ERROR(err);
    }
  }

  void expect(char const* op) {
    if (!try_consume(op)) {
      CHIBICPP_THROW_ERROR("Expect ", std::quoted(op));
    }
  }

  void expect_number(Token& token) { expect(TokenKind::kNum, token); }

  /// \brief Expect next token is an identifier. If it is, this token will be
  ///        consumed, otherwise an exception will be thrown.
  ///
  /// \param token
  void expect_identider(Token& token) { expect(TokenKind::kIdentifier, token); }

  /// @}

  /* constexpr */ bool is_eof() const { return idx_ >= tokens_.size(); }

  /// @name Iterators.
  /// @{

  auto begin() { return tokens_.begin(); }
  auto end() { return tokens_.end(); }
  auto begin() const { return tokens_.begin(); }
  auto end() const { return tokens_.end(); }

  /// @}

 private:
  size_t idx_;
  size_t mark_;
  std::vector<Token> tokens_;
};

}  // namespace chibicpp