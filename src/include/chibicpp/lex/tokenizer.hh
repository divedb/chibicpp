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

/// Checks if a character is an alphabetic letter or an underscore.
///
/// This function determines if the given character is an alphabetic letter
/// (a-z, A-Z) or an underscore ('_').
///
/// \param ch The character to check.
/// \return true if the character is an alphabetic letter or an underscore,
///         false otherwise
inline bool is_alpha(int ch) { return std::isalpha(ch) || ch == '_'; }

/// Checks if a character is alphanumeric or an underscore.
///
/// This function determines if the given character is either an alphabetic
/// letter (a-z, A-Z), a digit (0-9), or an underscore ('_').
///
/// \param ch The character to check.
/// \return true if the character is an alphabetic letter, a digit, or an
///         underscore, false otherwise
inline bool is_alnum(int ch) { return is_alpha(ch) || std::isdigit(ch); }

}  // namespace

class Tokenizer {
 public:
  virtual ~Tokenizer() = default;

  /// \return The next token if EOF hasn't been reached; EOF otherwise.
  virtual Token next() = 0;
};

class BasicTokenizer : public Tokenizer {
 public:
  static const std::vector<std::string> kKeywords;

  /// Constructs a tokenizer using the specified content and size.
  ///
  /// And the `content` parameter does not need to be null-terminated.
  ///
  /// \param content A pointer to the content to tokenize.
  /// \param size The size of the content in byte.
  BasicTokenizer(const char* content, size_t size)
      : begin_(content), end_(content + size), pos_(content) {}

  Token next() override {
    while (true) {
      skip_whitespace();

      // Next token is not comment.
      if (!process_comment()) {
        break;
      }
    }

    if (is_eof()) {
      return Token::dummy();
    }

    return parse_token();
  }

 private:
  bool process_comment() {
    const int sz = 2;

    if (advance_if_match("//", sz)) {
      location_.x_pos += sz;
      skip_single_line_comment();

      return true;
    }

    if (advance_if_match("/*", sz)) {
      location_.x_pos += sz;
      skip_multiple_line_comment();

      return true;
    }

    return false;
  }

  /// Checks if the end of the file has been reached.
  ///
  /// Note: If the input content consists only of spaces or newlines,
  /// `is_eof` will return false since the end of the file has not been
  /// reached. However, the `next` method will return an `EOF` token.
  ///
  /// \return true if reached eof; otherwise return `false.
  constexpr bool is_eof() const { return pos_ >= end_; }

  struct LocationGuard {
    LocationGuard(const char* init_pos, const char** init_ppos,
                  SourceLocation& init_loc)
        : pos(init_pos), ppos(init_ppos), location(init_loc) {}

    ~LocationGuard() { location.x_pos += dist(); }

    int dist() const {
      auto new_pos = *ppos;

      return new_pos - pos;
    }

    const char* pos;
    const char** ppos;
    SourceLocation& location;
  };

  /// Check if current position starts with a keyword.
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
  static int index_of_keyword(const char* pos, size_t max_size) {
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
        // If the data is '\0' terminated, we don't need to check `is_eof`
        // here.
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

  void skip_single_line_comment() {
    char ch;

    while (!is_eof() && (ch = *pos_) != '\n') {
      pos_++;
      update_source_location(ch);
    }

    if (!is_eof()) {
      update_source_location(ch);
    }
  }

  void skip_multiple_line_comment() {
    while (!is_eof()) {
      char ch = *pos_++;
      int avail = available();
      update_source_location(ch);

      // Check if the character is `*` and ensure there is at least one
      // following character.
      if (ch == '*' && avail > 0) {
        ch = *pos_;
        update_source_location(ch);

        if (ch == '/') {
          pos_++;

          return;
        }
      }
    }

    CHIBICPP_THROW_ERROR("Unclosed multiple line comment.");
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

  /// Skip the whitespace with side effect.
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
  /// \return true if the content matches `str`; otherwise return false.
  bool advance_if_match(const char* str, size_t len) {
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

  const char* begin_;
  const char* end_;
  const char* pos_;
  SourceLocation location_{};
};

class Lexer {
 public:
  /// When the current position is marked, calling reset will move the cursor
  /// back to this position. A value of `kInvalidMark` indicates that no valid
  /// mark exists.
  static constexpr size_t kInvalidMark = static_cast<size_t>(-1);

  explicit Lexer(const char* content, size_t size)
      : idx_{}, mark_{kInvalidMark} {
    BasicTokenizer tokenizer{content, size};

    while (true) {
      auto tok = tokenizer.next();

      if (tok.kind() == TokenKind::kEOF) {
        break;
      }

      tokens_.push_back(tok);
    }
  }

  /// Mark the current index position for future resetting.
  ///
  /// Saves the current index (`idx_`) to `mark_`, allowing the position
  /// to be restored later by calling `reset()`.
  void mark() { mark_ = idx_; }

  /// Reset the index to the previously marked position. If no valid
  /// mark has been set, it throws an error.
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

  /// Attempt to peek at the current token and check if it matches the
  ///        specified token kind.
  ///
  /// \param kind The expected token kind to match.
  /// \param out_token Output token that will be assigned the token if matched.
  /// \return true if the current token matches the specified kind,
  ///         false otherwise.
  bool try_peek(TokenKind kind, Token& out_token) {
    if (is_eof() || tokens_[idx_].kind() != kind) {
      return false;
    }

    out_token = tokens_[idx_];

    return true;
  }

  bool try_peek(const char* op) { return try_peek(op, Token::dummy()); }

  /// Attempt to peek at the current token and check if it matches a
  /// reserved keyword or punctuator.
  ///
  /// \param op The expected string (keyword or punctuator) to match.
  /// \param out_token Output token that will be assigned the token if
  ///                  matched.
  /// \return true if the current token matches the specified
  ///         string; false otherwise.
  bool try_peek(const char* op, Token& out_token) {
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

  /// @name Consume
  /// @{

  bool try_consume(TokenKind kind, Token& out_token) {
    if (!try_peek(kind, out_token)) {
      return false;
    }

    idx_++;

    return true;
  }

  bool try_consume_identifier(Token& out_token) {
    return try_consume(TokenKind::kIdentifier, out_token);
  }

  /// Try to consume the specified token.
  ///
  /// Tokenizer tries to compare the specified `token` with current token in
  /// buffer, if they are matched, current token will be consumed, otherwise,
  /// nothing happens.
  ///
  /// \param op
  /// \return true if succeed to consume this token, false otherwise
  bool try_consume(const char* op) {
    if (!try_peek(op, Token::dummy())) {
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
      auto err = error(token_kind_to_string(kind));

      CHIBICPP_THROW_ERROR(err);
    }
  }

  void expect(const char* op) {
    if (!try_consume(op)) {
      auto err = error(op);

      CHIBICPP_THROW_ERROR(err);
    }
  }

  void expect_number(Token& token) { expect(TokenKind::kNum, token); }

  /// Expect next token is an identifier. If it is, this token will be
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

  void dump_remain_tokens(std::ostream& os) {
    if (is_eof()) {
      os << "empty.\n";
    } else {
      for (auto i = idx_; i < tokens_.size(); ++i) {
        os << tokens_[i] << '\n';
      }
    }
  }

  Token next() {
    if (is_eof()) {
      return Token::dummy();
    }

    return tokens_[idx_++];
  }

 private:
  std::string error(const std::string& expect) {
    std::ostringstream oss;

    oss << "Expect " << std::quoted(expect) << " but got ";

    if (is_eof()) {
      oss << "EOF";
    } else {
      oss << tokens_[idx_];
    }

    return oss.str();
  }

  size_t idx_;
  size_t mark_;
  std::vector<Token> tokens_;
};

}  // namespace chibicpp