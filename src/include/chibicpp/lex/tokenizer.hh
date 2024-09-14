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

static bool is_alpha(int ch) { return std::isalpha(ch) || ch == '_'; }
static bool is_alnum(int ch) { return is_alpha(ch) || std::isdigit(ch); }

class Tokenizer {
 public:
  static std::vector<std::string> kKeywords;

  explicit Tokenizer(char const* content, size_t size)
      : begin_(content), end_(content + size), pos_(content) {}

  /// Get next token.
  ///
  /// \return Type of `kEOF` token if eof has been reached, otherwise next
  ///         token.
  Token next() {
    if (!is_eof()) {
      skip_whitespace();

      return internal_token();
    }

    return Token{location_};
  }

  constexpr bool is_eof() const { return pos_ >= end_; }

 private:
  Token internal_token() {
    if (is_eof()) {
      return Token{location_};
    }

    auto old_pos = pos_;
    auto old_loc = location_;
    TokenKind kind = parse_token_kind();

    if (kind == TokenKind::kNum) {
      /// TODO(gc): handle errno if failed to parse
      char* end_ptr;
      int64_t v = std::strtol(old_pos, &end_ptr, /*base*/ 10);
      pos_ = end_ptr;

      return Token{kind, old_loc, v};
    } else {
      return Token{kind, old_loc, old_pos, static_cast<size_t>(pos_ - old_pos)};
    }
  }

  /// \brief Check if current position starts with a keyword.
  ///
  /// This function checks if the current position (`pos_`) in the input matches
  /// any of the keywords stored in the `kKeywords` vector.
  ///
  /// \return The index of the keyword if it exists, otherwise return -1.
  int index_of_keyword() const {
    /// Check if it's a keyword.
    auto iter = std::find_if(
        kKeywords.begin(), kKeywords.end(), [this](auto const& str) {
          if (this->available() < str.size()) {
            return false;
          }

          return std::equal(str.begin(), str.end(), this->pos_,
                            this->pos_ + str.size());
        });

    if (iter == kKeywords.end()) {
      return -1;
    }

    return std::distance(kKeywords.begin(), iter);
  }

  TokenKind parse_token_kind() {
    auto old_pos = pos_;
    char ch = *pos_++;
    TokenKind kind = TokenKind::kEOF;

    if (std::isdigit(ch)) {
      kind = TokenKind::kNum;

    } else if (std::ispunct(ch)) {
      /// 041 ``!'' 042 ``"'' 043 ``#'' 044 ``$'' 045 ``%''
      /// 046 ``&'' 047 ``''' 050 ``('' 051 ``)'' 052 ``*''
      /// 053 ``+'' 054 ``,'' 055 ``-'' 056 ``.'' 057 ``/''
      /// 072 ``:'' 073 ``;'' 074 ``<'' 075 ``='' 076 ``>''
      /// 077 ``?'' 100 ``@'' 133 ``['' 134 ``\'' 135 ``]''
      /// 136 ``^'' 137 ``_'' 140 ```'' 173 ``{'' 174 ``|''
      /// 175 ``}'' 176 ``~''

      /// TODO(gc): Do we need to remove `$`, `@`?
      kind = TokenKind::kReserved;

      switch (ch) {
        case '.':
          (void)advance_if_match("..", 2);
          break;

        case '-':
          /// Continue to verify next character, greedy match.
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
          break;
      }
    } else if (is_alpha(ch)) {
      /// Step back one character and Check if it's a keyword at current
      /// position.
      pos_--;
      int index = index_of_keyword();

      if (index != -1) {
        kind = TokenKind::kReserved;
        auto len = kKeywords[index].size();
        pos_ += len;
      } else {
        kind = TokenKind::kIdentifier;

        while (!is_eof() && is_alnum(*pos_)) {
          pos_++;
        }
      }
    } else {
      pos_--;

      /// Meet an unknown character.
      CHIBICPP_THROW_ERROR("invalid token", ch);
    }

    /// Update the source location.
    size_t consumed = pos_ - old_pos;
    location_.x_pos += consumed;

    return kind;
  }

  /// Skip the whitespace.
  /// But the side effect is that we also update the location.
  /// TODO(gc): Is any cleaner way to do it?
  void skip_whitespace() {
    char ch;

    while (!is_eof() && std::isspace(ch = *pos_)) {
      if (ch == '\n') {
        location_.x_pos = 1;
        location_.y_pos++;
      } else {
        location_.x_pos++;
      }

      pos_++;
    }
  }

  /// Compare the specified `str` with the content starting from `pos_`. If the
  /// content matches `str`, advance the `pos_` pointer by `len`.
  ///
  /// @param str A pointer to string.
  /// @param len Size of the string.
  /// @return `true` if the content matches `str`; otherwise return `false`.
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
  explicit Lexer(char const* content, size_t size) : idx_(0) {
    Tokenizer tok(content, size);

    while (!tok.is_eof()) {
      tokens_.push_back(tok.next());
    }
  }

  bool try_peek(char const* op, Token& out_token) {
    if (is_eof()) {
      return false;
    }

    Token token = tokens_[idx_];
    TokenKind kind = token.kind();

    // Keywords or punctuators.
    if (kind != TokenKind::kReserved) {
      return false;
    }

    auto str = token.as_str();

    // If size or content is not matched.
    if (strlen(op) != str.size() ||
        std::strncmp(op, str.data(), str.size()) != 0) {
      return false;
    }

    out_token = token;

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
    Token dummy;
    auto succeed = try_peek(op, dummy);

    if (succeed) {
      idx_++;
    }

    return succeed;
  }

  /// \brief Try to consume next token and expect it's an identifier.
  ///
  /// \return
  bool try_consume_identifier(Token& token) {
    if (is_eof() || tokens_[idx_].kind() != TokenKind::kIdentifier) {
      return false;
    }

    token = tokens_[idx_++];

    return true;
  }

  void expect(char const* op) {
    if (!try_consume(op)) {
      CHIBICPP_THROW_ERROR("Expected ", std::quoted(op));
    }
  }

  void expect_number(Token& token) { expect(token, TokenKind::kNum); }

  /// \brief Expect next token is an identifier. If it is, this token will be
  ///        consumed, otherwise an exception will be thrown.
  ///
  /// \param token
  void expect_identider(Token& token) { expect(token, TokenKind::kIdentifier); }

  /* constexpr */ bool is_eof() const { return idx_ >= tokens_.size(); }

  auto begin() { return tokens_.begin(); }
  auto end() { return tokens_.end(); }
  auto begin() const { return tokens_.begin(); }
  auto end() const { return tokens_.end(); }

 private:
  void expect(Token& token, TokenKind kind) {
    if (is_eof() || tokens_[idx_].kind() != kind) {
      std::string error =
          "Expect a " + token_kind_to_string(kind) + " but got a ";

      if (is_eof()) {
        error += "EOF";
      } else {
        error += token_kind_to_string(tokens_[idx_].kind());
      }

      CHIBICPP_THROW_ERROR(error);
    }

    token = tokens_[idx_++];
  }

  size_t idx_;
  std::vector<Token> tokens_;
};

}  // namespace chibicpp