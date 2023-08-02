#pragma once

#include "Token.hpp"

#include <fstream>
#include <iostream>
#include <map>
#include <sstream>
#include <stack>
#include <unicode/uchar.h>
#include <unicode/uniset.h>
#include <unicode/utf8.h>

namespace qasm3 {
class Scanner {
private:
  std::istream& is;
  std::map<icu::UnicodeString, Token::Kind, std::less<>> keywords{};
  UChar32 ch = 0;
  size_t line = 1;
  size_t col = 0;

  icu::UnicodeSet spaceSet;
  icu::UnicodeSet firstIdCharSet;
  icu::UnicodeSet numSet;
  icu::UnicodeSet hexSet;

  [[nodiscard]] bool isSpace(const UChar32 c) const {
    return spaceSet.contains(c) != 0;
  }

  [[nodiscard]] bool isFirstIdChar(const UChar32 c) const {
    return firstIdCharSet.contains(c) != 0;
  }

  [[nodiscard]] bool isNum(const UChar32 c) const {
    return numSet.contains(c) != 0;
  }

  [[nodiscard]] bool isHex(const UChar32 c) const {
    return hexSet.contains(c) != 0;
  }

  static UChar32 readUtf8Codepoint(std::istream& ss);

  void nextCh();

  [[nodiscard]] UChar32 peek() const;

  std::optional<Token> consumeWhitespaceAndComments();

  uint64_t consumeIntegerLiteral(uint8_t base);

  Token consumeNumberLiteral();

  Token consumeHardwareQubit();

  Token consumeString();

  Token consumeName();

  void error(const icu::UnicodeString& msg) const {
    std::cerr << "Error at line " << line << ", column " << col << ": " << msg
              << '\n';
  }

  void expect(const UChar32& expected) {
    if (ch != expected) {
      auto ustr =
          icu::UnicodeString{"Expected '"} + expected + "', got '" + ch + "'";

      error(ustr);
    } else {
      nextCh();
    }
  }

public:
  explicit Scanner(std::istream& in);

  ~Scanner() = default;

  Token next();
};
} // namespace qasm3
