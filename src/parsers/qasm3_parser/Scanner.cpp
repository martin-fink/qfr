#include "parsers/qasm3_parser/Scanner.hpp"

#include "parsers/qasm3_parser/StdGates.hpp"

#include <unicode/unistr.h>
#include <unicode/utf8.h>
#include <unicode/utypes.h>

namespace qasm3 {
UChar32 Scanner::readUtf8Codepoint(std::istream& in) {
  char c = 0;
  in.get(c);
  auto trailBytes = U8_COUNT_TRAIL_BYTES(c);

  std::vector<char> buffer;
  buffer.emplace_back(c);

  for (int i = 0; i < trailBytes; ++i) {
    in.get(c);
    buffer.emplace_back(c);
  }

#pragma clang diagnostic push // we get a warning if we have an initializer and
                              // if we don't so just suppress this warning
#pragma ide diagnostic ignored "UnusedValue"
  UChar32 codepoint = 0;
#pragma clang diagnostic pop
  int32_t offset = 0;
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wsign-compare"
  U8_NEXT(buffer, offset, buffer.size(),
          codepoint); // NOLINT(bugprone-assignment-in-if-condition)
#pragma clang diagnostic pop

  return codepoint;
}

void Scanner::nextCh() {
  if (!is.eof()) {
    col++;
    ch = readUtf8Codepoint(is);
  } else {
    ch = 0;
  }
  if (ch == '\n') {
    col = 0;
    line++;
  }
}

UChar32 Scanner::peek() const {
  if (!is.eof()) {
    return static_cast<char>(is.peek());
  }
  return 0;
}

std::optional<Token> Scanner::consumeWhitespaceAndComments() {
  while (isSpace(ch)) {
    nextCh();
  }
  if (ch == '/' && peek() == '/') {
    Token t(line, col);
    // consume until newline
    icu::UnicodeString str;
    while (ch != '\n' && ch != 0) {
      str.append(ch);
      nextCh();
    }
    if (ch == '\n') {
      nextCh();
    }

    t.str = str;
    t.kind = Token::Kind::Comment;
    t.endCol = col;
    t.endLine = line;
    return t;
  }
  if (ch == '/' && peek() == '*') {
    // consume /*
    nextCh();
    nextCh();
    while (ch != 0 && (ch != '*' || peek() != '/')) {
      nextCh();
    }
    // consume */
    expect('*');
    expect('/');

    // tail calls should be optimized away
    return consumeWhitespaceAndComments();
  }

  return {};
}

Token Scanner::consumeName() {
  Token t(line, col);
  icu::UnicodeString str;
  while (isFirstIdChar(ch) || isNum(ch)) {
    str.append(ch);
    nextCh();
  }

  t.str = str;
  if (keywords.find(t.str) != keywords.end()) {
    t.kind = keywords[t.str];
  } else {
    t.kind = Token::Kind::Identifier;
  }

  t.endCol = col;
  t.endLine = line;
  return t;
}

uint64_t Scanner::consumeIntegerLiteral(uint8_t base) {
  uint64_t val = 0;
  auto isValidChar = [base, this](UChar32 c) {
    if (base == 2) {
      return c == '0' || c == '1';
    }
    if (base == 8) {
      return c >= '0' && c <= '7';
    }
    if (base == 10) {
      return isNum(c);
    }
    if (base == 16) {
      return isHex(c);
    }
    return false;
  };
  while (isValidChar(ch) || ch == '_') {
    if (ch == '_') {
      nextCh();
      continue;
    }

    if (isNum(ch)) {
      val *= base;
      val += static_cast<uint64_t>(ch) - '0';
    } else {
      val *= base;
      val += static_cast<uint64_t>(ch) - 'a' + 10;
    }
    nextCh();
  }
  return val;
}

Token Scanner::consumeNumberLiteral() {
  Token t(line, col);
  uint8_t base = 10;

  if (ch == '0') {
    switch (peek()) {
    case 'b':
    case 'B':
      base = 2;
      nextCh();
      nextCh();
      break;
    case 'o':
      base = 8;
      nextCh();
      nextCh();
      break;
    case 'x':
    case 'X':
      base = 16;
      nextCh();
      nextCh();
      break;
    }
  }
  const auto valBeforeDecimalSeparator = consumeIntegerLiteral(base);

  if (ch == '.') {
    if (base != 10) {
      error("unexpected `.` in non-decimal number");
    }

    nextCh();
    auto valAfterDecimalSeparator = consumeIntegerLiteral(base);

    const auto numDigits = log10(valAfterDecimalSeparator + 1);
    const double val =
        static_cast<double>(valBeforeDecimalSeparator) +
        static_cast<double>(valAfterDecimalSeparator) / pow(10, numDigits);

    t.kind = Token::Kind::FloatLiteral;
    t.valReal = val;
    return t;
  }
  if (ch == 'e' || ch == 'E') {
    if (base != 10) {
      error("unexpected `e` or `E` in non-decimal number");
    }

    const auto exponent = static_cast<double>(consumeIntegerLiteral(base));
    const auto val =
        static_cast<double>(valBeforeDecimalSeparator) * pow(10, exponent);
    t.kind = Token::Kind::FloatLiteral;
    t.valReal = val;
    return t;
  }

  t.kind = Token::Kind::IntegerLiteral;
  t.val = static_cast<int64_t>(valBeforeDecimalSeparator);

  t.endCol = col;
  t.endLine = line;

  return t;
}

Token Scanner::consumeHardwareQubit() {
  Token t(line, col);

  expect('$');

  t.kind = Token::Kind::HardwareQubit;
  t.val = 0;
  while (isNum(ch)) {
    t.val *= 10;
    t.val += static_cast<int64_t>(ch - '0');
    nextCh();
  }

  t.endCol = col;
  t.endLine = line;

  return t;
}

Token Scanner::consumeString() {
  Token t(line, col);
  t.kind = Token::Kind::StringLiteral;

  if (ch != '"' && ch != '\'') {
    error("expected `\"` or `'`");
    t.kind = Token::Kind::None;
    return t;
  }
  const auto delim = ch;
  nextCh();

  icu::UnicodeString str;
  while (ch != delim) {
    str.append(ch);
    nextCh();
  }

  t.str = str;

  expect(delim);

  t.endCol = col;
  t.endLine = line;

  return t;
}

Scanner::Scanner(std::istream& in) : is(in) {
  UErrorCode status = U_ZERO_ERROR;
  spaceSet = icu::UnicodeSet(R"([:White_Space:])", status);
  assert(U_SUCCESS(status) && "Invalid pattern.");
  firstIdCharSet =
      icu::UnicodeSet("[[:Lu:][:Ll:][:Lt:][:Lm:][:Lo:][:Nl:]a-zA-Z_]", status);
  assert(U_SUCCESS(status) && "Invalid pattern.");
  numSet = icu::UnicodeSet(R"([:number:])", status);
  assert(U_SUCCESS(status) && "Invalid pattern.");
  hexSet = icu::UnicodeSet(R"([[:number:]a-fA-F])", status);
  assert(U_SUCCESS(status) && "Invalid pattern.");

  keywords["OPENQASM"] = Token::Kind::OpenQasm;
  keywords["include"] = Token::Kind::Include;
  keywords["defcalgrammar"] = Token::Kind::DefCalGrammar;
  keywords["def"] = Token::Kind::Def;
  keywords["cal"] = Token::Kind::Cal;
  keywords["defcal"] = Token::Kind::DefCal;
  keywords["gate"] = Token::Kind::Gate;
  keywords["extern"] = Token::Kind::Extern;
  keywords["box"] = Token::Kind::Box;
  keywords["let"] = Token::Kind::Let;
  keywords["break"] = Token::Kind::Break;
  keywords["continue"] = Token::Kind::Continue;
  keywords["if"] = Token::Kind::If;
  keywords["else"] = Token::Kind::Else;
  keywords["end"] = Token::Kind::End;
  keywords["return"] = Token::Kind::Return;
  keywords["for"] = Token::Kind::For;
  keywords["while"] = Token::Kind::While;
  keywords["in"] = Token::Kind::In;
  // TOOD: handle pragma properly
  keywords["pragma"] = Token::Kind::Pragma;
  keywords["input"] = Token::Kind::Input;
  keywords["output"] = Token::Kind::Output;
  keywords["const"] = Token::Kind::Const;
  keywords["readonly"] = Token::Kind::ReadOnly;
  keywords["mutable"] = Token::Kind::Mutable;
  keywords["qreg"] = Token::Kind::Qreg;
  keywords["qubit"] = Token::Kind::Qubit;
  keywords["creg"] = Token::Kind::CReg;
  keywords["bool"] = Token::Kind::Bool;
  keywords["bit"] = Token::Kind::Bit;
  keywords["int"] = Token::Kind::Int;
  keywords["uint"] = Token::Kind::Uint;
  keywords["float"] = Token::Kind::Float;
  keywords["angle"] = Token::Kind::Angle;
  keywords["complex"] = Token::Kind::Complex;
  keywords["array"] = Token::Kind::Array;
  keywords["void"] = Token::Kind::Void;
  keywords["duration"] = Token::Kind::Duration;
  keywords["stretch"] = Token::Kind::Stretch;
  keywords["gphase"] = Token::Kind::Gphase;
  keywords["inv"] = Token::Kind::Inv;
  keywords["pow"] = Token::Kind::Pow;
  keywords["ctrl"] = Token::Kind::Ctrl;
  keywords["negctrl"] = Token::Kind::NegCtrl;
  keywords["#dim"] = Token::Kind::Dim;
  keywords["durationof"] = Token::Kind::DurationOf;
  keywords["delay"] = Token::Kind::Delay;
  keywords["reset"] = Token::Kind::Reset;
  keywords["measure"] = Token::Kind::Measure;
  keywords["barrier"] = Token::Kind::Barrier;
  keywords["true"] = Token::Kind::True;
  keywords["false"] = Token::Kind::False;
  keywords["im"] = Token::Kind::Imag;
  //        keywords["dt"]         = Token::Kind::TimeUnitDt;
  //        keywords["ns"]         = Token::Kind::TimeUnitNs;
  //        keywords["us"]         = Token::Kind::TimeUnitUs;
  //        keywords["mys"]        = Token::Kind::TimeUnitMys;
  //        keywords["ms"]         = Token::Kind::TimeUnitMs;
  //        keywords["s"]          = Token::Kind::TimeUnitS;
  keywords["sin"] = Token::Kind::Sin;
  keywords["cos"] = Token::Kind::Cos;
  keywords["tan"] = Token::Kind::Tan;
  keywords["exp"] = Token::Kind::Exp;
  keywords["ln"] = Token::Kind::Ln;
  keywords["sqrt"] = Token::Kind::Sqrt;

  nextCh();
}

Token Scanner::next() {
  auto commentToken = consumeWhitespaceAndComments();
  if (commentToken) {
    return *commentToken;
  }

  if (isFirstIdChar(ch)) {
    return consumeName();
  }
  if (isNum(ch) || ch == '.') {
    return consumeNumberLiteral();
  }
  if (ch == '$') {
    return consumeHardwareQubit();
  }

  if (ch == '"' || ch == '\'') {
    return consumeString();
  }

  Token t(line, col);
  switch (ch) {
  case 0:
    t.kind = Token::Kind::Eof;
    // Here we return as we don't want to call nextCh after EOF.
    // We also don't set length, as the eof token has no length.
    return t;
  case '[':
    t.kind = Token::Kind::LBracket;
    break;
  case ']':
    t.kind = Token::Kind::RBracket;
    break;
  case '{':
    t.kind = Token::Kind::LBrace;
    break;
  case '}':
    t.kind = Token::Kind::RBrace;
    break;
  case '(':
    t.kind = Token::Kind::LParen;
    break;
  case ')':
    t.kind = Token::Kind::RParen;
    break;
  case ':':
    t.kind = Token::Kind::Colon;
    break;
  case ';':
    t.kind = Token::Kind::Semicolon;
    break;
  case '.':
    t.kind = Token::Kind::Dot;
    break;
  case ',':
    t.kind = Token::Kind::Comma;
    break;
  case '-':
    switch (peek()) {
    case '>':
      nextCh();
      t.kind = Token::Kind::Arrow;
      break;
    case '=':
      nextCh();
      t.kind = Token::Kind::MinusEquals;
      break;
    default:
      t.kind = Token::Kind::Minus;
      break;
    }
    break;
  case '+':
    switch (peek()) {
    case '=':
      nextCh();
      t.kind = Token::Kind::PlusEquals;
      break;
    case '+':
      nextCh();
      t.kind = Token::Kind::DoublePlus;
      break;
    default:
      t.kind = Token::Kind::Plus;
      break;
    }
    break;
  case '*':
    switch (peek()) {
    case '=':
      nextCh();
      t.kind = Token::Kind::AsteriskEquals;
      break;
    case '*':
      nextCh();
      if (peek() == '=') {
        nextCh();
        t.kind = Token::Kind::DoubleAsteriskEquals;
      } else {
        t.kind = Token::Kind::DoubleAsterisk;
      }
      break;
    default:
      t.kind = Token::Kind::Asterisk;
      break;
    }
    break;
  case '/':
    switch (peek()) {
    case '=':
      nextCh();
      t.kind = Token::Kind::SlashEquals;
      break;
    default:
      t.kind = Token::Kind::Slash;
      break;
    }
    break;
  case '%':
    switch (peek()) {
    case '=':
      nextCh();
      t.kind = Token::Kind::PercentEquals;
      break;
    default:
      t.kind = Token::Kind::Percent;
      break;
    }
    break;
  case '|':
    switch (peek()) {
    case '=':
      nextCh();
      t.kind = Token::Kind::PipeEquals;
      break;
    case '|':
      nextCh();
      t.kind = Token::Kind::DoublePipe;
      break;
    default:
      t.kind = Token::Kind::Pipe;
      break;
    }
    break;
  case '&':
    switch (peek()) {
    case '=':
      nextCh();
      t.kind = Token::Kind::AmpersandEquals;
      break;
    case '&':
      nextCh();
      t.kind = Token::Kind::DoubleAmpersand;
      break;
    default:
      t.kind = Token::Kind::Ampersand;
      break;
    }
    break;
  case '^':
    switch (peek()) {
    case '=':
      nextCh();
      t.kind = Token::Kind::CaretEquals;
      break;
    default:
      t.kind = Token::Kind::Caret;
      break;
    }
    break;
  case '~':
    switch (peek()) {
    case '=':
      nextCh();
      t.kind = Token::Kind::TildeEquals;
      break;
    default:
      t.kind = Token::Kind::Tilde;
      break;
    }
    break;
  case '!':
    t.kind = Token::Kind::ExclamationPoint;
    break;
  case '<':
    switch (peek()) {
    case '=':
      nextCh();
      t.kind = Token::Kind::LessThanEquals;
      break;
    case '<':
      nextCh();
      if (peek() == '=') {
        nextCh();
        t.kind = Token::Kind::LeftShitEquals;
      } else {
        t.kind = Token::Kind::LeftShift;
      }
      break;
    default:
      t.kind = Token::Kind::LessThan;
      break;
    }
    break;
  case '>':
    switch (peek()) {
    case '=':
      nextCh();
      t.kind = Token::Kind::GreaterThanEquals;
      break;
    case '>':
      nextCh();
      if (peek() == '=') {
        nextCh();
        t.kind = Token::Kind::RightShiftEquals;
      } else {
        t.kind = Token::Kind::RightShift;
      }
      break;
    default:
      t.kind = Token::Kind::GreaterThan;
      break;
    }
    break;
  case '=':
    if (peek() == '=') {
      nextCh();
      t.kind = Token::Kind::DoubleEquals;
    } else {
      t.kind = Token::Kind::Equals;
    }
    break;
  case '@':
    t.kind = Token::Kind::At;
    break;
  default: {
    auto ustr = icu::UnicodeString{"Unknown character '"} + ch + "'";
    error(ustr);
    t.kind = Token::Kind::None;
    nextCh();
    break;
  }
  }

  nextCh();

  t.endCol = col;
  t.endLine = line;
  return t;
}
} // namespace qasm3
