#include <map>
#include <unicode/ustring.h>

namespace qasm3 {
class Expression;

struct Declaration {
public:
  icu::UnicodeString identifier;
  std::shared_ptr<Expression> expression;
  enum Type {
    Qubit,
    Bit,
    Float,
    Int,
    Uint,
    Bool,
  } type;
  uint8_t width;
  bool isConst;
};

using Environment = std::map<icu::UnicodeString, Declaration>;
} // namespace qasm3
