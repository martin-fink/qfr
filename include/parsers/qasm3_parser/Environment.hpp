#include <string>
#include <map>

namespace qasm3 {
class Expression;

struct Declaration {
public:
  std::string identifier;
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

using Environment = std::map<std::string, Declaration>;
} // namespace qasm3
