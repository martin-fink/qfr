#pragma once

#include "InstVisitor.hpp"
#include "QuantumComputation.hpp"
#include "Types.hpp"

#include <any>
#include <string>
#include <utility>
#include <vector>

namespace qasm3 {

struct DebugInfo {
  size_t line;
  size_t column;
  size_t endLine;
  size_t endColumn;
  std::string filename;
  std::shared_ptr<DebugInfo> parent;

  DebugInfo(size_t line, size_t column, size_t endLine, size_t endColumn,
            std::string filename, std::shared_ptr<DebugInfo> parent = nullptr)
      : line(line), column(column), endLine(endLine), endColumn(endColumn),
        filename(std::move(std::move(filename))), parent(std::move(parent)) {}

  [[nodiscard]] std::string toString() const {
    // TOOD: also print endLine and endColumn
    return filename + ":" + std::to_string(line) + ":" + std::to_string(column);
  }
};

class Statement {
public:
  std::shared_ptr<DebugInfo> debugInfo;
  explicit Statement(std::shared_ptr<DebugInfo> debugInfo)
      : debugInfo(std::move(debugInfo)) {}
  virtual ~Statement() = default;

  virtual void accept(InstVisitor* visitor) = 0;
};

class GateDeclaration : public Statement,
                        public std::enable_shared_from_this<GateDeclaration> {
public:
  std::string identifier;
  std::shared_ptr<IdentifierList> parameters;
  std::shared_ptr<IdentifierList> qubits;
  std::vector<std::shared_ptr<GateCallStatement>> statements;

  explicit GateDeclaration(
      std::shared_ptr<DebugInfo> debugInfo, std::string identifier,
      std::shared_ptr<IdentifierList> parameters,
      std::shared_ptr<IdentifierList> qubits,
      std::vector<std::shared_ptr<GateCallStatement>> statements)
      : Statement(std::move(debugInfo)), identifier(std::move(identifier)),
        parameters(std::move(parameters)), qubits(std::move(qubits)),
        statements(std::move(statements)) {}

  void accept(InstVisitor* visitor) override {
    visitor->visitGateStatement(shared_from_this());
  }
};

class VersionDeclaration
    : public Statement,
      public std::enable_shared_from_this<VersionDeclaration> {
public:
  double version;

  explicit VersionDeclaration(std::shared_ptr<DebugInfo> debugInfo,
                              double version)
      : Statement(std::move(debugInfo)), version(version) {}

  void accept(InstVisitor* visitor) override {
    visitor->visitVersionDeclaration(shared_from_this());
  }
};

class InitialLayout : public Statement,
                      public std::enable_shared_from_this<InitialLayout> {
public:
  qc::Permutation permutation;

  explicit InitialLayout(std::shared_ptr<DebugInfo> debugInfo,
                         qc::Permutation permutation)
      : Statement(std::move(debugInfo)), permutation(std::move(permutation)) {}

private:
  void accept(InstVisitor* visitor) override {
    visitor->visitInitialLayout(shared_from_this());
  }
};

class OutputPermutation
    : public Statement,
      public std::enable_shared_from_this<OutputPermutation> {
public:
  qc::Permutation permutation;

  explicit OutputPermutation(std::shared_ptr<DebugInfo> debugInfo,
                             qc::Permutation permutation)
      : Statement(std::move(debugInfo)), permutation(std::move(permutation)) {}

private:
  void accept(InstVisitor* visitor) override {
    visitor->visitOutputPermutation(shared_from_this());
  }
};

class Expression {
public:
  virtual ~Expression() = default;

  virtual std::any accept(ExpressionVisitor* visitor) = 0;
};

class DeclarationExpression {
public:
  std::shared_ptr<Expression> expression;

  explicit DeclarationExpression(std::shared_ptr<Expression> expression)
      : expression(std::move(expression)) {}

  virtual ~DeclarationExpression() = default;

  virtual std::any accept(ExpressionVisitor* visitor) {
    return expression->accept(visitor);
  };
};

class Constant : public Expression {
protected:
  Constant() {}

public:
  [[nodiscard]] virtual bool isInt() const { return false; };
  [[nodiscard]] virtual bool isFP() const { return false; };
  [[nodiscard]] virtual int64_t getInt() const { return 0; };
  [[nodiscard]] virtual double getFP() const { return 0.0; };
};

class ConstantInt : public Constant,
                    public std::enable_shared_from_this<ConstantInt> {
public:
  int64_t val;

  explicit ConstantInt(int64_t val) : val(val) {}

  explicit ConstantInt(bool val) : val(static_cast<int64_t>(val)) {}

  std::any accept(ExpressionVisitor* visitor) override {
    return visitor->visitConstantIntExpression(shared_from_this());
  }

  [[nodiscard]] bool isInt() const override { return true; }
  [[nodiscard]] int64_t getInt() const override { return val; }
  [[nodiscard]] double getFP() const override {
    return static_cast<double>(val);
  }
};

class ConstantFP : public Constant,
                   public std::enable_shared_from_this<ConstantFP> {
public:
  double val;

  explicit ConstantFP(double val) : val(val) {}

  std::any accept(ExpressionVisitor* visitor) override {
    return visitor->visitConstantFPExpression(shared_from_this());
  }

  [[nodiscard]] bool isFP() const override { return true; }
  [[nodiscard]] double getFP() const override { return val; }
};

class BinaryExpression : public Expression,
                         public std::enable_shared_from_this<BinaryExpression> {
public:
  enum Op {
    Power,
    Add,
    Subtract,
    Multiply,
    Divide,
    Modulo,
    LeftShift,
    RightShift,
    LessThan,
    LessThanOrEqual,
    GreaterThan,
    GreaterThanOrEqual,
    Equal,
    NotEqual,
    BitwiseAnd,
    BitwiseXor,
    BitwiseOr,
    LogicalAnd,
    LogicalOr,
  };

  Op op;
  std::shared_ptr<Expression> lhs;
  std::shared_ptr<Expression> rhs;

  BinaryExpression(Op op, std::shared_ptr<Expression> lhs,
                   std::shared_ptr<Expression> rhs)
      : op(op), lhs(std::move(lhs)), rhs(std::move(rhs)) {}

  std::any accept(ExpressionVisitor* visitor) override {
    return visitor->visitBinaryExpression(shared_from_this());
  }
};

class UnaryExpression : public Expression,
                        public std::enable_shared_from_this<UnaryExpression> {
public:
  enum Op {
    BitwiseNot,
    LogicalNot,
    Negate,
    DurationOf,
    Sin,
    Cos,
    Tan,
    Exp,
    Ln,
    Sqrt,
  };

  std::shared_ptr<Expression> operand;
  Op op;

  UnaryExpression(Op op, std::shared_ptr<Expression> operand)
      : operand(std::move(operand)), op(op) {}

  std::any accept(ExpressionVisitor* visitor) override {
    return visitor->visitUnaryExpression(shared_from_this());
  }
};

class IdentifierExpression
    : public Expression,
      public std::enable_shared_from_this<IdentifierExpression> {
public:
  std::string identifier;

  explicit IdentifierExpression(std::string identifier)
      : identifier(std::move(identifier)) {}

  std::any accept(ExpressionVisitor* visitor) override {
    return visitor->visitIdentifierExpression(shared_from_this());
  }
};

class IdentifierList : public Expression,
                       public std::enable_shared_from_this<IdentifierList> {
public:
  std::vector<std::shared_ptr<IdentifierExpression>> identifiers;

  explicit IdentifierList(
      std::vector<std::shared_ptr<IdentifierExpression>> identifiers)
      : identifiers(std::move(identifiers)) {}

  explicit IdentifierList() : identifiers() {}

  std::any accept(ExpressionVisitor* visitor) override {
    return visitor->visitIdentifierList(shared_from_this());
  }
};

// TODO: physical qubits are currently not supported
class GateOperand {
public:
  std::string identifier;
  std::shared_ptr<Expression> expression;

  GateOperand(std::string identifier,
              std::shared_ptr<Expression> expression)
      : identifier(std::move(identifier)), expression(std::move(expression)) {}
};

class DeclarationStatement
    : public Statement,
      public std::enable_shared_from_this<DeclarationStatement> {
public:
  bool isConst;
  std::unique_ptr<Type> type;
  std::string identifier;
  std::shared_ptr<DeclarationExpression> expression;

  DeclarationStatement(std::shared_ptr<DebugInfo> debugInfo, bool isConst,
                       std::unique_ptr<Type> type,
                       std::string identifier,
                       std::shared_ptr<DeclarationExpression> expression)
      : Statement(std::move(debugInfo)), isConst(isConst),
        type(std::move(type)), identifier(std::move(identifier)),
        expression(std::move(expression)) {}

  void accept(InstVisitor* visitor) override {
    visitor->visitDeclarationStatement(shared_from_this());
  }
};

class GateModifier : public std::enable_shared_from_this<GateModifier> {
protected:
  GateModifier() {}

  virtual ~GateModifier() = default;
};

class InvGateModifier : public GateModifier,
                        public std::enable_shared_from_this<InvGateModifier> {
public:
  explicit InvGateModifier() = default;
};

class PowGateModifier : public GateModifier,
                        public std::enable_shared_from_this<PowGateModifier> {
public:
  std::shared_ptr<Expression> expression;

  explicit PowGateModifier(std::shared_ptr<Expression> expression)
      : expression(std::move(expression)) {}
};

class CtrlGateModifier : public GateModifier,
                         public std::enable_shared_from_this<CtrlGateModifier> {
public:
  bool ctrlType;
  std::shared_ptr<Expression> expression;

  explicit CtrlGateModifier(bool ctrlType,
                            std::shared_ptr<Expression> expression)
      : ctrlType(ctrlType), expression(std::move(expression)) {}
};

class GateCallStatement
    : public Statement,
      public std::enable_shared_from_this<GateCallStatement> {
public:
  std::string identifier;
  std::vector<std::shared_ptr<GateModifier>> modifiers;
  std::vector<std::shared_ptr<Expression>> arguments;
  std::vector<std::shared_ptr<GateOperand>> operands;

  GateCallStatement(std::shared_ptr<DebugInfo> debugInfo,
                    std::string identifier,
                    std::vector<std::shared_ptr<GateModifier>> modifiers,
                    std::vector<std::shared_ptr<Expression>> arguments,
                    std::vector<std::shared_ptr<GateOperand>> operands)
      : Statement(std::move(debugInfo)), identifier(std::move(identifier)),
        modifiers(std::move(modifiers)), arguments(std::move(arguments)),
        operands(std::move(operands)) {}

  void accept(InstVisitor* visitor) override {
    visitor->visitGateCallStatement(shared_from_this());
  }
};

class AssignmentStatement
    : public Statement,
      public std::enable_shared_from_this<AssignmentStatement> {
public:
  enum Type {
    Assignment,
    PlusAssignment,
    MinusAssignment,
    TimesAssignment,
    DivAssignment,
    BitwiseAndAssignment,
    BitwiseOrAssignment,
    BitwiseNotAssignment,
    BitwiseXorAssignment,
    LeftShiftAssignment,
    RightShiftAssignment,
    ModuloAssignment,
    PowerAssignment,
  } type;
  std::shared_ptr<IdentifierExpression> identifier;
  std::shared_ptr<Expression> indexExpression;
  std::shared_ptr<DeclarationExpression> expression;

  AssignmentStatement(std::shared_ptr<DebugInfo> debugInfo, Type type,
                      std::shared_ptr<IdentifierExpression> identifier,
                      std::shared_ptr<Expression> indexExpression,
                      std::shared_ptr<DeclarationExpression> expression)
      : Statement(std::move(debugInfo)), type(type),
        identifier(std::move(identifier)),
        indexExpression(std::move(indexExpression)),
        expression(std::move(expression)) {}

  void accept(InstVisitor* visitor) override {
    visitor->visitAssignmentStatement(shared_from_this());
  }
};

class MeasureExpression
    : public Expression,
      public std::enable_shared_from_this<MeasureExpression> {
public:
  std::shared_ptr<GateOperand> gate;

  explicit MeasureExpression(std::shared_ptr<GateOperand> gate)
      : gate(std::move(gate)) {}

  std::any accept(ExpressionVisitor* visitor) override {
    return visitor->visitMeasureExpression(shared_from_this());
  }
};

class BarrierStatement : public Statement,
                         public std::enable_shared_from_this<BarrierStatement> {
public:
  std::vector<std::shared_ptr<GateOperand>> gates;

  explicit BarrierStatement(std::shared_ptr<DebugInfo> debugInfo,
                            std::vector<std::shared_ptr<GateOperand>> gates)
      : Statement(std::move(debugInfo)), gates(std::move(gates)) {}

  void accept(InstVisitor* visitor) override {
    visitor->visitBarrierStatement(shared_from_this());
  }
};

class ResetStatement : public Statement,
                       public std::enable_shared_from_this<ResetStatement> {
public:
  std::shared_ptr<GateOperand> gate;

  explicit ResetStatement(std::shared_ptr<DebugInfo> debugInfo,
                          std::shared_ptr<GateOperand> gate)
      : Statement(std::move(debugInfo)), gate(std::move(gate)) {}

  void accept(InstVisitor* visitor) override {
    visitor->visitResetStatement(shared_from_this());
  }
};
} // namespace qasm3
