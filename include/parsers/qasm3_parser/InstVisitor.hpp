#pragma once

#include "Environment.hpp"

#include <any>
#include <iostream>
#include <map>
#include <sstream>
#include <utility>

namespace qasm3 {

class TypeCheckException : public std::exception {
private:
  std::string msg;

public:
  explicit TypeCheckException(std::string msg) : msg(msg) {}

  [[nodiscard]] const char* what() const noexcept override {
    return msg.c_str();
  }
};

// We cannot include Statement.hpp here, because it would create a circular
// dependency.
class GateDeclaration;
class GateCallStatement;
class VersionDeclaration;
class DeclarationStatement;
class InitialLayout;
class OutputPermutation;
class AssignmentStatement;
class BarrierStatement;
class ResetStatement;

class BinaryExpression;
class UnaryExpression;
class IdentifierExpression;
class IdentifierList;
class Expression;
class ConstantInt;
class ConstantFP;
class MeasureExpression;

class InstVisitor {
public:
  virtual void
  visitGateStatement(std::shared_ptr<GateDeclaration> gateStatement) = 0;
  virtual void visitVersionDeclaration(
      std::shared_ptr<VersionDeclaration> versionDeclaration) = 0;
  virtual void visitDeclarationStatement(
      std::shared_ptr<DeclarationStatement> declarationStatement) = 0;
  virtual void
  visitInitialLayout(std::shared_ptr<InitialLayout> initialLayout) = 0;
  virtual void visitOutputPermutation(
      std::shared_ptr<OutputPermutation> outputPermutation) = 0;
  virtual void visitGateCallStatement(
      std::shared_ptr<GateCallStatement> gateCallStatement) = 0;
  virtual void visitAssignmentStatement(
      std::shared_ptr<AssignmentStatement> assignmentStatement) = 0;
  virtual void
  visitBarrierStatement(std::shared_ptr<BarrierStatement> barrierStatement) = 0;
  virtual void
  visitResetStatement(std::shared_ptr<ResetStatement> resetStatement) = 0;

  virtual ~InstVisitor() = default;
};

class ExpressionVisitor {
public:
  virtual std::any
  visitBinaryExpression(std::shared_ptr<BinaryExpression> binaryExpression) = 0;
  virtual std::any
  visitUnaryExpression(std::shared_ptr<UnaryExpression> unaryExpression) = 0;
  virtual std::any
  visitConstantIntExpression(std::shared_ptr<ConstantInt> constantInt) = 0;
  virtual std::any
  visitConstantFPExpression(std::shared_ptr<ConstantFP> constantFP) = 0;
  virtual std::any visitIdentifierExpression(
      std::shared_ptr<IdentifierExpression> identifierExpression) = 0;
  virtual std::any
  visitIdentifierList(std::shared_ptr<IdentifierList> identifierList) = 0;
  virtual std::any visitMeasureExpression(
      std::shared_ptr<MeasureExpression> measureExpression) = 0;

  virtual ~ExpressionVisitor() = default;
};

class ConstExpressionPropagation : public ExpressionVisitor {
private:
  std::shared_ptr<Environment> environment;

public:
  explicit ConstExpressionPropagation(std::shared_ptr<Environment> environment)
      : environment(std::move(environment)) {}

  std::any visitBinaryExpression(
      std::shared_ptr<BinaryExpression> binaryExpression) override;
  std::any visitUnaryExpression(
      std::shared_ptr<UnaryExpression> unaryExpression) override;
  std::any
  visitConstantIntExpression(std::shared_ptr<ConstantInt> constantInt) override;
  std::any
  visitConstantFPExpression(std::shared_ptr<ConstantFP> constantFP) override;
  std::any visitIdentifierExpression(
      std::shared_ptr<IdentifierExpression> identifierExpression) override;
  std::any
  visitIdentifierList(std::shared_ptr<IdentifierList> identifierList) override;
  std::any visitMeasureExpression(
      std::shared_ptr<MeasureExpression> measureExpression) override;
};

class BitType;
class IntType;
class UintType;
class FloatType;
class AngleType;
class BoolType;
class DurationType;
class StretchType;
class ComplexType;
class QubitType;

class TypeVisitor {
public:
  virtual void visitBitType(BitType* type) = 0;
  virtual void visitIntType(IntType* type) = 0;
  virtual void visitUintType(UintType* type) = 0;
  virtual void visitFloatType(FloatType* type) = 0;
  virtual void visitAngleType(AngleType* type) = 0;
  virtual void visitBoolType(BoolType* type) = 0;
  virtual void visitDurationType(DurationType* type) = 0;
  virtual void visitStretchType(StretchType* type) = 0;
  virtual void visitComplexType(ComplexType* type) = 0;
  virtual void visitQubitType(QubitType* type) = 0;

  virtual ~TypeVisitor() = default;
};
} // namespace qasm3
