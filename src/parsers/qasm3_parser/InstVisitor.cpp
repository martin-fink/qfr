#include "parsers/qasm3_parser/InstVisitor.hpp"

#include "parsers/qasm3_parser/Statement.hpp"

#include <cmath>

namespace qasm3 {
namespace {
std::shared_ptr<Constant> evaluate(BinaryExpression::Op op, int64_t lhs,
                                   int64_t rhs) {
  int64_t result = 0;
  switch (op) {
  case BinaryExpression::Power:
    result = static_cast<int64_t>(std::pow(lhs, rhs));
    break;
  case BinaryExpression::Add:
    result = lhs + rhs;
    break;
  case BinaryExpression::Subtract:
    result = lhs - rhs;
    break;
  case BinaryExpression::Multiply:
    result = lhs * rhs;
    break;
  case BinaryExpression::Divide:
    result = lhs / rhs;
    break;
  case BinaryExpression::Modulo:
    result = lhs % rhs;
    break;
  case BinaryExpression::LeftShift:
    result = lhs << rhs;
    break;
  case BinaryExpression::RightShift:
    result = lhs >> rhs;
    break;
  case BinaryExpression::LessThan:
    result = static_cast<int64_t>(lhs < rhs);
    break;
  case BinaryExpression::LessThanOrEqual:
    result = static_cast<int64_t>(lhs <= rhs);
    break;
  case BinaryExpression::GreaterThan:
    result = static_cast<int64_t>(lhs > rhs);
    break;
  case BinaryExpression::GreaterThanOrEqual:
    result = static_cast<int64_t>(lhs >= rhs);
    break;
  case BinaryExpression::Equal:
    result = static_cast<int64_t>(lhs == rhs);
    break;
  case BinaryExpression::NotEqual:
    result = static_cast<int64_t>(lhs != rhs);
    break;
  case BinaryExpression::BitwiseAnd:
    result = lhs & rhs;
    break;
  case BinaryExpression::BitwiseXor:
    result = lhs ^ rhs;
    break;
  case BinaryExpression::BitwiseOr:
    result = lhs | rhs;
    break;
  case BinaryExpression::LogicalAnd:
    result =
        static_cast<int64_t>(static_cast<bool>(lhs) && static_cast<bool>(rhs));
    break;
  case BinaryExpression::LogicalOr:
    result =
        static_cast<int64_t>(static_cast<bool>(lhs) || static_cast<bool>(rhs));
    break;
  }

  return std::make_shared<ConstantInt>(ConstantInt(result));
}

std::shared_ptr<Constant> evaluate(BinaryExpression::Op op, double lhs,
                                   double rhs) {
  switch (op) {
  case BinaryExpression::Power:
    return std::dynamic_pointer_cast<Constant>(
        std::make_shared<ConstantFP>(ConstantFP(std::pow(lhs, rhs))));
  case BinaryExpression::Add:
    return std::dynamic_pointer_cast<Constant>(
        std::make_shared<ConstantFP>(ConstantFP(lhs + rhs)));
  case BinaryExpression::Subtract:
    return std::dynamic_pointer_cast<Constant>(
        std::make_shared<ConstantFP>(ConstantFP(lhs - rhs)));
  case BinaryExpression::Multiply:
    return std::dynamic_pointer_cast<Constant>(
        std::make_shared<ConstantFP>(ConstantFP(lhs * rhs)));
  case BinaryExpression::Divide:
    return std::dynamic_pointer_cast<Constant>(
        std::make_shared<ConstantFP>(ConstantFP(lhs / rhs)));
  case BinaryExpression::LessThan:
    return std::dynamic_pointer_cast<Constant>(
        std::make_shared<ConstantInt>(ConstantInt(lhs < rhs)));
  case BinaryExpression::LessThanOrEqual:
    return std::dynamic_pointer_cast<Constant>(
        std::make_shared<ConstantInt>(ConstantInt(lhs <= rhs)));
  case BinaryExpression::GreaterThan:
    return std::dynamic_pointer_cast<Constant>(
        std::make_shared<ConstantInt>(ConstantInt(lhs > rhs)));
  case BinaryExpression::GreaterThanOrEqual:
    return std::dynamic_pointer_cast<Constant>(
        std::make_shared<ConstantInt>(ConstantInt(lhs >= rhs)));
  case BinaryExpression::Equal:
    return std::dynamic_pointer_cast<Constant>(
        std::make_shared<ConstantInt>(ConstantInt(lhs == rhs)));
  case BinaryExpression::NotEqual:
    return std::dynamic_pointer_cast<Constant>(
        std::make_shared<ConstantInt>(ConstantInt(lhs != rhs)));
  default:
    return nullptr;
  }
}

std::shared_ptr<Constant> evaluate(UnaryExpression::Op op, int64_t value) {
  switch (op) {
  case UnaryExpression::Op::BitwiseNot:
    return std::dynamic_pointer_cast<Constant>(
        std::make_shared<ConstantInt>(ConstantInt(~value)));
  case UnaryExpression::Op::LogicalNot:
    return std::dynamic_pointer_cast<Constant>(
        std::make_shared<ConstantInt>(ConstantInt(!static_cast<bool>(value))));
  case UnaryExpression::Op::Negate:
    return std::dynamic_pointer_cast<Constant>(
        std::make_shared<ConstantInt>(ConstantInt(-value)));
  case UnaryExpression::Op::DurationOf:
    throw std::runtime_error("DurationOf not implemented.");
  case UnaryExpression::Sin:
  case UnaryExpression::Cos:
  case UnaryExpression::Tan:
  case UnaryExpression::Exp:
  case UnaryExpression::Ln:
  case UnaryExpression::Sqrt:
    throw std::runtime_error("not implemented");
  }
}

std::shared_ptr<Constant> evaluate(UnaryExpression::Op op, double value) {
  switch (op) {
  case UnaryExpression::Op::Negate:
    return std::dynamic_pointer_cast<Constant>(
        std::make_shared<ConstantFP>(ConstantFP(-value)));
  case UnaryExpression::Op::DurationOf:
    throw std::runtime_error("DurationOf not implemented.");
  case UnaryExpression::Sin:
  case UnaryExpression::Cos:
  case UnaryExpression::Tan:
  case UnaryExpression::Exp:
  case UnaryExpression::Ln:
  case UnaryExpression::Sqrt:
    throw std::runtime_error("not implemented");
  default:
    return nullptr;
  }
}
} // namespace

std::any ConstExpressionPropagation::visitBinaryExpression(
    std::shared_ptr<BinaryExpression> binaryExpression) {
  auto lhs = std::any_cast<std::shared_ptr<Constant>>(
      binaryExpression->lhs->accept(this));
  auto rhs = std::any_cast<std::shared_ptr<Constant>>(
      binaryExpression->rhs->accept(this));

  if (lhs == nullptr || rhs == nullptr) {
    return nullptr;
  }

  binaryExpression->lhs = lhs;
  binaryExpression->rhs = rhs;

  if (lhs->isFP() || rhs->isFP()) {
    return evaluate(binaryExpression->op, lhs->getFP(), rhs->getFP());
  }
  return evaluate(binaryExpression->op, lhs->getInt(), rhs->getInt());
}

std::any ConstExpressionPropagation::visitUnaryExpression(
    std::shared_ptr<UnaryExpression> unaryExpression) {
  auto value = std::any_cast<std::shared_ptr<Constant>>(
      unaryExpression->operand->accept(this));

  if (value == nullptr) {
    return nullptr;
  }

  unaryExpression->operand = value;

  if (value->isFP()) {
    return evaluate(unaryExpression->op, value->getFP());
  }
  return evaluate(unaryExpression->op, value->getInt());
}

std::any ConstExpressionPropagation::visitConstantIntExpression(
    std::shared_ptr<ConstantInt> constant) {
  return std::dynamic_pointer_cast<Constant>(constant);
}

std::any ConstExpressionPropagation::visitConstantFPExpression(
    std::shared_ptr<ConstantFP> constant) {
  return std::dynamic_pointer_cast<Constant>(constant);
}

std::any ConstExpressionPropagation::visitIdentifierExpression(
    std::shared_ptr<IdentifierExpression> identifierExpression) {
  auto identifier = identifierExpression->identifier;
  auto value = environment->find(identifier);
  if (value == environment->end()) {
    icu::UnicodeString message{"Usage of undeclared identifier '"};
    message.append(identifier);
    message.append("'.");
    throw TypeCheckException(message);
  }

  auto declaration = value->second;
  if (!declaration.isConst) {
    icu::UnicodeString message{"Usage of non-const identifier '"};
    message.append(identifier);
    message.append("'.");
    throw TypeCheckException(message);
  }

  auto result = std::any_cast<std::shared_ptr<Constant>>(
      declaration.expression->accept(this));

  if (result != nullptr) {
    declaration.expression = result;
  }

  return result;
}

std::any ConstExpressionPropagation::visitIdentifierList(
    std::shared_ptr<IdentifierList> /*identifierList*/) {
  // cannot be a constant expression
  return nullptr;
}

std::any ConstExpressionPropagation::visitMeasureExpression(
    std::shared_ptr<MeasureExpression> /*measureExpression*/) {
  // cannot be a constant expression
  return nullptr;
}
} // namespace qasm3
