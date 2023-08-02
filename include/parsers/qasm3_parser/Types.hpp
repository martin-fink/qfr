#pragma once

#include <memory>

namespace qasm3 {
class Expression;

class Type {
public:
  virtual ~Type() = default;

  [[nodiscard]] virtual bool allowsDesignator() const = 0;

  virtual void setDesignator(std::shared_ptr<Expression>) {}

  [[nodiscard]] virtual std::shared_ptr<Expression> getDesignator() const {
    return nullptr;
  }

  virtual void accept(TypeVisitor* visitor) = 0;
};

class ScalarType : public Type {};

class BitType : public ScalarType {
public:
  std::shared_ptr<Expression> designator;

  explicit BitType(std::shared_ptr<Expression> designator = nullptr)
      : designator(designator) {}

  [[nodiscard]] bool allowsDesignator() const override { return true; }

  void setDesignator(std::shared_ptr<Expression> d) override { designator = d; }

  [[nodiscard]] virtual std::shared_ptr<Expression>
  getDesignator() const override {
    return designator;
  }

  void accept(TypeVisitor* visitor) override { visitor->visitBitType(this); }
};

class IntType : public ScalarType {
public:
  std::shared_ptr<Expression> designator;

  explicit IntType(std::shared_ptr<Expression> designator = nullptr)
      : designator(designator) {}

  [[nodiscard]] bool allowsDesignator() const override { return true; }

  void setDesignator(std::shared_ptr<Expression> d) override { designator = d; }

  [[nodiscard]] virtual std::shared_ptr<Expression>
  getDesignator() const override {
    return designator;
  }

  void accept(TypeVisitor* visitor) override { visitor->visitIntType(this); }
};

class UintType : public ScalarType {
public:
  std::shared_ptr<Expression> designator;

  explicit UintType(std::shared_ptr<Expression> designator = nullptr)
      : designator(designator) {}

  [[nodiscard]] bool allowsDesignator() const override { return true; }

  void setDesignator(std::shared_ptr<Expression> d) override { designator = d; }

  [[nodiscard]] virtual std::shared_ptr<Expression>
  getDesignator() const override {
    return designator;
  }

  void accept(TypeVisitor* visitor) override { visitor->visitUintType(this); }
};

class FloatType : public ScalarType {
public:
  std::shared_ptr<Expression> designator;

  explicit FloatType(std::shared_ptr<Expression> designator = nullptr)
      : designator(designator) {}

  [[nodiscard]] bool allowsDesignator() const override { return true; }

  void setDesignator(std::shared_ptr<Expression> d) override { designator = d; }

  [[nodiscard]] virtual std::shared_ptr<Expression>
  getDesignator() const override {
    return designator;
  }

  void accept(TypeVisitor* visitor) override { visitor->visitFloatType(this); }
};

class AngleType : public ScalarType {
public:
  std::shared_ptr<Expression> designator;

  explicit AngleType(std::shared_ptr<Expression> designator = nullptr)
      : designator(designator) {}

  [[nodiscard]] bool allowsDesignator() const override { return true; }

  void setDesignator(std::shared_ptr<Expression> d) override { designator = d; }

  [[nodiscard]] virtual std::shared_ptr<Expression>
  getDesignator() const override {
    return designator;
  }

  void accept(TypeVisitor* visitor) override { visitor->visitAngleType(this); }
};

class BoolType : public ScalarType {
public:
  [[nodiscard]] bool allowsDesignator() const override { return false; }

  void accept(TypeVisitor* visitor) override { visitor->visitBoolType(this); }
};

class DurationType : public ScalarType {
public:
  [[nodiscard]] bool allowsDesignator() const override { return false; }

  void accept(TypeVisitor* visitor) override {
    visitor->visitDurationType(this);
  }
};

class StretchType : public ScalarType {
public:
  [[nodiscard]] bool allowsDesignator() const override { return false; }

  void accept(TypeVisitor* visitor) override {
    visitor->visitStretchType(this);
  }
};

class ComplexType : public ScalarType {
public:
  std::unique_ptr<ScalarType> scalarType;

  explicit ComplexType(std::unique_ptr<ScalarType> scalarType)
      : scalarType(std::move(scalarType)) {}

  [[nodiscard]] bool allowsDesignator() const override { return false; }

  void accept(TypeVisitor* visitor) override {
    visitor->visitComplexType(this);
  }
};

class QubitType : public Type {
public:
  std::shared_ptr<Expression> designator;

  explicit QubitType(std::shared_ptr<Expression> designator = nullptr)
      : designator(std::move(designator)) {}

  bool allowsDesignator() const override { return true; }

  void setDesignator(std::shared_ptr<Expression> d) override { designator = d; }

  [[nodiscard]] virtual std::shared_ptr<Expression>
  getDesignator() const override {
    return designator;
  }

  void accept(TypeVisitor* visitor) override { visitor->visitQubitType(this); }
};
} // namespace qasm3
