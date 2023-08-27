#include "QuantumComputation.hpp"
#include "operations/Operation.hpp"
#include "parsers/qasm3_parser/Gate.hpp"
#include "parsers/qasm3_parser/InstVisitor.hpp"
#include "parsers/qasm3_parser/Parser.hpp"
#include "parsers/qasm3_parser/Statement.hpp"

#include <iostream>
#include <utility>

using namespace qasm3;

class OpenQasm3Parser : public InstVisitor {
  std::shared_ptr<Environment> declarations;
  bool hasError{false};
  qc::QuantumComputation* qc;

  std::vector<std::unique_ptr<qc::Operation>> ops{};

  std::map<std::string, std::shared_ptr<Gate>> gates = {
      {"gphase",
       std::make_shared<StandardGate>(StandardGate({0, 0, 1, qc::GPhase}))},
      {"U", std::make_shared<StandardGate>(StandardGate({0, 1, 3, qc::U3}))},
      //            {"i", std::make_shared<StandardGate>(StandardGate({0, 1, 0,
      //            qc::I}))},
      //            {"id", std::make_shared<StandardGate>(StandardGate({0, 1, 0,
      //            qc::I}))},
      //            {"x", std::make_shared<StandardGate>(StandardGate({0, 1, 0,
      //            qc::X}))},
      //            {"y", std::make_shared<StandardGate>(StandardGate({0, 1, 0,
      //            qc::Y}))},
      //            {"z", std::make_shared<StandardGate>(StandardGate({0, 1, 0,
      //            qc::Z}))},
      //            {"h", std::make_shared<StandardGate>(StandardGate({0, 1, 0,
      //            qc::H}))},
      //            {"s", std::make_shared<StandardGate>(StandardGate({0, 1, 0,
      //            qc::S}))},
      //            {"sdg", std::make_shared<StandardGate>(StandardGate({0, 1,
      //            0, qc::Sdag}))},
      //            {"t", std::make_shared<StandardGate>(StandardGate({0, 1, 0,
      //            qc::T}))},
      //            {"tdg", std::make_shared<StandardGate>(StandardGate({0, 1,
      //            0, qc::Tdag}))},
      //            {"sx", std::make_shared<StandardGate>(StandardGate({0, 1, 0,
      //            qc::SX}))},
      //            {"sxdg", std::make_shared<StandardGate>(StandardGate({0, 1,
      //            0, qc::SXdag}))},
      //            {"rx", std::make_shared<StandardGate>(StandardGate({0, 1, 1,
      //            qc::RX}))},
      //            {"ry", std::make_shared<StandardGate>(StandardGate({0, 1, 1,
      //            qc::RY}))},
      //            {"rz", std::make_shared<StandardGate>(StandardGate({0, 1, 1,
      //            qc::RZ}))},
      //            {"u1", std::make_shared<StandardGate>(StandardGate({0, 1, 1,
      //            qc::Phase}))},
      //            {"p", std::make_shared<StandardGate>(StandardGate({0, 1, 1,
      //            qc::Phase}))},
      //            {"u2", std::make_shared<StandardGate>(StandardGate({0, 1, 2,
      //            qc::U2}))},
      //            {"u3", std::make_shared<StandardGate>(StandardGate({0, 1, 3,
      //            qc::U3}))},
      //            {"u", std::make_shared<StandardGate>(StandardGate({0, 1, 3,
      //            qc::U3}))},
      //            {"teleport", std::make_shared<StandardGate>(StandardGate({0,
      //            3, 0, qc::Teleportation}))},
      //            {"swap", std::make_shared<StandardGate>(StandardGate({0, 2,
      //            0, qc::SWAP}))},
      //            {"iswap", std::make_shared<StandardGate>(StandardGate({0, 2,
      //            0, qc::iSWAP}))},
      //            {"cnot", std::make_shared<StandardGate>(StandardGate({1, 1,
      //            0, qc::X}))},
      //            {"CX", std::make_shared<StandardGate>(StandardGate({1, 1, 0,
      //            qc::X}))},
      //            {"cx", std::make_shared<StandardGate>(StandardGate({1, 1, 0,
      //            qc::X}))},
      //            {"dcx", std::make_shared<StandardGate>(StandardGate({0, 2,
      //            0, qc::DCX}))},
      //            {"ecr", std::make_shared<StandardGate>(StandardGate({0, 2,
      //            0, qc::ECR}))},
      //            {"rxx", std::make_shared<StandardGate>(StandardGate({0, 2,
      //            1, qc::RXX}))},
      //            {"ryy", std::make_shared<StandardGate>(StandardGate({0, 2,
      //            1, qc::RYY}))},
      //            {"rzz", std::make_shared<StandardGate>(StandardGate({0, 2,
      //            1, qc::RZZ}))},
      //            {"rzx", std::make_shared<StandardGate>(StandardGate({0, 2,
      //            1, qc::RZX}))},
      //            {"xx_minus_yy",
      //            std::make_shared<StandardGate>(StandardGate({0, 2, 2,
      //            qc::XXminusYY}))},
      //            {"xx_plus_yy",
      //            std::make_shared<StandardGate>(StandardGate({0, 2, 2,
      //            qc::XXplusYY}))},
  };

  void error(const std::string& message,
             const std::shared_ptr<DebugInfo>& debugInfo) {
    std::cerr << debugInfo->toString();

    auto parentDebugInfo = debugInfo->parent;
    while (parentDebugInfo != nullptr) {
      std::cerr << "\n  (included from " << parentDebugInfo->toString() << ")";
      parentDebugInfo = parentDebugInfo->parent;
    }

    std::cerr << ":\n" << message << "\n\n";
    hasError = true;
  }

  void initializeBuiltins() {
    auto addConstantDeclaration = [this](const std::string& identifier,
                                         Declaration::Type type, uint8_t width,
                                         std::shared_ptr<Expression> expr) {
      auto decl = Declaration{identifier, std::move(expr), type, width, true};
      declarations->emplace(identifier, decl);
      return decl;
    };

    addConstantDeclaration("pi", Declaration::Type::Float, 1,
                           std::make_shared<ConstantFP>(ConstantFP(qc::PI)));
    addConstantDeclaration("π", Declaration::Type::Float, 1,
                           std::make_shared<ConstantFP>(ConstantFP(qc::PI)));
  }

  bool translateGateOperand(const std::shared_ptr<GateOperand>& gateOperand,
                            std::vector<qc::Qubit>& qubits,
                            const qc::QuantumRegisterMap& qregs,
                            const std::shared_ptr<DebugInfo>& debugInfo) {
    return translateGateOperand(gateOperand->identifier,
                                gateOperand->expression, qubits, qregs,
                                debugInfo);
  }

  bool translateGateOperand(const std::string& gateIdentifier,
                            const std::shared_ptr<Expression>& indexExpr,
                            std::vector<qc::Qubit>& qubits,
                            const qc::QuantumRegisterMap& qregs,
                            const std::shared_ptr<DebugInfo>& debugInfo) {
    auto qubitIter = qregs.find(gateIdentifier);
    if (qubitIter == qregs.end()) {
      error("Usage of unknown quantum register.", debugInfo);
      return false;
    }
    auto qubit = qubitIter->second;

    if (indexExpr != nullptr) {
      ConstExpressionPropagation visitor(declarations);
      try {
        auto result = std::any_cast<std::shared_ptr<Constant>>(
            indexExpr->accept(&visitor));

        if (result == nullptr || !result->isInt()) {
          error("Index expression must be a constant integer.", debugInfo);
          return false;
        }
        if (result->getInt() < 0) {
          error("Index expression must be a positive integer.", debugInfo);
          return false;
        }
        if (static_cast<uint64_t>(result->getInt()) > qubit.second) {
          error("Index expression must be smaller than the width of the "
                "quantum register.",
                debugInfo);
          return false;
        }

        qubit.first += static_cast<uint64_t>(result->getInt());
        qubit.second = 1;
      } catch (TypeCheckException& e) {
        error(e.what(), debugInfo);
        return false;
      }
    }

    for (uint64_t i = 0; i < qubit.second; ++i) {
      qubits.emplace_back(qubit.first + i);
    }

    return true;
  }

  bool translateBitOperand(const std::string& bitIdentifier,
                           const std::shared_ptr<Expression>& indexExpr,
                           std::vector<qc::Bit>& bits,
                           const std::shared_ptr<DebugInfo>& debugInfo) {
    auto iter = qc->getCregs().find(bitIdentifier);
    if (iter == qc->getCregs().end()) {
      error("Usage of unknown classical register.", debugInfo);
      return false;
    }
    auto creg = iter->second;

    if (indexExpr != nullptr) {
      ConstExpressionPropagation visitor(declarations);
      try {
        auto result = std::any_cast<std::shared_ptr<Constant>>(
            indexExpr->accept(&visitor));

        if (result == nullptr || !result->isInt()) {
          error("Index expression must be a constant integer.", debugInfo);
          return false;
        }
        if (result->getInt() < 0) {
          error("Index expression must be a positive integer.", debugInfo);
          return false;
        }
        if (static_cast<uint64_t>(result->getInt()) > creg.second) {
          error("Index expression must be smaller than the width of the "
                "classical register.",
                debugInfo);
          return false;
        }

        creg.first += static_cast<uint64_t>(result->getInt());
        creg.second = 1;
      } catch (TypeCheckException& e) {
        error(e.what(), debugInfo);
        return false;
      }
    }

    for (uint64_t i = 0; i < creg.second; ++i) {
      bits.emplace_back(creg.first + i);
    }

    return true;
  }

public:
  explicit OpenQasm3Parser(qc::QuantumComputation* qc)
      : declarations(std::make_shared<Environment>()), qc(qc) {
    initializeBuiltins();
  }

  ~OpenQasm3Parser() override = default;

  bool visitProgram(const std::vector<std::shared_ptr<Statement>>& program) {
    // TODO: in the future, don't exit early, but collect all errors
    // To do this, we need to insert make sure that erroneous declarations
    // actually insert a dummy entry; also, we need to synchronize to the next
    // semicolon, to make sure we don't do some weird stuff and report false
    // errors.
    return std::all_of(program.begin(), program.end(),
                       [this](const auto& statement) {
                         statement->accept(this);
                         return !hasError;
                       });
  }

  void visitVersionDeclaration(
      std::shared_ptr<VersionDeclaration> /*versionDeclaration*/) override {
    // TODO: do something with the version declaration
  }

  void visitDeclarationStatement(
      std::shared_ptr<DeclarationStatement> declarationStatement) override {
    auto identifier = declarationStatement->identifier;
    if (declarations->find(identifier) != declarations->end()) {
      // TODO: show the location of the previous declaration
      error("Identifier '" + identifier + "' already declared.",
            declarationStatement->debugInfo);
      return;
    }

    uint8_t designator = 1;
    if (auto designatorExpr = declarationStatement->type->getDesignator()) {
      ConstExpressionPropagation visitor(declarations);
      try {
        auto result = std::any_cast<std::shared_ptr<Constant>>(
            designatorExpr->accept(&visitor));

        if (result == nullptr) {
          error("Designator expression must be a constant integer.",
                declarationStatement->debugInfo);
          return;
        }
        if (!result->isInt()) {
          error("Designator expression must be a constant integer.",
                declarationStatement->debugInfo);
          return;
        }
        if (result->getInt() <= 0) {
          error("Designator expression must be a positive integer > 0.",
                declarationStatement->debugInfo);
          return;
        }

        declarationStatement->type->setDesignator(result);
        designator = static_cast<uint8_t>(result->getInt());
      } catch (TypeCheckException& e) {
        error(e.what(), declarationStatement->debugInfo);
        return;
      }
    }

    class TyVisitor : public TypeVisitor {
    private:
      OpenQasm3Parser* parser;
      qc::QuantumComputation* qc;
      std::string identifier;
      uint8_t designator;
      std::shared_ptr<Environment> env;
      std::shared_ptr<DeclarationStatement> declarationStatement;

      void insertDeclaration(Declaration::Type type) {
        std::shared_ptr<Expression> expr{};
        if (declarationStatement->expression != nullptr) {
          expr = declarationStatement->expression->expression;
        }
        env->emplace(identifier, Declaration{identifier, expr, type, designator,
                                             declarationStatement->isConst});
      }

    public:
      explicit TyVisitor(
          OpenQasm3Parser* parser, qc::QuantumComputation* qc,
          std::string identifier, uint8_t designator,
          std::shared_ptr<Environment> env,
          std::shared_ptr<DeclarationStatement> declarationStatement)
          : parser(parser), qc(qc), identifier(std::move(identifier)),
            designator(designator), env(std::move(env)),
            declarationStatement(std::move(declarationStatement)) {}

      void visitBitType(BitType* /*type*/) override {
        qc->addClassicalRegister(designator, identifier);
        insertDeclaration(Declaration::Type::Bit);
      }
      void visitIntType(IntType* /*type*/) override {
        insertDeclaration(Declaration::Type::Int);
      }
      void visitUintType(UintType* /*type*/) override {
        insertDeclaration(Declaration::Type::Uint);
      }
      void visitFloatType(FloatType* /*type*/) override {
        insertDeclaration(Declaration::Type::Float);
      }
      void visitAngleType(AngleType* /*type*/) override {
        parser->error("Angle type not supported yet.",
                      declarationStatement->debugInfo);
      }
      void visitBoolType(BoolType* /*type*/) override {
        insertDeclaration(Declaration::Type::Bool);
      }
      void visitDurationType(DurationType* /*type*/) override {
        parser->error("Duration type not supported yet.",
                      declarationStatement->debugInfo);
      }
      void visitStretchType(StretchType* /*type*/) override {
        parser->error("Stretch type not supported yet.",
                      declarationStatement->debugInfo);
      }
      void visitComplexType(ComplexType* /*type*/) override {
        parser->error("Complex type not supported yet.",
                      declarationStatement->debugInfo);
      }
      void visitQubitType(QubitType* /*type*/) override {
        if (declarationStatement->isConst) {
          parser->error("Cannot declare a constant qubit register.",
                        declarationStatement->debugInfo);
          return;
        }
        qc->addQubitRegister(designator, identifier);
        insertDeclaration(Declaration::Type::Qubit);
      }
    };

    TyVisitor tyVisitor(this, qc, identifier, designator, declarations,
                        declarationStatement);
    declarationStatement->type->accept(&tyVisitor);

    // for now, only support measure initialization and constant initialization
    if (declarationStatement->isConst) {
      ConstExpressionPropagation visitor(declarations);
      auto result = std::any_cast<std::shared_ptr<Constant>>(
          declarationStatement->expression->accept(&visitor));
      if (result == nullptr) {
        error("Const variable initialized with non-const value.",
              declarationStatement->debugInfo);
        return;
      }
      // immediately evaluate the expression and store the result
      declarations->at(identifier).expression = result;
    } else if (declarationStatement->expression != nullptr) {
      if (auto measureExpression = std::dynamic_pointer_cast<MeasureExpression>(
              declarationStatement->expression->expression)) {
        if (declarationStatement->isConst) {
          error("Cannot initialize a const register with a measure statement.",
                declarationStatement->debugInfo);
          return;
        }
        visitMeasureAssignment(declarations->at(identifier), nullptr,
                               measureExpression,
                               declarationStatement->debugInfo);
      }
    }
  }

  void visitAssignmentStatement(
      std::shared_ptr<AssignmentStatement> assignmentStatement) override {
    auto identifier = assignmentStatement->identifier->identifier;
    auto declaration = declarations->find(identifier);
    if (declaration == declarations->end()) {
      error("Usage of unknown identifier '" + identifier + "'.",
            assignmentStatement->debugInfo);
      return;
    }

    if (declaration->second.isConst) {
      error("Assignment to constant identifier '" + identifier + "'.",
            assignmentStatement->debugInfo);
      return;
    }

    if (auto measureExpression = std::dynamic_pointer_cast<MeasureExpression>(
            assignmentStatement->expression->expression)) {
      visitMeasureAssignment(declaration->second,
                             assignmentStatement->indexExpression,
                             measureExpression, assignmentStatement->debugInfo);
      return;
    }

    // In the future, handle classical computation.
    error("Classical computation not supported.",
          assignmentStatement->debugInfo);
  }

  void
  visitInitialLayout(std::shared_ptr<InitialLayout> initialLayout) override {
    if (!qc->initialLayout.empty()) {
      error("Multiple initial layout specifications found.",
            initialLayout->debugInfo);
      return;
    }
    qc->initialLayout = initialLayout->permutation;
  }

  void visitOutputPermutation(
      std::shared_ptr<OutputPermutation> outputPermutation) override {
    if (!qc->outputPermutation.empty()) {
      error("Multiple output permutation specifications found.",
            outputPermutation->debugInfo);
      return;
    }
    qc->outputPermutation = outputPermutation->permutation;
  }

  void
  visitGateStatement(std::shared_ptr<GateDeclaration> gateStatement) override {
    auto identifier = gateStatement->identifier;
    if (gates.find(identifier) != gates.end()) {
      // TODO: print location of previous declaration
      error("Gate '" + identifier + "' already declared.",
            gateStatement->debugInfo);
      return;
    }

    auto parameters = gateStatement->parameters;
    auto qubits = gateStatement->qubits;

    // first we check that all parameters and qubits are unique
    std::vector<std::string> parameterIdentifiers{};
    for (const auto& parameter : parameters->identifiers) {
      if (std::find(parameterIdentifiers.begin(), parameterIdentifiers.end(),
                    parameter->identifier) != parameterIdentifiers.end()) {
        error("Parameter '" + parameter->identifier + "' already declared.",
              gateStatement->debugInfo);
        return;
      }
      parameterIdentifiers.emplace_back(parameter->identifier);
    }
    std::vector<std::string> qubitIdentifiers{};
    for (const auto& qubit : qubits->identifiers) {
      if (std::find(qubitIdentifiers.begin(), qubitIdentifiers.end(),
                    qubit->identifier) != qubitIdentifiers.end()) {
        error("Qubit '" + qubit->identifier + "' already declared.",
              gateStatement->debugInfo);
        return;
      }
      qubitIdentifiers.emplace_back(qubit->identifier);
    }

    std::unique_ptr<Environment> globalEnv = std::make_unique<Environment>();
    for (const auto& decl : *declarations) {
      globalEnv->emplace(decl.first, decl.second);
    }

    auto compoundGate = std::make_shared<CompoundGate>(
        CompoundGate(parameterIdentifiers, qubitIdentifiers,
                     gateStatement->statements, std::move(globalEnv)));

    gates.emplace(identifier, compoundGate);
  }

  void visitGateCallStatement(
      std::shared_ptr<GateCallStatement> gateCallStatement) override {
    if (gates.find(gateCallStatement->identifier) == gates.end()) {
      error("Gate '" + gateCallStatement->identifier + "' not declared.",
            gateCallStatement->debugInfo);
      return;
    }

    auto qregs = qc->getQregs();

    auto op =
        evaluateGateCall(gateCallStatement, gateCallStatement->identifier,
                         gateCallStatement->arguments,
                         gateCallStatement->operands, declarations, qregs);
    if (op != nullptr) {
      qc->emplace_back(std::move(op));
    }
  }

  std::unique_ptr<qc::Operation>
  evaluateGateCall(const std::shared_ptr<GateCallStatement>& gateCallStatement,
                   const std::string& identifier,
                   const std::vector<std::shared_ptr<Expression>>& parameters,
                   std::vector<std::shared_ptr<GateOperand>> targets,
                   const std::shared_ptr<Environment>& env,
                   qc::QuantumRegisterMap& qregs,
                   bool invertOperation = false) {
    auto iter = gates.find(identifier);
    if (iter == gates.end()) {
      error("Usage of unknown gate '" + identifier + "'.",
            gateCallStatement->debugInfo);
      return nullptr;
    }
    auto gate = iter->second;

    if (gate->getNParameters() != parameters.size()) {
      error("Gate '" + identifier + "' takes " +
                std::to_string(gate->getNParameters()) + " parameters, but " +
                std::to_string(parameters.size()) + " were supplied.",
            gateCallStatement->debugInfo);
      return nullptr;
    }

    // here we count the number of controls
    std::vector<std::pair<std::shared_ptr<GateOperand>, bool>> controls;
    // since standard gates may define a number of control targets, we first
    // need to handle those
    size_t nControls{gate->getNControls()};
    if (targets.size() < nControls) {
      error("Gate '" + identifier + "' takes " + std::to_string(nControls) +
                " controls, but only " + std::to_string(targets.size()) +
                " were supplied.",
            gateCallStatement->debugInfo);
      return nullptr;
    }

    for (size_t i = 0; i < nControls; ++i) {
      controls.emplace_back(targets[i], true);
    }

    for (const auto& modifier : gateCallStatement->modifiers) {
      if (auto* ctrlModifier = dynamic_cast<CtrlGateModifier*>(modifier.get());
          ctrlModifier != nullptr) {
        size_t n{0};
        if (ctrlModifier->expression == nullptr) {
          n = 1;
        } else {
          ConstExpressionPropagation visitor{env};
          auto result = std::any_cast<std::shared_ptr<Constant>>(
              ctrlModifier->expression->accept(&visitor));
          if (result == nullptr) {
            error("Only const expressions are supported.",
                  gateCallStatement->debugInfo);
            return nullptr;
          }
          if (!result->isInt() || result->getInt() <= 0) {
            error("Only positive integer values are supported.",
                  gateCallStatement->debugInfo);
            return nullptr;
          }
          n = static_cast<size_t>(result->getInt());
        }
        if (targets.size() < n + nControls) {
          error("Gate '" + identifier + "' takes " +
                    std::to_string(n + nControls) + " controls, but only " +
                    std::to_string(targets.size()) + " were supplied.",
                gateCallStatement->debugInfo);
          return nullptr;
        }

        for (size_t i = 0; i < n; ++i) {
          controls.emplace_back(targets[nControls + i], ctrlModifier->ctrlType);
        }
        nControls += n;
      } else if (auto* invModifier =
                     dynamic_cast<InvGateModifier*>(modifier.get());
                 invModifier != nullptr) {
        // if we have an even number of inv modifiers, they cancel each other
        // out
        invertOperation = !invertOperation;
      } else {
        error("Only ctrl/negctrl/inv modifiers are supported.",
              gateCallStatement->debugInfo);
        return nullptr;
      }
    }
    targets.erase(targets.begin(),
                  targets.begin() + static_cast<int64_t>(nControls));

    if (gate->getNTargets() != targets.size()) {
      error("Gate '" + identifier + "' takes " +
                std::to_string(gate->getNTargets()) + " targets, but " +
                std::to_string(targets.size()) + " were supplied.",
            gateCallStatement->debugInfo);
      return nullptr;
    }

    // now evaluate all arguments; we only support const arguments.
    std::vector<qc::fp> evaluatedParameters;
    ConstExpressionPropagation visitor{env};
    for (const auto& param : parameters) {
      auto result =
          std::any_cast<std::shared_ptr<Constant>>(param->accept(&visitor));

      if (!result) {
        error("Only const expressions are supported.",
              gateCallStatement->debugInfo);
        return nullptr;
      }

      evaluatedParameters.emplace_back(result->getFP());
    }

    size_t broadcastingWidth{1};
    qc::Targets targetBits{};
    std::vector<size_t> targetBroadcastingIndices{};
    size_t i{0};
    for (const auto& target : targets) {
      qc::Targets t{};
      if (!translateGateOperand(target, t, qregs,
                                gateCallStatement->debugInfo)) {
        return nullptr;
      }

      targetBits.emplace_back(t[0]);

      if (t.size() > 1) {
        if (broadcastingWidth != 1 && t.size() != broadcastingWidth) {
          error("When broadcasting, all registers must be of the same width.",
                gateCallStatement->debugInfo);
          return nullptr;
        }
        broadcastingWidth = t.size();

        targetBroadcastingIndices.emplace_back(i);
      }

      i++;
    }

    std::vector<qc::Control> controlBits{};
    std::vector<size_t> controlBroadcastingIndices{};
    i = 0;
    for (const auto& [control, type] : controls) {
      qc::Targets c{};
      if (!translateGateOperand(control, c, qregs,
                                gateCallStatement->debugInfo)) {
        return nullptr;
      }

      controlBits.emplace_back(qc::Control{
          c[0], type ? qc::Control::Type::Pos : qc::Control::Type::Neg});

      if (c.size() > 1) {
        if (broadcastingWidth != 1 && c.size() != broadcastingWidth) {
          error("When broadcasting, all registers must be of the same width.",
                gateCallStatement->debugInfo);
          return nullptr;
        }
        broadcastingWidth = c.size();

        controlBroadcastingIndices.emplace_back(i);
      }

      i++;
    }

    auto applyGateOperation =
        [this, &gate, &evaluatedParameters, &env, &gateCallStatement,
         &invertOperation](qc::Targets targetBits,
                           std::vector<qc::Control> controlBits)
        -> std::unique_ptr<qc::Operation> {
      if (auto* standardGate = dynamic_cast<StandardGate*>(gate.get())) {
        std::vector<qc::fp> params = evaluatedParameters;
        if (invertOperation) {
          for (auto& param : params) {
            param = -param;
          }
        }
        return std::make_unique<qc::StandardOperation>(
            qc->getNqubits(),
            qc::Controls{controlBits.begin(), controlBits.end()}, targetBits,
            standardGate->info.type, params);
      }
      if (auto* compoundGate = dynamic_cast<CompoundGate*>(gate.get())) {
        auto nestedEnv = std::make_shared<Environment>();

        for (auto& globalDecl : *compoundGate->globalEnv) {
          nestedEnv->emplace(globalDecl.first, globalDecl.second);
        }

        for (const auto& paramIdentifier : compoundGate->parameterNames) {
          auto expr = env->find(paramIdentifier);
          if (expr == env->end()) {
            error("Parameter not found.", gateCallStatement->debugInfo);
            return nullptr;
          }

          nestedEnv->emplace(paramIdentifier, expr->second);
        }

        auto nestedQubits = qc::QuantumRegisterMap{};
        size_t index = 0;
        for (const auto& qubitIdentifier : compoundGate->targetNames) {
          auto qubit = std::pair{targetBits[index], 1};

          nestedQubits.emplace(qubitIdentifier, qubit);
          index++;
        }

        auto op = std::make_unique<qc::CompoundOperation>(qc->getNqubits());
        for (const auto& nestedGate : compoundGate->body) {
          for (const auto& operand : nestedGate->operands) {
            // OpenQASM 3.0 doesn't support indexing of gate arguments.
            if (operand->expression != nullptr &&
                std::find(compoundGate->targetNames.begin(),
                          compoundGate->targetNames.end(),
                          operand->identifier) !=
                    compoundGate->targetNames.end()) {
              error("Gate arguments cannot be indexed within gate body.",
                    gateCallStatement->debugInfo);
              return nullptr;
            }
          }

          auto nestedOp = evaluateGateCall(
              nestedGate, nestedGate->identifier, nestedGate->arguments,
              nestedGate->operands, nestedEnv, nestedQubits, invertOperation);
          if (nestedOp == nullptr) {
            return nullptr;
          }
          op->getOps().emplace_back(std::move(nestedOp));
        }
        op->setControls(qc::Controls{controlBits.begin(), controlBits.end()});
        if (invertOperation) {
          std::reverse(op->getOps().begin(), op->getOps().end());
        }

        return legalizeControls(std::move(op));
      }

      error("Unknown gate type.", gateCallStatement->debugInfo);
      return nullptr;
    };

    if (broadcastingWidth == 1) {
      return applyGateOperation(targetBits, controlBits);
    }

    // if we are broadcasting, we need to create a compound operation
    auto op = std::make_unique<qc::CompoundOperation>(qc->getNqubits());
    for (size_t j = 0; j < broadcastingWidth; ++j) {
      // first we apply the operation
      auto nestedOp = applyGateOperation(targetBits, controlBits);
      if (nestedOp == nullptr) {
        return nullptr;
      }
      op->getOps().emplace_back(std::move(nestedOp));

      // after applying the operation, we update the broadcast bits
      for (auto index : targetBroadcastingIndices) {
        targetBits[index] = qc::Qubit{targetBits[index] + 1};
      }
      for (auto index : controlBroadcastingIndices) {
        controlBits[index].qubit = qc::Qubit{controlBits[index].qubit + 1};
      }
    }
    return op;
  }

  void visitMeasureAssignment(
      Declaration& classicalReg,
      const std::shared_ptr<Expression>& indexExpression,
      const std::shared_ptr<MeasureExpression>& measureExpression,
      const std::shared_ptr<DebugInfo>& debugInfo) {
    if (classicalReg.type != Declaration::Type::Bit) {
      error("Measure expression can only be assigned to a bit register.",
            debugInfo);
      return;
    }

    std::vector<qc::Qubit> qubits{};
    std::vector<qc::Bit> bits{};
    if (!translateGateOperand(measureExpression->gate, qubits, qc->getQregs(),
                              debugInfo) ||
        !translateBitOperand(classicalReg.identifier, indexExpression, bits,
                             debugInfo)) {
      return;
    }

    if (qubits.size() != bits.size()) {
      error("Classical and quantum register must have the same width in "
            "measure statement.",
            debugInfo);
      return;
    }

    auto op = std::make_unique<qc::NonUnitaryOperation>(qc->getNqubits(),
                                                        qubits, bits);
    qc->emplace_back(std::move(op));
  }

  template <typename To, typename From, typename Deleter>
  std::unique_ptr<To, Deleter>
  dynamic_unique_cast(std::unique_ptr<From, Deleter>&& p) {
    if (To* cast = dynamic_cast<To*>(p.get())) {
      std::unique_ptr<To, Deleter> result(cast, std::move(p.get_deleter()));
      p.release();
      return result;
    }
    return std::unique_ptr<To, Deleter>(nullptr);
  }

  void pushDownControls(qc::CompoundOperation* op) {
    std::vector<std::unique_ptr<qc::Operation>> newOps{};
    newOps.reserve(op->getOps().size());
    for (auto& nestedOp : op->getOps()) {
      nestedOp->getControls().insert(op->getControls().begin(),
                                     op->getControls().end());
      newOps.push_back(legalizeControls(std::move(nestedOp)));
    }

    op->getOps() = std::move(newOps);

    // now that all controls have been pushed down, the compound operation has
    // no more controls
    op->setControls(qc::Controls{});
  }

  std::unique_ptr<qc::Operation>
  legalizeControls(std::unique_ptr<qc::Operation> op) {
    if (op->getControls().empty()) {
      return op;
    }

    if (auto compoundOp =
            dynamic_unique_cast<qc::CompoundOperation>(std::move(op))) {
      pushDownControls(compoundOp.get());
      return compoundOp;
    }
    if (auto standardOp =
            dynamic_unique_cast<qc::StandardOperation>(std::move(op))) {
      if (standardOp->getType() == qc::GPhase) {
        // a controlled `gphase a` is replaced with a single-qubit U(0, 0, a)
        // gate
        auto angle = standardOp->getParameter()[0];
        auto newOp = std::make_unique<qc::CompoundOperation>(qc->getNqubits());
        for (auto control : standardOp->getControls()) {
          auto newStdOp = std::make_unique<qc::StandardOperation>(
              qc->getNqubits(), qc::Controls{}, qc::Targets{control.qubit},
              qc::U3, std::vector<qc::fp>{0, 0, angle});
          newOp->emplace_back(newStdOp);
        }
        return newOp;
      }
      return standardOp;
    }

    return op;
  }

  void visitBarrierStatement(
      std::shared_ptr<BarrierStatement> barrierStatement) override {
    std::vector<qc::Qubit> qubits{};
    for (const auto& gate : barrierStatement->gates) {
      translateGateOperand(gate, qubits, qc->getQregs(),
                           barrierStatement->debugInfo);
    }

    auto op = std::make_unique<qc::NonUnitaryOperation>(qc->getNqubits(),
                                                        qubits, qc::Barrier);
    qc->emplace_back(std::move(op));
  }
  void
  visitResetStatement(std::shared_ptr<ResetStatement> resetStatement) override {
    std::vector<qc::Qubit> qubits{};
    translateGateOperand(resetStatement->gate, qubits, qc->getQregs(),
                         resetStatement->debugInfo);
    auto op = std::make_unique<qc::NonUnitaryOperation>(qc->getNqubits(),
                                                        qubits, qc::Reset);
    qc->emplace_back(std::move(op));
  }
};

void qc::QuantumComputation::importOpenQASM3(std::istream& is) {
  using namespace qasm3;

  Parser p(is);

  auto program = p.parseProgram();
  OpenQasm3Parser parser{this};
  if (!parser.visitProgram(program)) {
    throw std::runtime_error("Error importing OpenQASM.");
  }
}
