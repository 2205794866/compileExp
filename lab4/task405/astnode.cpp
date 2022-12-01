#include "astnode.h"

extern int spaces;
extern std::unique_ptr<LLVMContext> theContext;
extern std::unique_ptr<Module> theModule;
extern std::unique_ptr<IRBuilder<>> builder;
extern std::map<std::string, AllocaInst *> namedValues;
extern std::unique_ptr<legacy::FunctionPassManager> theFPM;
extern int grammererror;
extern std::map<std::string, AllocaInst *> curNamedValues;

/* --------------------begin--------------*/
// #define DEBUG
void debug(std::string s)
{
#ifdef DEBUG
  std::cout << s << std::endl;
#endif
}


struct LocalVarTable
{
  int level;
  std::map<std::string, AllocaInst *> localVar;
};

std::vector<LocalVarTable> varTable;
std::vector<LocalVarTable>::iterator varIte;
int level = -1;
bool funWithArg = false;
int findVar(std::string name)
{
  for(varIte = varTable.end() - 1; varIte >= varTable.begin(); varIte --)
  {
    if(varIte->localVar[name])
    return varIte->level;
  }
  return -1;
}



/* --------------------end---------------*/

extern BasicBlock *continueBasicBlock;
void printspaces() {
  for (int i = 0; i < spaces; ++i)
    std::cout << " ";
}
void printGrammerInfo(std::string nodeName, int line) {
  printspaces();
  std::cout << nodeName << " (" << line << ")" << std::endl;
}

void printSemanticError(int type, int line, std::string info = "") {
  grammererror = 1;
  std::cout << "Error type " << type << " at Line " << line << "."
            << std::endl;
}

int parseNIdentifier(NIdentifier &nIdentifier) {
  printspaces();
  std::cout << "ID: " << nIdentifier.name << std::endl;
  return 0;
}

Value *LogErrorV(const char *Str) {
  // std::cout << Str << std::endl;
  return nullptr;
}

void InitializeModuleAndPassManager() {
  // Open a new module.
  theContext = std::make_unique<LLVMContext>();
  theModule = std::make_unique<Module>("test", *theContext);

  // theModule->setDataLayout(dL);

  // Create a new builder for the module.
  builder = std::make_unique<IRBuilder<>>(*theContext);

  // Create a new pass manager attached to it.
  theFPM = std::make_unique<legacy::FunctionPassManager>(theModule.get());

  // Promote allocas to registers.
  //theFPM->add(createPromoteMemoryToRegisterPass());
  // Do simple "peephole" optimizations and bit-twiddling optzns.
  //theFPM->add(createInstructionCombiningPass());
  // Reassociate expressions.
  //theFPM->add(createReassociatePass());
  // Eliminate Common SubExpressions.
  //theFPM->add(createGVNPass());
  // Simplify the control flow graph (deleting unreachable blocks, etc).
  //theFPM->add(createCFGSimplificationPass());

  theFPM->doInitialization();
}

Function *getFunction(std::string Name) {
  // First, see if the function has already been added to the current module.
  if (auto *F = theModule->getFunction(Name))
    return F;
  return nullptr;
}

/// CreateEntryBlockAlloca - Create an alloca instruction in the entry block
/// of the function.  This is used for mutable variables etc.
static AllocaInst *CreateEntryBlockAlloca(Function *TheFunction,
                                          StringRef VarName, Type *varType) {
  IRBuilder<> TmpB(&TheFunction->getEntryBlock(),
                   TheFunction->getEntryBlock().begin());
  return TmpB.CreateAlloca(varType, nullptr, VarName);
}

int NInteger::parse() {
  printGrammerInfo(getNodeName(), line);
  spaces += 2;
  printspaces();
  std::cout << "INT"
            << ": " << value << std::endl;
  spaces -= 2;
  return 0;
}
int NFloat::parse() {
  printGrammerInfo(getNodeName(), line);
  spaces += 2;
  printspaces();
  std::cout << "FLOAT"
            << ": " << value << std::endl;
  spaces -= 2;
  return 0;
}
int NChar::parse() {
  printGrammerInfo(getNodeName(), line);
  spaces += 2;
  printspaces();
  std::cout << "CHAR"
            << ": " << value << std::endl;
  spaces -= 2;
  return 0;
}
int NIdentifier::parse() {
  printGrammerInfo(getNodeName(), line);

  spaces += 2;
  printspaces();
  std::cout << "ID"
            << ": " << name << std::endl;
  spaces -= 2;
  return 0;
}
int NDotOperator::parse() {
  printGrammerInfo(getNodeName(), line);
  spaces += 2;
  exp.parse();
  printspaces();
  std::cout << "DOT" << std::endl;
  parseNIdentifier(id);
  // id.parse();
  spaces -= 2;
  return 0;
}
int NListOperator::parse() {
  printGrammerInfo(getNodeName(), line);
  spaces += 2;
  lhs.parse();
  printspaces();
  std::cout << "LB" << std::endl;
  rhs.parse();
  printspaces();
  std::cout << "RB" << std::endl;
  spaces -= 2;
  return 0;
}
int NArgs::parse() {
  printGrammerInfo(getNodeName(), line);

  spaces += 2;
  exp.parse();
  if (nArgs) {
    printspaces();
    std::cout << "COMMA" << std::endl;
    nArgs->parse();
  }
  spaces -= 2;
  return 0;
}
int NMethodCall::parse() {
  printGrammerInfo(getNodeName(), line);

  spaces += 2;
  parseNIdentifier(id);
  // id.parse();
  printspaces();
  std::cout << "LP" << std::endl;
  if (nargs) {
    nargs->parse();
  }
  printspaces();
  std::cout << "RP" << std::endl;
  spaces -= 2;
  return 0;
}
int NParenOperator::parse() {
  printGrammerInfo(getNodeName(), line);

  spaces += 2;
  printspaces();
  std::cout << "LP" << std::endl;
  printspaces();
  exp.parse();
  printspaces();
  std::cout << "RP" << std::endl;
  spaces -= 2;
  return 0;
}
int NSingleOperator::parse() {
  printGrammerInfo(getNodeName(), line);

  spaces += 2;
  printspaces();
  std::cout << name << std::endl;
  hs.parse();
  spaces -= 2;
  return 0;
}
int NBinaryOperator::parse() {
  printGrammerInfo(getNodeName(), line);

  spaces += 2;
  lhs.parse();
  printspaces();
  if (name.substr(0, 5) == "RELOP")
    std::cout << "RELOP" << std::endl;
  else
    std::cout << name << std::endl;
  rhs.parse();
  spaces -= 2;
  return 0;
}
int NAssignment::parse() {
  printGrammerInfo(getNodeName(), line);

  spaces += 2;
  lhs.parse();
  printspaces();
  std::cout << name << std::endl;
  rhs.parse();
  spaces -= 2;
  return 0;
}
int NSpecifier::parse() {
  printGrammerInfo(getNodeName(), line);

  spaces += 2;
  printspaces();
  std::cout << "TYPE: " << type << std::endl;
  spaces -= 2;
  return 0;
}
int NVarDec::parse() {
  printGrammerInfo(getNodeName(), line);

  if (v.size()) {
    spaces += 2;
    for (int i = 0; i < v.size(); ++i) {
      printGrammerInfo(getNodeName(), line);

      spaces += 2;
    }
    parseNIdentifier(Id);
    // Id.parse();
    spaces -= 2;
    for (int i = 0; i < v.size(); ++i) {
      printspaces();
      std::cout << "LB" << std::endl;
      printspaces();
      std::cout << "INT: " << v[i] << std::endl;
      printspaces();
      std::cout << "RB" << std::endl;
      spaces -= 2;
    }
  } else {
    spaces += 2;
    parseNIdentifier(Id);
    // Id.parse();
    spaces -= 2;
  }
  return 0;
}
int NParamDec::parse() {
  printGrammerInfo(getNodeName(), line);

  spaces += 2;
  nSpecifier.parse();
  varDec.parse();
  spaces -= 2;
  return 0;
}
int NVarList::parse() {
  printGrammerInfo(getNodeName(), line);

  spaces += 2;
  nParamDec.parse();
  if (nVarList) {
    printspaces();
    std::cout << "COMMA" << std::endl;
    nVarList->parse();
  }
  spaces -= 2;
  return 0;
}
int NFunDec::parse() {
  printGrammerInfo(getNodeName(), line);

  spaces += 2;
  parseNIdentifier(Id);
  // Id.parse();
  printspaces();
  std::cout << "LP" << std::endl;
  if (arguments)
    arguments->parse();
  printspaces();
  std::cout << "RP" << std::endl;
  spaces -= 2;
  return 0;
}
int NDec::parse() {
  printGrammerInfo(getNodeName(), line);

  spaces += 2;
  vardec.parse();
  if (exp) {
    printspaces();
    std::cout << "ASSIGNOP" << std::endl;
    exp->parse();
  }
  spaces -= 2;
  return 0;
}
int NDecList::parse() {
  printGrammerInfo(getNodeName(), line);

  spaces += 2;
  dec.parse();
  if (nDecList) {
    printspaces();
    std::cout << "COMMA" << std::endl;
    nDecList->parse();
  }
  spaces -= 2;
  return 0;
}
int NDef::parse() {
  printGrammerInfo(getNodeName(), line);

  spaces += 2;
  nSpecifier.parse();
  if (nDecList)
    nDecList->parse();
  printspaces();
  std::cout << "SEMI" << std::endl;
  spaces -= 2;
  return 0;
}
int NDefList::parse() {
  printGrammerInfo(getNodeName(), line);

  spaces += 2;
  nDef.parse();
  if (nDefList) {
    nDefList->parse();
  }
  spaces -= 2;
  return 0;
}
int NStructSpecifier::parse() {
  printGrammerInfo(getNodeName(), line);

  spaces += 2;
  printGrammerInfo("StructSpecifier", line);

  spaces += 2;
  printspaces();
  std::cout << "STRUCT" << std::endl;
  if (deflist) {
    if (tag) {
      printGrammerInfo("OptTag", line);
      spaces += 2;
      parseNIdentifier(*tag);
      spaces -= 2;
      printspaces();
      std::cout << "LC" << std::endl;
      deflist->parse();
      printspaces();
      std::cout << "RC" << std::endl;
    } else {
      deflist->parse();
    }
  } else if (tag) {
    printGrammerInfo("Tag", line);

    spaces += 2;
    parseNIdentifier(*tag);
    spaces -= 2;
  }
  spaces -= 2;
  spaces -= 2;
  return 0;
}
int NStmtList::parse() {
  printGrammerInfo(getNodeName(), line);

  spaces += 2;
  nStmt.parse();
  if (nStmtList)
    nStmtList->parse();
  spaces -= 2;
  return 0;
}

int NCompSt::parse() {
  printGrammerInfo(getNodeName(), line);

  spaces += 2;
  printspaces();
  std::cout << "LC" << std::endl;
  if (ndeflist)
    ndeflist->parse();
  if (nstmtlist)
    nstmtlist->parse();
  printspaces();
  std::cout << "RC" << std::endl;
  spaces -= 2;
  return 0;
}
int NExpStmt::parse() {
  printGrammerInfo(getNodeName(), line);

  spaces += 2;
  this->exp.parse();
  printspaces();
  std::cout << "SEMI" << std::endl;
  spaces -= 2;
  return 0;
}
int NCompStStmt::parse() {
  printGrammerInfo(getNodeName(), line);

  spaces += 2;
  compst.parse();
  spaces -= 2;
  return 0;
}
int NRetutnStmt::parse() {
  printGrammerInfo(getNodeName(), line);

  spaces += 2;
  printspaces();
  std::cout << "RETURN" << std::endl;
  this->exp.parse();
  printspaces();
  std::cout << "SEMI" << std::endl;
  spaces -= 2;
  return 0;
}
int NIfStmt::parse() {
  printGrammerInfo(getNodeName(), line);

  spaces += 2;
  printspaces();
  std::cout << "IF" << std::endl;
  printspaces();
  std::cout << "LP" << std::endl;
  this->exp.parse();
  printspaces();
  std::cout << "RP" << std::endl;
  this->stmt.parse();
  spaces -= 2;
  return 0;
}
int NIfElseStmt::parse() {
  printGrammerInfo(getNodeName(), line);

  spaces += 2;
  printspaces();
  std::cout << "IF" << std::endl;
  printspaces();
  std::cout << "LP" << std::endl;
  this->exp.parse();
  printspaces();
  std::cout << "RP" << std::endl;
  this->stmt.parse();
  printspaces();
  std::cout << "ELSE" << std::endl;
  this->stmt_else.parse();
  spaces -= 2;
  return 0;
}
int NWhileStmt::parse() {
  printGrammerInfo(getNodeName(), line);

  spaces += 2;
  printspaces();
  std::cout << "WHILE" << std::endl;
  printspaces();
  std::cout << "LP" << std::endl;
  this->exp.parse();
  printspaces();
  std::cout << "RP" << std::endl;
  this->stmt.parse();
  spaces -= 2;
  return 0;
}
int NBreakStmt::parse() {
  printGrammerInfo(getNodeName(), line);

  spaces += 2;
  printspaces();
  std::cout << "BREAK" << std::endl;
  printspaces();
  std::cout << "SEMI" << std::endl;
  spaces -= 2;
  return 0;
}
int NExtDecList::parse() {
  printGrammerInfo(getNodeName(), line);

  spaces += 2;
  nVarDec.parse();
  if (nExtDecList) {
    printspaces();
    std::cout << "COMMA" << std::endl;
    nExtDecList->parse();
  }
  spaces -= 2;
  return 0;
}
int NExtDef::parse() {
  printGrammerInfo(getNodeName(), line);

  spaces += 2;
  specifier.parse();
  if (fundec) {
    fundec->parse();
    if (compst) {
      compst->parse();
    }
  } else {
    if (nextdeclist) {
      nextdeclist->parse();
    }
    printspaces();
    std::cout << "SEMI" << std::endl;
  }

  spaces -= 2;
  return 0;
}
int NExtDefList::parse() {
  printGrammerInfo(getNodeName(), line);

  spaces += 2;
  nExtDef.parse();
  if (nExtDefList)
    nExtDefList->parse();
  spaces -= 2;
  return 0;
}
int NProgram::parse() {
  printGrammerInfo("Program", line);
  spaces += 2;
  if (nextdeflist)
    nextdeflist->parse();
  spaces -= 2;
  return 0;
}

// codegen()
Value *Node::codegen() {
  assert(false); // Never use this function.
  // This is a list.
  return ConstantInt::get(*theContext, APInt(32, 0, true));
}
Value *NExpression::codegen() {
  return ConstantInt::get(*theContext, APInt(32, 0, true));
}
Value *NInteger::codegen() {
  return ConstantInt::get(*theContext, APInt(32, value, true));
}    
Value *NFloat::codegen() {
  // begin
  return ConstantFP::get(*theContext, APFloat(value));
  // end
}
Value *NChar::codegen() {
  // begin
  debug("NChar");
  return ConstantInt::get(*theContext, APInt(32, value, true));
  // end
}
Value *NIdentifier::codegen() {
  debug(std::string("NIdentifier") + ":" + name);
  // begin
  Value *retVal;
  // 从底层往上查找
  int varLevel = -1;
  if((varLevel = findVar(name)) != -1)
  {
    retVal = builder->CreateLoad(varTable[varLevel].localVar[std::string(name)], "load");
  }
  else
  {
    printSemanticError(1, line, "Undefined" + name);
    return LogErrorV("Error.");
  }
  return retVal;
  // end
}
Value *NArgs::codegen() { return exp.codegen(); }
Value *NMethodCall::codegen() {
  debug("NMethodCall");
  // begin
  Value *retVal;
  // 参数列表
  std::vector<Value *> argsV;
  // 获取函数
  Function *func = theModule->getFunction(id.name);
  // 函数是否存在检查
  if(func == nullptr)
  {
    printSemanticError(2, line, "Undefined Method " + id.name);
    return LogErrorV("Undefined Method.");
  }
  // 添加函数参数
  for (auto pArg = this->nargs; pArg != nullptr ; pArg = pArg->nArgs)
  {
    argsV.push_back(pArg->codegen());
  }
  // 函数参数个数检查
  if(argsV.size() != func->arg_size())
  {
    printSemanticError(8, line, "Wrong arg num.");
    return LogErrorV("Wrong arg num.");
  }  
  // 函数类型检查
  for(std::size_t i = 0; i<argsV.size(); i++)
  {
    if(argsV[i]->getType() != func->getArg(i)->getType())
    {
      printSemanticError(8, line, "Wrong arg type.");
      return LogErrorV("Wrong arg type.");
    }
  }
  // 函数调用
  retVal = builder->CreateCall(func, argsV, "methodCall");
  return retVal;
  // end
}
Value *NParenOperator::codegen() { return exp.codegen(); }
Value *NSingleOperator::codegen() {
  debug("NSingleOperator");
  // begin
  Value *retVal = nullptr;
  Value *tempHS = hs.codegen();
  // 检查左变量
  if(tempHS == nullptr) 
  {
    return nullptr;
  }
  if(name == "PLUSPLUS-")
  {}
  
  return nullptr;
  // end
}
Value *NBinaryOperator::codegen() {
  debug("NBinaryOperator");
  // begin
  Value *retVal = nullptr;
  Value *tempLHS = lhs.codegen(), *tempRHS = rhs.codegen();
  if(tempLHS == nullptr || tempRHS == nullptr)
  return nullptr;
  if (name == "AND")
  {
    retVal = builder->CreateAdd(tempLHS, tempRHS, "and");
  }
  else if(name == "OR")
  {
    retVal = builder->CreateOr(tempLHS, tempRHS, "or");
  }
  else if(name == "RELOP==")
  {
    retVal = builder->CreateICmpEQ(tempLHS, tempRHS, "EQ");
  }
  else if (name == "RELOP!=") 
  {
  retVal = builder->CreateICmpNE(tempLHS, tempRHS, "NE");
  }
  else if(name == "RELOP<=")
  {
    retVal = builder->CreateICmpSLE(tempLHS, tempRHS, "SLE");
  }
  else if(name == "RELOP>=")
  {
    retVal = builder->CreateICmpSGE(tempLHS, tempRHS, "SGE");
  }
  else if(name == "RELOP<")
  {
    retVal = builder->CreateICmpSLT(tempLHS, tempRHS, "SLT");
  }
  else if(name == "RELOP>")
  {
    retVal = builder->CreateICmpSGT(tempLHS, tempRHS, "SGT");
  }
  else if(name == "PLUS")
  {
    retVal = builder->CreateAdd(tempLHS, tempRHS, "add");
  }
  else if(name == "MINUS")
  {
    retVal = builder->CreateSub(tempLHS, tempRHS, "sub");
  }
  else if(name == "STAR")
  {
    retVal = builder->CreateMul(tempLHS, tempRHS, "mul");
  }
  else if(name == "DIV")
  {
    retVal = builder->CreateSDiv(tempLHS, tempRHS, "sdiv");
  }
  else if(name == "MOD")
  {
    retVal = builder->CreateSRem(tempLHS, tempLHS, "srem");
  }
  return retVal;
  // end
}
Value *NAssignment::codegen() {
  // Assignment requires the LHS to be an identifier.
  debug(std::string("NAssignment") + lhs.name);
  // begin
  Value *retVal;
  int varLevel;
  if ((varLevel = findVar(std::string(lhs.name))) != -1) 
  {
    Value *tempLHS = lhs.codegen(), *tempRHS = rhs.codegen();
    if (tempLHS == nullptr || tempRHS == nullptr)
    {
      return nullptr;
    }
      
    if (tempLHS->getType() != tempRHS->getType())
    {
      printSemanticError(5, line, "Type error in binary operator.");
      return LogErrorV("Error.");
    }
    if (this->name == "ASSIGNOP")
    {
      builder->CreateStore((retVal = tempRHS), varTable[varLevel].localVar[std::string(lhs.name)]);
    }
    else if (this->name == "PLUSASS")
    {
      retVal = builder->CreateAdd(tempLHS, tempRHS, "add");
      builder->CreateStore(retVal, varTable[varLevel].localVar[std::string(lhs.name)]);
    }
    else if (this->name == "MINUSASS")
    {
      retVal = builder->CreateSub(tempLHS, tempRHS, "sub");
      builder->CreateStore(retVal, varTable[varLevel].localVar[std::string(lhs.name)]);
    }
    else if (this->name == "STARASS")
    {
      retVal = builder->CreateMul(tempLHS, tempRHS, "mul");
      builder->CreateStore(retVal, varTable[varLevel].localVar[std::string(lhs.name)]);
    }
    else if (this->name == "DIVASS")
    {
      retVal = builder->CreateSDiv(tempLHS, tempRHS, "sdiv");
      builder->CreateStore(retVal, varTable[varLevel].localVar[std::string(lhs.name)]);
    }
  }
  else
  {
    printSemanticError(6, line, "Only rvalue " + lhs.name);
    return LogErrorV("Error.");
  }

  return retVal;
  // end
}
Value *NSpecifier::codegen() {
  // begin
  debug("NSpecifier");
  return ConstantInt::get(*theContext, APInt(32, 0, true));
  // end
}
Type *NSpecifier::getType() {
  if (type == "int")
    return Type::getInt32Ty(*theContext);
  if (type == "float")
    return Type::getFloatTy(*theContext);
  if (type == "char")
    return Type::getInt8Ty(*theContext);
  assert(false);
  return Type::getInt32Ty(*theContext);
}
Value *NVarDec::codegen(Type *varType, Value * varValue) {
  debug("NVarDec");
  // begin
  Value *retVal = nullptr;
  // 分配空间
  AllocaInst *alloca = builder->CreateAlloca(varType, nullptr, Id.name);
  // 判断是否重定义
  if(varTable[level].localVar[Id.name])
  {
    printSemanticError(3, line, "Redefined " + Id.name);
    return LogErrorV("Error.");
  }
  if(!varValue)
  {
    varValue = ConstantInt::get(*theContext, APInt(32, 0, true));
  }
  builder->CreateStore(varValue, alloca);
  retVal = varValue;
  varTable[level].localVar[std::string(Id.name)] = alloca;
  return retVal;
  // end
}
Value *NParamDec::codegen() {
  // begin
  debug("NParamDec");
  return ConstantInt::get(*theContext, APInt(32, 0, true));
  // end
}

std::pair<std::string, Type *> NParamDec::getType() {
  assert(varDec.v.size() == 0);
  std::pair<std::string, Type *> tmp(varDec.Id.name, nSpecifier.getType());
  return tmp;
}
Value *NVarList::codegen() {
  assert(false); // Never use this function.
  // This is a list.
  return ConstantInt::get(*theContext, APInt(32, 0, true));
}
Function *NFunDec::funcodegen(Type *retType) {
  debug("NFunDec");
  // check if it exists the same name of fun
  if (theModule->getFunction(Id.name)) {
    printSemanticError(4, line, "Redefined " + Id.name);
    return nullptr;
  }

  std::vector<Type *> argsTypes;
  std::vector<std::string> argNames;
  for (NVarList *item = arguments; item; item = item->nVarList) {
    funWithArg = true;
    auto tmp = item->nParamDec.getType();
    argNames.push_back(tmp.first);
    argsTypes.push_back(tmp.second);
  }
  if (funWithArg) {
    level++;
    LocalVarTable localVarTable = {0};
    localVarTable.level = level;
    varTable.push_back(localVarTable);
  }

  FunctionType *ft = FunctionType::get(retType, argsTypes, false);
  Function *f =
      Function::Create(ft, Function::ExternalLinkage, Id.name, theModule.get());
  unsigned idx = 0;
  for (auto &arg : f->args()) {
    arg.setName(argNames[idx++]);
  }
  return f;
}

/* -------------------------------------------------*/
Value* NDec::codegen(Type* varType) { 
  debug("NDec");
  // begin
  Value* retVal = nullptr;
  if (!exp) {
      retVal = vardec.codegen(varType, nullptr);
  } else {
      retVal = vardec.codegen(varType, exp->codegen());
  }
  return retVal;
  // end
}

Value* NDecList::codegen(Type* varType) {
  debug("NDecList");
  // begin
  Value* retVal = nullptr;
  retVal = dec.codegen(varType);
  if (!retVal) {
      return nullptr;
  }
  if (nDecList) {
      retVal = nDecList->codegen(varType);
  }
  return retVal;
  // end
}
/*--------------------------------------*/


Value *NDef::codegen() {
  debug("NDef");
  // begin
  Type *varType = nSpecifier.getType();
  Value *retVal = nDecList->codegen(varType);

  return retVal;
  // end
}
Value *NDefList::codegen() {
  debug("NDefList");
  // begin
  Value *retVal = nDef.codegen();
  if(!retVal) return nullptr;
  if (nDefList) 
  retVal = nDefList->codegen();
  return retVal;
  // end
}
Value *NStmtList::codegen() {
  auto *retVal = nStmt.codegen();
  if (nStmtList)
    retVal = nStmtList->codegen();
  return retVal;
}
Value *NCompSt::codegen() {
  // 自行处理变量作用域的问题
  Value *retVal = nullptr;
  if (!funWithArg) {
    level++;
    LocalVarTable localVarTable = {0};
    localVarTable.level = level;
    varTable.push_back(localVarTable);
  } else {
    funWithArg = false;
  }
  if (ndeflist)
    retVal = ndeflist->codegen();
  if (nstmtlist)
    retVal = nstmtlist->codegen();
  varTable.pop_back();
  level--;
  return retVal;
}
Value *NExpStmt::codegen() { return exp.codegen(); }
Value *NCompStStmt::codegen() {
  // begin

  return compst.codegen();
  // end
}
Value *NRetutnStmt::codegen() {
  Function *theFun = builder->GetInsertBlock()->getParent();
  BasicBlock *bb = BasicBlock::Create(*theContext, "ret", theFun);
  builder->CreateBr(bb);
  builder->SetInsertPoint(bb);
  auto *retVal = exp.codegen();
  // check the return type and fundec type
  // begin
  if(!retVal)
  return retVal;
  if(retVal->getType() != theFun->getReturnType())
  {
    printSemanticError(7, line, "Wrong return type.");
    return LogErrorV("Wrong return type.");
  }
  // end
    builder->CreateRet(retVal);
  return retVal;
}
Value *NIfStmt::codegen() {
  Function *theFun = builder->GetInsertBlock()->getParent();
  // begin
  Value *retVal = nullptr;
  Value *compI = exp.codegen();
  Value* condValI = builder->CreateICmpNE(compI, Constant::getNullValue(compI->getType()), "condValI");
  // 真和后继两个基本块
  BasicBlock* thenBlockI = BasicBlock::Create(*theContext, "thenI", theFun);
  BasicBlock* contBlockI = BasicBlock::Create(*theContext, "contI", theFun);
  // 根据比较值跳转
  builder->CreateCondBr(condValI, thenBlockI, contBlockI);
  // 设置真基本块标志
  builder->SetInsertPoint(thenBlockI);
  retVal = ConstantInt::get(*theContext, APInt(32, 1, true));
  stmt.codegen();
  // 进入后继基本块
  builder->CreateBr(contBlockI);
  // 设置后继基本块标志
  builder->SetInsertPoint(contBlockI);
  if (!retVal) { retVal = ConstantInt::get(*theContext, APInt(32, 0, true)); }
  return retVal;


  return nullptr;
  // end
}
Value *NIfElseStmt::codegen() {
  Function *theFun = builder->GetInsertBlock()->getParent();
  // begin
  Value* retVal = nullptr;
  Value* compIE = exp.codegen();
  Value* condValIE = builder->CreateICmpNE(compIE, Constant::getNullValue(compIE->getType()), "condValIE");
  // 创建真,假,后继三个基本块
  BasicBlock* thenBlockIE = BasicBlock::Create(*theContext, "thenIE", theFun);
  BasicBlock* elseBlockIE = BasicBlock::Create(*theContext, "elseIE", theFun);
  BasicBlock* contBlockIE = BasicBlock::Create(*theContext, "contIE", theFun);
  // 根据判断值跳转
  builder->CreateCondBr(condValIE, thenBlockIE, elseBlockIE);
  // 设置真基本块入口
  builder->SetInsertPoint(thenBlockIE);
  retVal = ConstantInt::get(*theContext, APInt(32, 0, true));
  stmt.codegen();
  // 进入后继基本块
  builder->CreateBr(contBlockIE);
  // 设置假基本块入口
  builder->SetInsertPoint(elseBlockIE);
  retVal = ConstantInt::get(*theContext, APInt(32, 1, true));
  stmt_else.codegen();
  // 进入后继基本块
  builder->CreateBr(contBlockIE);
  // 设置后继基本块入口
  builder->SetInsertPoint(contBlockIE);
  return retVal;
  // end
}
Value *NWhileStmt::codegen() {
  Function *theFun = builder->GetInsertBlock()->getParent();
  BasicBlock *condb = BasicBlock::Create(*theContext, "cond", theFun);
  // begin
  Value *retVal = nullptr;
  // 创建两个基本块
  BasicBlock *doBlockW = BasicBlock::Create(*theContext, "doW", theFun);
  BasicBlock *ntBlockW = BasicBlock::Create(*theContext, "ntW", theFun);
  // 跳转判断基本块
  builder->CreateBr(condb);
  // 设置判断基本块入口
  builder->SetInsertPoint(condb);
  // 进入
  Value* compW = exp.codegen();
  Value* condValW = builder->CreateICmpNE(compW, Constant::getNullValue(compW->getType()), "condValW");
  // 根据condValW值跳转 真为doBlockW 否则为ntBlockW
  builder->CreateCondBr(condValW, doBlockW, ntBlockW);
  // 创建doblock入口
  builder->SetInsertPoint(doBlockW);
  stmt.codegen();
  // 跳转到判断基本块
  builder->CreateBr(condb);
  // 设置否则为ntBlockW入口
  builder->SetInsertPoint(ntBlockW);
  return ConstantInt::get(*theContext, APInt(32, 0, true)); 
  // end
}
Value *NBreakStmt::codegen() {
  // begin
  return ConstantInt::get(*theContext, APInt(32, 0, true));
  // end
}
Value *NExtDefVarDec::codegen() {
  // begin

  return ConstantInt::get(*theContext, APInt(32, 0, true));
  // end
}
Value *NExtDefFunDec::codegen() {
  Type *retType = specifier.getType();

  Function *f = fundec->funcodegen(retType);
  if (!f) {
    return nullptr;
  }
  assert(compst != nullptr); // Assert compst is not null.
  BasicBlock *bb = BasicBlock::Create(*theContext, "entry", f);
  builder->SetInsertPoint(bb);
  namedValues.clear();
  for (auto &arg : f->args()) {
    // Create an alloca for this variable.
    AllocaInst *alloca =
        CreateEntryBlockAlloca(f, arg.getName(), arg.getType());

    if (curNamedValues[std::string(arg.getName())]) {
      printSemanticError(3, line, "Redefined " + arg.getName().str());
      return LogErrorV("Unknown function referenced");
    }
    // Store the initial value into the alloca.
    builder->CreateStore(&arg, alloca);
    // Add arguments to variable symbol table.
    varTable[level].localVar[std::string(arg.getName())] = alloca;
    // namedValues[std::string(arg.getName())] = alloca;
    // curNamedValues[std::string(arg.getName())] = alloca;
  }
  if (Value *retVal = compst->codegen()) {
    // Finish off the function.

    // Validate the generated code, checking for consistency.
    verifyFunction(*f);

    // Run the optimizer on the function.
    theFPM->run(*f);
    return f;
  }
  // Error reading body, remove function.
  f->eraseFromParent();

  return nullptr;
}
Value *NExtDefList::codegen() {
  auto *lastCode = nExtDef.codegen();
  // lastCode->print(errs());
  // assert(nExtDefList == nullptr);
  if (nExtDefList)
    lastCode = nExtDefList->codegen();
  return lastCode;
}
Value *NProgram::codegen() {

  //默认输出函数putchar
  std::vector<Type *> putArgs;
  putArgs.push_back(Type::getInt32Ty(*theContext));

  FunctionType *putType =
      FunctionType::get(builder->getInt32Ty(), putArgs, false);
  Function *putFunc = Function::Create(putType, Function::ExternalLinkage,
                                       "putchar", theModule.get());

  //默认输入函数getchar
  std::vector<Type *> getArgs;
  // getArgs.push_back(Type::getInt32Ty(*theContext));

  FunctionType *getType =
      FunctionType::get(builder->getInt32Ty(), getArgs, false);
  Function *getFunc = Function::Create(getType, Function::ExternalLinkage,
                                       "getchar", theModule.get());

  Value *lastCode = nextdeflist->codegen();
  if (grammererror)
    return nullptr;
  return lastCode;
}

