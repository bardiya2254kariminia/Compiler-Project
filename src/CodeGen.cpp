#include "CodeGen.h"
#include "llvm/ADT/StringMap.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/IR/Constants.h"

using namespace llvm;

// Define a visitor class for generating LLVM IR from the AST.
namespace
ns{
  class ToIRVisitor : public ASTVisitor
  {
    Module *M;
    IRBuilder<> Builder;
    Type *Int1Ty;
    Type *Int32Ty;
    Type *Int8PtrTy;
    Type *Int8PtrPtrTy;
    Constant *Int32Zero;
    Constant *Int32One;
    Constant *Int1False;
    Constant *Int1True;

    Value *V;
    StringMap<AllocaInst *> nameMapInt;
    StringMap<AllocaInst *> nameMapBool;

    FunctionType *CompilerWriteFnTy;
    Function *CompilerWriteFn;

  public:
    // Constructor for the visitor class.
    ToIRVisitor(Module *M) : M(M), Builder(M->getContext())
    {
      // Initialize LLVM types and constants.
      Int1Ty = Type::getInt1Ty(M->getContext());
      Int32Ty = Type::getInt32Ty(M->getContext());
      Int8PtrTy = Type::getInt8PtrTy(M->getContext());
      Int8PtrPtrTy = Int8PtrTy->getPointerTo();

      Int1False = ConstantInt::getFalse(Int1Ty);
      Int1True = ConstantInt::getTrue(Int1Ty);
      Int32Zero = ConstantInt::get(Int32Ty, 0, true);
      Int32One = ConstantInt::get(Int32Ty, 1, true);

      // Create a function type for the "compiler_write" function.
      CompilerWriteFnTy = FunctionType::get(VoidTy, {Int32Ty}, false);
      // Create a function declaration for the "compiler_write" function.
      CompilerWriteFn = Function::Create(CompilerWriteFnTy, GlobalValue::ExternalLinkage, "compiler_write", M);
    }

    // Entry point for generating LLVM IR from the AST.
    void run(Program *Tree)
    {
      // Create the main function with the appropriate function type.
      FunctionType *MainFty = FunctionType::get(Int32Ty, {Int32Ty, Int8PtrPtrTy}, false);
      Function *MainFn = Function::Create(MainFty, GlobalValue::ExternalLinkage, "main", M);

      // Create a basic block for the entry point of the main function.
      BasicBlock *BB = BasicBlock::Create(M->getContext(), "entry", MainFn);
      Builder.SetInsertPoint(BB);

      // Visit the root node of the AST to generate IR.
      Tree->accept(*this);

      // Create a return instruction at the end of the main function.
      Builder.CreateRet(Int32Zero);
    }

    // Visit function for the Program node in the AST.
    virtual void visit(Program &Node) override
    {
      // Iterate over the children of the Program node and visit each child.
      for (llvm::SmallVector<AST *>::const_iterator I = Node.begin(), E = Node.end(); I != E; ++I)
    {
      (*I)->accept(*this); // Visit each child node
    }
    };

    virtual void visit(DeclarationInt &Node) override
    {
      llvm::SmallVector<Value *, 8> vals;

      llvm::SmallVector<Expr *, 8>::const_iterator E = Node.valBegin();
      for (llvm::SmallVector<llvm::StringRef, 8>::const_iterator Var = Node.varBegin(), End = Node.varEnd(); Var != End; ++Var){
        if (E<Node.valEnd())
        {
          (*E)->accept(*this); // If the Declaration node has an expression, recursively visit the expression node
          vals.push_back(V);
        }
        else 
        {
          vals.push_back(nullptr);
        }
        E++;
      }
      StringRef Var;
      Value* val;
      llvm::SmallVector<Value *, 8>::const_iterator itVal = vals.begin();
      for (llvm::SmallVector<llvm::StringRef, 8>::const_iterator S = Node.varBegin(), End = Node.varEnd(); S != End; ++S){
        
        Var = *S;

        // Create an alloca instruction to allocate memory for the variable.
        nameMapInt[Var] = Builder.CreateAlloca(Int32Ty);
        
        // Store the initial value (if any) in the variable's memory location.
        if (*itVal != nullptr)
        {
          Builder.CreateStore(*itVal, nameMapInt[Var]);
        }
        else
        {
          Builder.CreateStore(Int32Zero, nameMapInt[Var]);
        }
        itVal++;
      }
    };

    virtual void visit(DeclarationBool &Node) override
    {
      llvm::SmallVector<Value *, 8> vals;

      llvm::SmallVector<Logic *, 8>::const_iterator L = Node.valBegin();
      for (llvm::SmallVector<llvm::StringRef, 8>::const_iterator Var = Node.varBegin(), End = Node.varEnd(); Var != End; ++Var){
        if (L<Node.valEnd())
        {
          (*L)->accept(*this); // If the Declaration node has an expression, recursively visit the expression node
          vals.push_back(V);
        }
        else 
        {
          vals.push_back(nullptr);
        }
        L++;
      }
      StringRef Var;
      Value* val;
      llvm::SmallVector<Value *, 8>::const_iterator itVal = vals.begin();
      for (llvm::SmallVector<llvm::StringRef, 8>::const_iterator S = Node.varBegin(), End = Node.varEnd(); S != End; ++S){
        
        Var = *S;

        // Create an alloca instruction to allocate memory for the variable.
        nameMapBool[Var] = Builder.CreateAlloca(Int1Ty);
        
        // Store the initial value (if any) in the variable's memory location.
        if (*itVal != nullptr)
        {
          Builder.CreateStore(*itVal, nameMapBool[Var]);
        }
        else
        {
          Builder.CreateStore(Int1False, nameMapBool[Var]);
        }
        itVal++;
      }
    };
    // TODO
    virtual void visit(Assignment &Node) override
    {
      // Get the name of the variable being assigned.
      llvm::StringRef varName = Node.getLeft()->getVal();
      Node.getLeft()->accept(*this);
      Value *varVal = V;

      bool isBool = Node.getRightExpr() == nullptr;
      
      if (isBool)
        Node.getRightLogic()->accept(*this)
      else
        Node.getRightExpr()->accept(*this);

      Value *val = V;

      switch (Node.getAssignKind())
      {
      case Assignment::Plus_assign:
        val = Builder.CreateNSWAdd(varVal, val);
        break;
      case Assignment::Minus_assign:
        val = Builder.CreateNSWSub(varVal, val);
        break;
      case Assignment::Star_assign:
        val = Builder.CreateNSWMul(varVal, val);
        break;
      case Assignment::Slash_assign:
        val = Builder.CreateSDiv(varVal, val);
        break;
      default:
        break;
      }

      // Create a store instruction to assign the value to the variable.
      Builder.CreateStore(val, nameMapBool[varName]);

      // Create a call instruction to invoke the "compiler_write" function with the value.
      CallInst *Call = Builder.CreateCall(CompilerWriteFnTy, CompilerWriteFn, {val});
    };

    virtual void visit(Final &Node) override
    {
      if (Node.getKind() == Final::Ident)
      {
        // If the Final is an identifier, load its value from memory.
        V = Builder.CreateLoad(Int32Ty, nameMapInt[Node.getVal()]);
      }
      else
      {
        // If the Final is a literal, convert it to an integer and create a constant.
        int intval;
        Node.getVal().getAsInteger(10, intval);
        V = ConstantInt::get(Int32Ty, intval, true);
      }
    };

    virtual void visit(BinaryOp &Node) override
    {
      // Visit the left-hand side of the binary operation and get its value.
      Node.getLeft()->accept(*this);
      Value *Left = V;

      // Visit the right-hand side of the binary operation and get its value.
      Node.getRight()->accept(*this);
      Value *Right = V;

      // Perform the binary operation based on the operator type and create the corresponding instruction.
      switch (Node.getOperator())
      {
      case BinaryOp::Plus:
        V = Builder.CreateNSWAdd(Left, Right);
        break;
      case BinaryOp::Minus:
        V = Builder.CreateNSWSub(Left, Right);
        break;
      case BinaryOp::Mul:
        V = Builder.CreateNSWMul(Left, Right);
        break;
      case BinaryOp::Div:
        V = Builder.CreateSDiv(Left, Right);
        break;
      case BinaryOp::Mod:
        V = Builder.CreateSRem(Left, Right);
        break;
      case BinaryOp::Exp:
        V = CreateExp(Left, Right);
        break;
      default:
        break;
      }
    };

    virtual void visit(UnaryOp &Node) override
    {
      // Visit the left-hand side of the binary operation and get its value.
      Value *Left = Builder.CreateLoad(Int32Ty, nameMapInt[Node.getIdent()]);;

      // Perform the binary operation based on the operator type and create the corresponding instruction.
      switch (Node.getOperator())
      {
      case UnaryOp::Plus_plus:
        V = Builder.CreateNSWAdd(Left, Int32One);
        break;
      case UnaryOp::Minus_minus:
        V = Builder.CreateNSWSub(Left, Int32One);
      default:
        break;
      }
      
      Builder.CreateStore(V, nameMapInt[Node.getIdent()]);
    };

    Value* CreateExp(Value *Left, Value *Right)
    { 
      Value* res = Int32One;

      int intValue;
      
      if (ConstantInt* intConstant = dyn_cast<ConstantInt>(Right)) {
        // Get the integer value
        intValue = intConstant->getSExtValue(); // or getZExtValue() for unsigned values
        // Now, 'intValue' contains the actual integer value.
      } else {
        // Handle the case where the Value is not an integer constant
        llvm::errs() << "Error: The exponent only allowed to be a constant.\n";
        exit(3);
      }

      for (int i = 0; i < intValue; ++i)
      {
        res = Builder.CreateNSWMul(res, Left);
      }
      return res;
    }

    virtual void visit(NegExpr &Node) override
    {
      Node.getExpr()->accept(*this);
      V = builder.CreateNeg(V);
    };

    virtual void visit(LogicalExpr &Node) override{
      // Visit the left-hand side of the Logical operation and get its value.
      Node.getLeft()->accept(*this);
      Value *Left = V;

      // Visit the right-hand side of the Logical operation and get its value.
      Node.getRight()->accept(*this);
      Value *Right = V;

      switch (Node.getOperator())
      {
      case LogicalExpr::And:
        V = Builder.CreateAnd(Left, Right);
        break;
      case LogicalExpr::Or:
        V = Builder.CreateOr(Left, Right);
        break;
      default:
        break;
      }
    };

    virtual void visit(Comparison &Node) override{
      // Visit the left-hand side of the Comparison operation and get its value.
      Node.getLeft()->accept(*this);
      Value *Left = V;

      // Visit the right-hand side of the Comparison operation and get its value.
      Node.getRight()->accept(*this);
      Value *Right = V;

      switch (Node.getOperator())
      {
      case Comparison::Equal:
        V = Builder.CreateICmpEQ(Left, Right);
        break;
      case Comparison::Not_equal:
        V = Builder.CreateICmpNE(Left, Right);
        break;
      case Comparison::Less:
        V = Builder.CreateICmpSLT(Left, Right);
        break;
      case Comparison::Greater:
        V = Builder.CreateICmpSGT(Left, Right);
        break;
      case Comparison::Less_equal:
        V = Builder.CreateICmpSLE(Left, Right);
        break;
      case Comparison::Greater_equal:
        V = Builder.CreateICmpSGE(Left, Right);
        break;
      case Comparison::True:
        V = Int1True;
        break;
      case Comparison::False:
        V = Int1False;
        break;
      case Comparison::Ident:
        V = Builder.CreateLoad(Int1Ty, nameMapBool[Node.getLeft()]);
        break;
      default:
        break;
      }
    };

    bool isBool(llvm::StringRef Var)
    {
      return nameMapBool.find(Var) != nameMapBool.end();
    }

    virtual void visit(PrintStmt &Node) override
    {
      // Visit the right-hand side of the assignment and get its value.
      if (isBool(Node.getVal()))
        V = Builder.CreateLoad(Int1Ty, nameMapBool[Node.getVal()]);
      else
        V = Builder.CreateLoad(Int32Ty, nameMapInt[Node.getVal()]);

      // Create a call instruction to invoke the "print" function with the value.
      CallInst *Call = Builder.CreateCall(CompilerWriteFnTy, CompilerWriteFn, {V});
    };

    virtual void visit(IterStmt &Node) override
    {
      llvm::BasicBlock* WhileCondBB = llvm::BasicBlock::Create(M->getContext(), "loopc.cond", Builder.GetInsertBlock()->getParent());
      llvm::BasicBlock* WhileBodyBB = llvm::BasicBlock::Create(M->getContext(), "loopc.body", Builder.GetInsertBlock()->getParent());
      llvm::BasicBlock* AfterWhileBB = llvm::BasicBlock::Create(M->getContext(), "after.loopc", Builder.GetInsertBlock()->getParent());

      Builder.CreateBr(WhileCondBB);
      Builder.SetInsertPoint(WhileCondBB);
      Node.getCond()->accept(*this);
      Value* val=V;
      Builder.CreateCondBr(val, WhileBodyBB, AfterWhileBB);
      Builder.SetInsertPoint(WhileBodyBB);

      for (llvm::SmallVector<Assignment* >::const_iterator I = Node.begin(), E = Node.end(); I != E; ++I)
        {
            (*I)->accept(*this);
        }

      Builder.CreateBr(WhileCondBB);

      Builder.SetInsertPoint(AfterWhileBB);
        
    };

    virtual void visit(IfStmt &Node) override{
      llvm::BasicBlock* IfCondBB = llvm::BasicBlock::Create(M->getContext(), "if.cond", Builder.GetInsertBlock()->getParent());
      llvm::BasicBlock* IfBodyBB = llvm::BasicBlock::Create(M->getContext(), "if.body", Builder.GetInsertBlock()->getParent());
      llvm::BasicBlock* AfterIfBB = llvm::BasicBlock::Create(M->getContext(), "after.if", Builder.GetInsertBlock()->getParent());

      Builder.CreateBr(IfCondBB);
      Builder.SetInsertPoint(IfCondBB);
      Node.getCond()->accept(*this);
      Value* IfCondVal=V;

      Builder.SetInsertPoint(IfBodyBB);

      for (llvm::SmallVector<Assignment* >::const_iterator I = Node.begin(), E = Node.end(); I != E; ++I)
        {
            (*I)->accept(*this);
        }

      Builder.CreateBr(AfterIfBB);

      llvm::BasicBlock* PreviousCondBB = IfCondBB;
      llvm::BasicBlock* PreviousBodyBB = IfBodyBB;
      Value* PreviousCondVal = IfCondVal;

      for (llvm::SmallVector<elifStmt *, 8>::const_iterator I = Node.beginElif(), E = Node.endElif(); I != E; ++I)
      {
        llvm::BasicBlock* ElifCondBB = llvm::BasicBlock::Create(M->getContext(), "elif.cond", Builder.GetInsertBlock()->getParent());
        llvm::BasicBlock* ElifBodyBB = llvm::BasicBlock::Create(M->getContext(), "elif.body", Builder.GetInsertBlock()->getParent());

        Builder.SetInsertPoint(PreviousCondBB);
        Builder.CreateCondBr(PreviousCondVal, PreviousBodyBB, ElifCondBB);

        Builder.SetInsertPoint(ElifCondBB);
        (*I)->getCond()->accept(*this);
        Value* ElifCondVal = V;

        Builder.SetInsertPoint(ElifBodyBB);
        (*I)->accept(*this);
        Builder.CreateBr(AfterIfBB);

        PreviousCondBB = ElifCondBB;
        PreviousCondVal = ElifCondVal;
        PreviousBodyBB = ElifBodyBB;
      }
      if (Node.beginElse() != Node.endElse()) {
        llvm::BasicBlock* ElseBB = llvm::BasicBlock::Create(M->getContext(), "else.body", Builder.GetInsertBlock()->getParent());
        Builder.SetInsertPoint(ElseBB);
        for (llvm::SmallVector<Assignment* >::const_iterator I = Node.beginElse(), E = Node.endElse(); I != E; ++I)
        {
            (*I)->accept(*this);
        }
        Builder.CreateBr(AfterIfBB);

        Builder.SetInsertPoint(PreviousCondBB);
        Builder.CreateCondBr(PreviousCondVal, PreviousBodyBB, ElseBB);
      }
      else {
        Builder.SetInsertPoint(PreviousCondBB);
        Builder.CreateCondBr(IfCondVal, PreviousBodyBB, AfterIfBB);
      }

      Builder.SetInsertPoint(AfterIfBB);
    };

    virtual void visit(elifStmt &Node) override{
      for (llvm::SmallVector<Assignment* >::const_iterator I = Node.begin(), E = Node.end(); I != E; ++I)
        {
            (*I)->accept(*this);
        }
    };
  };
}; // namespace

void CodeGen::compile(Program *Tree)
{
  // Create an LLVM context and a module.
  LLVMContext Ctx;
  Module *M = new Module("simple-compiler", Ctx);

  // Create an instance of the ToIRVisitor and run it on the AST to generate LLVM IR.
  ns::ToIRVisitor *ToIR = new ns::ToIRVisitor(M);


  ToIR->run(Tree);

  // Print the generated module to the standard output.
  M->print(outs(), nullptr);
}
