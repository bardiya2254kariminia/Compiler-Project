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
    Type *VoidTy;
    Type *Int1Ty;
    Type *Int32Ty; //for int
    Type *Int8Ty;  // for char
    Type *Int8PtrTy;
    Type *Int8PtrPtrTy;
    Constant *Int32Zero;
    Constant *Int32One;
    Constant *Int1False;
    Constant *Int1True;
    Type *FloatTy; //for float
    Constant *FloatZero; //defalut  zero for float
    Constant *Int8Zero;  // Default zero value for chars
    // Add string type constants
    Constant *EmptyString;
    


    Value *V;
    StringMap<AllocaInst *> nameMapString;
    StringMap<AllocaInst *> nameMapInt;
    StringMap<AllocaInst *> nameMapBool;
    StringMap<AllocaInst *> nameMapFloat;
    StringMap<AllocaInst *> nameMapChar;
    StringMap<std::pair<Value*, size_t>> nameMapArray; // New map for array variables

    // int print
    FunctionType *PrintIntFnTy;
    Function *PrintIntFn;
    // bool print
    FunctionType *PrintBoolFnTy;
    Function *PrintBoolFn;
    // float print
    FunctionType *PrintFloatFnTy; 
    Function *PrintFloatFn;      
    // chhar print
    FunctionType *PrintCharFnTy;
    Function *PrintCharFn;
    // string print
    FunctionType *PrintStringFnTy;
    Function *PrintStringFn; 
    // Array-related types
    Type *ArrayTy;
    Type *ArrayPtrTy;

  public:
    // Constructor for the visitor class.
    ToIRVisitor(Module *M) : M(M), Builder(M->getContext())
    {
      // Initialize LLVM types and constants.
      VoidTy = Type::getVoidTy(M->getContext());
      Int1Ty = Type::getInt1Ty(M->getContext());
      Int32Ty = Type::getInt32Ty(M->getContext());
      Int8PtrTy = Type::getInt8PtrTy(M->getContext());
      Int8PtrPtrTy = Int8PtrTy->getPointerTo();

      Int1False = ConstantInt::getFalse(Int1Ty);
      Int1True = ConstantInt::getTrue(Int1Ty);
      Int32Zero = ConstantInt::get(Int32Ty, 0, true);
      Int32One = ConstantInt::get(Int32Ty, 1, true);
      
      FloatTy = Type::getFloatTy(M->getContext());
      FloatZero = ConstantFP::get(FloatTy, 0.0);

      Int8Ty = Type::getInt8Ty(M->getContext());
      Int8Zero = ConstantInt::get(Int8Ty, 0, true);

      EmptyString = ConstantPointerNull::get(Type::getInt8PtrTy(M->getContext())); // for getting the string's default

      // Initialize array types
      ArrayTy = ArrayType::get(Int32Ty, 0); // Initially zero-length, will resize per array
      ArrayPtrTy = ArrayTy->getPointerTo();

      // Create a function declaration for the "compiler_write" function.
      PrintIntFnTy = FunctionType::get(VoidTy, {Int32Ty}, false);
      PrintIntFn = Function::Create(PrintIntFnTy, GlobalValue::ExternalLinkage, "print_int", M);

      // Create a function declaration for the "compiler_write" function.
      PrintBoolFnTy = FunctionType::get(VoidTy, {Int1Ty}, false);
      PrintBoolFn = Function::Create(PrintBoolFnTy, GlobalValue::ExternalLinkage, "print_bool", M);
      // Create a function declaration for the "compiler_write" function.
      PrintFloatFnTy = FunctionType::get(VoidTy, {FloatTy}, false);
      PrintFloatFn = Function::Create(PrintFloatFnTy, GlobalValue::ExternalLinkage, "print_float", M);
      // Create a function declaration for the "compiler_write" function.
      PrintCharFnTy = FunctionType::get(VoidTy, {Int8Ty}, false);
      PrintCharFn = Function::Create(PrintCharFnTy, GlobalValue::ExternalLinkage, "print_char", M);
      // Create a function declaration for the "compiler_write" function.
      PrintStringFnTy = FunctionType::get(VoidTy, {Int8PtrTy}, false);
      PrintStringFn = Function::Create(PrintStringFnTy, GlobalValue::ExternalLinkage, "print_string", M);
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

    // Visit function for the Program node in the AST. and  declarations
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
        if (E<Node.valEnd() && *E != nullptr)
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

    virtual void visit(DeclarationFloat &Node) override{
      llvm::SmallVector<Value *, 8> vals;

      // Iterate over the initial value expressions
      llvm::SmallVector<Expr *, 8>::const_iterator E = Node.valBegin();
      for (llvm::SmallVector<llvm::StringRef, 8>::const_iterator Var = Node.varBegin(), End = Node.varEnd(); Var != End; ++Var){
        if (E < Node.valEnd() && *E != nullptr)
        {
          (*E)->accept(*this); // Visit expression, sets V
          vals.push_back(V);
        }
        else
        {
          vals.push_back(nullptr); // No initializer
        }
        E++;
      }

      // Now allocate and initialize each float variable
      llvm::SmallVector<Value *, 8>::const_iterator itVal = vals.begin();
      for (llvm::SmallVector<llvm::StringRef, 8>::const_iterator S = Node.varBegin(), End = Node.varEnd(); S != End; ++S){
        StringRef Var = *S;

        // Allocate space for a float variable
        nameMapFloat[Var] = Builder.CreateAlloca(FloatTy);

        if (*itVal != nullptr)
        {
          Builder.CreateStore(*itVal, nameMapFloat[Var]);
        }
        else
        {
          Builder.CreateStore(FloatZero, nameMapFloat[Var]);
        }
        itVal++;
      }
    }

    virtual void visit(DeclarationBool &Node) override
    {
      llvm::SmallVector<Value *, 8> vals;

      llvm::SmallVector<Logic *, 8>::const_iterator L = Node.valBegin();
      for (llvm::SmallVector<llvm::StringRef, 8>::const_iterator Var = Node.varBegin(), End = Node.varEnd(); Var != End; ++Var){
        if (L<Node.valEnd() && *L != nullptr)
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
    
    virtual void visit(DeclarationChar &Node) override {
        llvm::SmallVector<Value *, 8> vals;

        llvm::SmallVector<Expr *, 8>::const_iterator E = Node.valBegin();
        for (llvm::SmallVector<llvm::StringRef, 8>::const_iterator Var = Node.varBegin(), End = Node.varEnd(); Var != End; ++Var) {
            if (E < Node.valEnd() && *E != nullptr) {
                (*E)->accept(*this);
                vals.push_back(V);
            } else {
                vals.push_back(nullptr);
            }
            E++;
        }

        llvm::SmallVector<Value *, 8>::const_iterator itVal = vals.begin();
        for (llvm::SmallVector<llvm::StringRef, 8>::const_iterator S = Node.varBegin(), End = Node.varEnd(); S != End; ++S) {
            StringRef Var = *S;
            nameMapChar[Var] = Builder.CreateAlloca(Int8Ty);
            
            if (*itVal != nullptr) {
                Builder.CreateStore(*itVal, nameMapChar[Var]);
            } else {
                Builder.CreateStore(Int8Zero, nameMapChar[Var]);
            }
            itVal++;
        }
    }
    
    virtual void visit(DeclarationString &Node) override {
      llvm::SmallVector<Value *, 8> vals;
      // Process initial values
      auto E = Node.valBegin();
      for (auto Var = Node.varBegin(), End = Node.varEnd(); Var != End; ++Var) {
          if (E < Node.valEnd() && *E != nullptr) {
              (*E)->accept(*this);
              vals.push_back(V);
          } else {
              vals.push_back(EmptyString);
          }
          E++;
      }

      // Allocate and store strings
      auto itVal = vals.begin();
      for (auto S = Node.varBegin(), End = Node.varEnd(); S != End; ++S) {
          StringRef Var = *S;
          
          // Allocate space for a string pointer
          AllocaInst *Alloca = Builder.CreateAlloca(Type::getInt8PtrTy(M->getContext()));
          nameMapString[Var] = Alloca;
          
          Builder.CreateStore(*itVal, Alloca);
          itVal++;
      }
    }

    virtual void visit(DeclarationArray &Node) override {
        // First determine the array size from the number of elements
        size_t arraySize = std::distance(Node.valBegin(), Node.valEnd());
        
        // Create array type with the correct size
        ArrayType *arrayType = ArrayType::get(Int32Ty, arraySize);
        
        // Process each variable in the declaration
        for (auto Var = Node.varBegin(), End = Node.varEnd(); Var != End; ++Var) {
            // Allocate memory for the array
            AllocaInst *arrayAlloca = Builder.CreateAlloca(arrayType);
            nameMapArray[*Var] = {arrayAlloca, arraySize}; 
            
            // Initialize each element
            unsigned index = 0;
            for (auto E = Node.valBegin(); E != Node.valEnd(); ++E, ++index) {
                if (*E != nullptr) {
                    (*E)->accept(*this); // Generate code for the element expression
                    
                    // Get pointer to array element
                    Value *indexList[] = {
                        ConstantInt::get(Int32Ty, 0),        // First dimension (always 0)
                        ConstantInt::get(Int32Ty, index)     // Element index
                    };
                    Value *elementPtr = Builder.CreateInBoundsGEP(
                        arrayType, arrayAlloca, indexList);
                    
                    // Store the value
                    Builder.CreateStore(V, elementPtr);
                }
            }
        }
    }

    virtual void visit(LengthFunction &Node) override {
        auto arrayEntry = nameMapArray[Node.getArrayName()];
        if (!arrayEntry.first) {
            llvm::report_fatal_error("Unknown array: " + Node.getArrayName());
        }
        V = ConstantInt::get(Int32Ty, arrayEntry.second);
    }

    virtual void visit(ArrayAccess &Node) override {
        // Get the array pointer from the pair
        auto arrayEntry = nameMapArray[Node.getArrayName()];
        if (!arrayEntry.first) {
            llvm::report_fatal_error("Undefined array: " + Node.getArrayName());
        }

        // Evaluate the index expression
        Node.getIndex()->accept(*this);
        Value *Idx = V;

        // Convert index to i32 if needed
        if (Idx->getType() != Int32Ty) {
            Idx = Builder.CreateIntCast(Idx, Int32Ty, true);
        }

        // Calculate element pointer
        Value *IndexList[] = {
            ConstantInt::get(Int32Ty, 0),  // First dimension (always 0)
            Idx                             // Element index
        };
        
        // Get pointer to the array element
        Value *ElementPtr = Builder.CreateInBoundsGEP(
            arrayEntry.first->getType()->getPointerElementType(), 
            arrayEntry.first, 
            IndexList
        );

        // For now, just return the element pointer
        // The actual load will be done by the parent node (Assignment or expression)
        V = ElementPtr;
    };

    virtual void visit(MinFunction &Node) override {
        auto arrayEntry = nameMapArray[Node.getArrayName()];
        Value *ArrayPtr = arrayEntry.first;
        size_t ArraySize = arrayEntry.second;

        // Create loop to find minimum
        BasicBlock *PreBB = Builder.GetInsertBlock();
        BasicBlock *CondBB = BasicBlock::Create(M->getContext(), "min.cond", PreBB->getParent());
        BasicBlock *BodyBB = BasicBlock::Create(M->getContext(), "min.body", PreBB->getParent());
        BasicBlock *ExitBB = BasicBlock::Create(M->getContext(), "min.exit", PreBB->getParent());

        // Initialize variables
        AllocaInst *MinVal = Builder.CreateAlloca(Int32Ty);
        AllocaInst *Index = Builder.CreateAlloca(Int32Ty);
        Builder.CreateStore(ConstantInt::get(Int32Ty, INT_MAX), MinVal);
        Builder.CreateStore(ConstantInt::get(Int32Ty, 0), Index);

        Builder.CreateBr(CondBB);

        // Condition block
        Builder.SetInsertPoint(CondBB);
        Value *Idx = Builder.CreateLoad(Index);
        Value *LoopCond = Builder.CreateICmpSLT(Idx, ConstantInt::get(Int32Ty, ArraySize));
        Builder.CreateCondBr(LoopCond, BodyBB, ExitBB);

        // Body block
        Builder.SetInsertPoint(BodyBB);
        // Load current element
        Value *GEP = Builder.CreateInBoundsGEP(
            ArrayPtr->getType()->getPointerElementType(),
            ArrayPtr,
            {ConstantInt::get(Int32Ty, 0), Idx}
        );
        Value *Current = Builder.CreateLoad(GEP);
        
        // Compare with min
        Value *CurMin = Builder.CreateLoad(MinVal);
        Value *IsSmaller = Builder.CreateICmpSLT(Current, CurMin);
        Value *NewMin = Builder.CreateSelect(IsSmaller, Current, CurMin);
        Builder.CreateStore(NewMin, MinVal);
        
        // Increment index
        Value *NextIdx = Builder.CreateAdd(Idx, ConstantInt::get(Int32Ty, 1));
        Builder.CreateStore(NextIdx, Index);
        Builder.CreateBr(CondBB);

        // Exit block
        Builder.SetInsertPoint(ExitBB);
        V = Builder.CreateLoad(MinVal);
    }

    virtual void visit(MaxFunction &Node) override {
        auto arrayEntry = nameMapArray[Node.getArrayName()];
        Value *ArrayPtr = arrayEntry.first;
        size_t ArraySize = arrayEntry.second;

        // Create loop to find maximum
        BasicBlock *PreBB = Builder.GetInsertBlock();
        BasicBlock *CondBB = BasicBlock::Create(M->getContext(), "max.cond", PreBB->getParent());
        BasicBlock *BodyBB = BasicBlock::Create(M->getContext(), "max.body", PreBB->getParent());
        BasicBlock *ExitBB = BasicBlock::Create(M->getContext(), "max.exit", PreBB->getParent());

        // Initialize variables
        AllocaInst *MaxVal = Builder.CreateAlloca(Int32Ty);
        AllocaInst *Index = Builder.CreateAlloca(Int32Ty);
        Builder.CreateStore(ConstantInt::get(Int32Ty, INT_MIN), MaxVal); // Initialize to minimum integer
        Builder.CreateStore(ConstantInt::get(Int32Ty, 0), Index);

        Builder.CreateBr(CondBB);

        // Condition block
        Builder.SetInsertPoint(CondBB);
        Value *Idx = Builder.CreateLoad(Index);
        Value *LoopCond = Builder.CreateICmpSLT(Idx, ConstantInt::get(Int32Ty, ArraySize));
        Builder.CreateCondBr(LoopCond, BodyBB, ExitBB);

        // Body block
        Builder.SetInsertPoint(BodyBB);
        // Load current element
        Value *GEP = Builder.CreateInBoundsGEP(
            ArrayPtr->getType()->getPointerElementType(),
            ArrayPtr,
            {ConstantInt::get(Int32Ty, 0), Idx}
        );
        Value *Current = Builder.CreateLoad(GEP);
        
        // Compare with max
        Value *CurMax = Builder.CreateLoad(MaxVal);
        Value *IsLarger = Builder.CreateICmpSGT(Current, CurMax); // Changed to SGT
        Value *NewMax = Builder.CreateSelect(IsLarger, Current, CurMax);
        Builder.CreateStore(NewMax, MaxVal);
        
        // Increment index
        Value *NextIdx = Builder.CreateAdd(Idx, ConstantInt::get(Int32Ty, 1));
        Builder.CreateStore(NextIdx, Index);
        Builder.CreateBr(CondBB);

        // Exit block
        Builder.SetInsertPoint(ExitBB);
        V = Builder.CreateLoad(MaxVal);
    }

    virtual void visit(AbsFunction &Node) override {
        // Visit the value expression first
        Node.getValue()->accept(*this);
        Value* value = V;

        // Check if the value is a float or integer
        if (value->getType()->isFloatTy()) {
            // For float values, use fabs
            Function* fabsFn = Intrinsic::getDeclaration(M, Intrinsic::fabs, {FloatTy});
            V = Builder.CreateCall(fabsFn, {value});
        } else if (value->getType()->isIntegerTy()) {
            // For integer values, create a comparison and select
            Value* zero = ConstantInt::get(value->getType(), 0);
            Value* isNegative = Builder.CreateICmpSLT(value, zero);
            Value* negated = Builder.CreateNeg(value);
            V = Builder.CreateSelect(isNegative, negated, value);
        } else {
            // Error: unsupported type
            llvm::errs() << "Error: abs() only supports integer and float types\n";
            V = nullptr;
        }
    }

    virtual void visit(Assignment &Node) override
    {
      // Get the name of the variable being assigned.
      llvm::StringRef varName;
      if (Node.getLeft()) {
          varName = Node.getLeft()->getVal();
      }

      // Check if this is an array access by looking at the variable name
      if (nameMapArray.count(varName)) {
          // This is an array access assignment
          auto arrayEntry = nameMapArray[varName];
          Value *arrayPtr = arrayEntry.first;
          
          // Get the index expression
          if (Node.getRightExpr()) {
              Node.getRightExpr()->accept(*this);
              Value *index = V;
              
              // Get the array type from the pointer
              Type *arrayType = arrayPtr->getType()->getPointerElementType();
              
              // Get element pointer using GEP
              Value *indices[] = {
                  ConstantInt::get(Int32Ty, 0), // First dimension (always 0)
                  index                         // Element index
              };
              
              // Get pointer to the element
              Value *elementPtr = Builder.CreateInBoundsGEP(
                  arrayType,     // Array type
                  arrayPtr,      // Array pointer
                  indices,       // Index list
                  "arrayidx"     // Name for the instruction
              );
              
              // Store the value
              Builder.CreateStore(V, elementPtr);
          }
          return;
      }

      // Regular variable assignment
      if (Node.getLeft()) {
          Node.getLeft()->accept(*this);
          Value *varVal = V;

          if (Node.getRightExpr() == nullptr)
              Node.getRightLogic()->accept(*this);        
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
          if (isBool(varName))
              Builder.CreateStore(val, nameMapBool[varName]);
          else if (nameMapString.count(varName)) {
              if (Node.getAssignKind() != Assignment::Assign) {
                  llvm::report_fatal_error("Compound assignment not supported for strings");
              }
              Builder.CreateStore(val, nameMapString[varName]);
              return;
          } else if(nameMapInt.count(varName)) {
              Builder.CreateStore(val, nameMapInt[varName]);
          } else if(nameMapChar.count(varName)) {
              Builder.CreateStore(val, nameMapChar[varName]);
          } else if(nameMapFloat.count(varName)) {
              Builder.CreateStore(val, nameMapFloat[varName]);
          } else if (nameMapArray.count(varName)) {
              llvm::report_fatal_error("Direct assignment to array variables not supported. Use element-wise assignment.");
          }
      }
    };

    virtual void visit(Final &Node) override {
      StringRef val = Node.getVal();

      if (Node.getKind() == Final::Ident) {
          // If the Final is an identifier, load from the corresponding map
          if (nameMapBool.count(val))
              V = Builder.CreateLoad(Int1Ty, nameMapBool[val]);
          else if (nameMapInt.count(val))
              V = Builder.CreateLoad(Int32Ty, nameMapInt[val]);
          else if (nameMapFloat.count(val))
              V = Builder.CreateLoad(FloatTy, nameMapFloat[val]);
          else if (nameMapChar.count(val))
            V = Builder.CreateLoad(Int8Ty, nameMapChar[val]);
          else if (nameMapString.count(val)) {
            V = Builder.CreateLoad(Int8PtrTy, nameMapString[val]);
            return;
          }else if (nameMapArray.count(val)) {
                // For now, just return the array pointer
                // V = nameMapArray[val];
            // For array variables in expressions, we need an index
            llvm::report_fatal_error("Array variable used without index: " + val);
          }else 
              llvm::report_fatal_error("Undefined variable: " + val);
          
      } else {
          // If it's a literal, check the kind and generate the appropriate constant
          if (Node.getKind() == Final::Float) {
              double floatVal;
              val.getAsDouble(floatVal); // NOT getAsFloat (that doesn't exist)
              V = ConstantFP::get(FloatTy, floatVal);
          } else if (Node.getKind() == Final::Number) {
              int intVal;
              val.getAsInteger(10, intVal);
              V = ConstantInt::get(Int32Ty, intVal, true);
          }else if (Node.getKind() == Final::Char) {
            // Handle character literals (assuming format like 'a')
            char charVal;  // Simple case - may need more complex parsing
            val.getAsInteger(10,charVal);
            V = ConstantInt::get(Int8Ty, charVal); 
          }else if (Node.getKind() == Final::String) {
            // Ensure string is properly null-terminated
            std::string str = val.str();
            if (!str.empty() && str.back() != '\0') {
                str += '\0';
            }
            V = Builder.CreateGlobalStringPtr(str);
            return;
          }else {
            llvm::report_fatal_error("Unknown literal kind");
          }
      }
    };

    // the others 
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

    Value* CreateExp(Value *Left, Value *Right)
    {
      AllocaInst* counterAlloca = Builder.CreateAlloca(Int32Ty);
      AllocaInst* resultAlloca = Builder.CreateAlloca(Int32Ty);
      Builder.CreateStore(Int32Zero, counterAlloca);
      Builder.CreateStore(Int32One, resultAlloca);

      llvm::BasicBlock* ForCondBB = llvm::BasicBlock::Create(M->getContext(), "exp.cond", Builder.GetInsertBlock()->getParent());
      llvm::BasicBlock* ForBodyBB = llvm::BasicBlock::Create(M->getContext(), "exp.body", Builder.GetInsertBlock()->getParent());
      llvm::BasicBlock* AfterForBB = llvm::BasicBlock::Create(M->getContext(), "after.exp", Builder.GetInsertBlock()->getParent());

      Builder.CreateBr(ForCondBB); //?

      Builder.SetInsertPoint(ForCondBB);
      Value* counterLoad = Builder.CreateLoad(counterAlloca);

      Value *cond = Builder.CreateICmpSLT(counterLoad, Right);
      Builder.CreateCondBr(cond, ForBodyBB, AfterForBB);

      Builder.SetInsertPoint(ForBodyBB);
      Value* resultLoad = Builder.CreateLoad(resultAlloca);

      Value* resultMul = Builder.CreateMul(resultLoad, Left);
      Value* counterInc = Builder.CreateAdd(counterLoad, Int32One);
      Builder.CreateStore(resultMul, resultAlloca);
      Builder.CreateStore(counterInc, counterAlloca);

      Builder.CreateBr(ForCondBB);
      Builder.SetInsertPoint(AfterForBB);

      Value* result = Builder.CreateLoad(resultAlloca);
      return result;
    }

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

    virtual void visit(SignedNumber &Node) override
    {
      int intval;
      Node.getValue().getAsInteger(10, intval);
      V = ConstantInt::get(Int32Ty, (Node.getSign() == SignedNumber::Minus) ? -intval : intval, true);
      if (Node.getType() == SignedNumber::Float){
        double floatVal;
        Node.getValue().getAsDouble(floatVal);
        V = ConstantFP::get(FloatTy, (Node.getSign() == SignedNumber::Minus) ? -floatVal : floatVal);
      }
    };

    virtual void visit(NegExpr &Node) override
    {
      Node.getExpr()->accept(*this);
      V = Builder.CreateNeg(V);
    };

    virtual void visit(LogicalExpr &Node) override{
      // Visit the left-hand side of the Logical operation and get its value.
      Node.getLeft()->accept(*this);
      Value *Left = V;

      if (Node.getRight() == nullptr)
      {
        V = Left;
        return; 
      }
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
      if (Node.getRight() == nullptr)
      {
        switch (Node.getOperator())
        {
        case Comparison::True:
          V = Int1True;
          break;
        case Comparison::False:
          V = Int1False;
          break;
        case Comparison::Ident: 
          if(isBool(((Final*)Node.getLeft())->getVal())){
            V = Builder.CreateLoad(Int1Ty, nameMapBool[((Final*)Node.getLeft())->getVal()]);
            break;
          }
          
          V = Builder.CreateLoad(Int32Ty, nameMapInt[((Final*)Node.getLeft())->getVal()]);
          break;
        
        default:
          break;
        }
        return;
      }
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
      if (isBool(Node.getVar())) {
        V = Builder.CreateLoad(Int1Ty, nameMapBool[Node.getVar()]);
        CallInst *Call = Builder.CreateCall(PrintBoolFnTy, PrintBoolFn, {V});
      }
      else if (nameMapInt.count(Node.getVar())) {
          V = Builder.CreateLoad(Int32Ty, nameMapInt[Node.getVar()]);
          CallInst *Call = Builder.CreateCall(PrintIntFnTy, PrintIntFn, {V});
      }
      else if (nameMapFloat.count(Node.getVar())) {
          V = Builder.CreateLoad(FloatTy, nameMapFloat[Node.getVar()]);
          CallInst *Call = Builder.CreateCall(PrintFloatFnTy, PrintFloatFn, {V});
      }
      else if (nameMapChar.count(Node.getVar())) {
          V = Builder.CreateLoad(Int8Ty, nameMapChar[Node.getVar()]);
          CallInst *Call = Builder.CreateCall(PrintCharFnTy, PrintCharFn, {V});
      }else if (nameMapString.count(Node.getVar())) {
          V = Builder.CreateLoad(Int8PtrTy, nameMapString[Node.getVar()]);
          CallInst *Call = Builder.CreateCall(PrintStringFnTy, PrintStringFn, {V});
      } 
    };

    virtual void visit(WhileStmt &Node) override
    {
      llvm::BasicBlock* WhileCondBB = llvm::BasicBlock::Create(M->getContext(), "while.cond", Builder.GetInsertBlock()->getParent());
      llvm::BasicBlock* WhileBodyBB = llvm::BasicBlock::Create(M->getContext(), "while.body", Builder.GetInsertBlock()->getParent());
      llvm::BasicBlock* AfterWhileBB = llvm::BasicBlock::Create(M->getContext(), "after.while", Builder.GetInsertBlock()->getParent());

      Builder.CreateBr(WhileCondBB); //?
      Builder.SetInsertPoint(WhileCondBB);
      Node.getCond()->accept(*this);
      Value* val=V;
      Builder.CreateCondBr(val, WhileBodyBB, AfterWhileBB);
      Builder.SetInsertPoint(WhileBodyBB);

      for (llvm::SmallVector<AST* >::const_iterator I = Node.begin(), E = Node.end(); I != E; ++I)
        {
            (*I)->accept(*this);
        }

      Builder.CreateBr(WhileCondBB);

      Builder.SetInsertPoint(AfterWhileBB);
        
    };

    virtual void visit(ForStmt &Node) override
    {
      // Create basic blocks for the for loop
      llvm::BasicBlock *CondBB = llvm::BasicBlock::Create(M->getContext(), "for.cond", Builder.GetInsertBlock()->getParent());
      llvm::BasicBlock *BodyBB = llvm::BasicBlock::Create(M->getContext(), "for.body", Builder.GetInsertBlock()->getParent());
      llvm::BasicBlock *AfterBB = llvm::BasicBlock::Create(M->getContext(), "for.after", Builder.GetInsertBlock()->getParent());

      // Handle initialization
      if (Node.getFirst())
      {
          // If it's an assignment from a declaration, create the variable
          if (Node.getFirst()->getLeft() && Node.getFirst()->getRightExpr())
          {
              llvm::StringRef varName = Node.getFirst()->getLeft()->getVal();
              // Create alloca for the variable
              llvm::AllocaInst *Alloca = Builder.CreateAlloca(Int32Ty);
              // Store the variable in the symbol table
              nameMapInt[varName] = Alloca;
              // Generate code for the initialization value
              Node.getFirst()->getRightExpr()->accept(*this);
              // Store the initialization value
              Builder.CreateStore(V, Alloca);
          }
          else
          {
              Node.getFirst()->accept(*this);
          }
      }

      // Branch to condition block
      Builder.CreateBr(CondBB);

      // Condition block
      Builder.SetInsertPoint(CondBB);
      if (Node.getSecond())
      {
          Node.getSecond()->accept(*this);
          Builder.CreateCondBr(V, BodyBB, AfterBB);
      }
      else
      {
          Builder.CreateBr(BodyBB);
      }

      // Body block
      Builder.SetInsertPoint(BodyBB);
      for (auto *S : Node)
      {
          S->accept(*this);
      }

      // Handle increment
      if (Node.getThirdAssign())
      {
          Node.getThirdAssign()->accept(*this);
      }
      else if (Node.getThirdUnary())
      {
          Node.getThirdUnary()->accept(*this);
      }

      // Branch back to condition
      Builder.CreateBr(CondBB);

      // After block
      Builder.SetInsertPoint(AfterBB);
    };

    virtual void visit(IfStmt &Node) override{
      llvm::BasicBlock* IfCondBB = llvm::BasicBlock::Create(M->getContext(), "if.cond", Builder.GetInsertBlock()->getParent());
      llvm::BasicBlock* IfBodyBB = llvm::BasicBlock::Create(M->getContext(), "if.body", Builder.GetInsertBlock()->getParent());
      llvm::BasicBlock* AfterIfBB = llvm::BasicBlock::Create(M->getContext(), "after.if", Builder.GetInsertBlock()->getParent());

      Builder.CreateBr(IfCondBB); //?
      Builder.SetInsertPoint(IfCondBB);
      Node.getCond()->accept(*this);
      Value* IfCondVal=V;

      Builder.SetInsertPoint(IfBodyBB);

      for (llvm::SmallVector<AST* >::const_iterator I = Node.begin(), E = Node.end(); I != E; ++I)
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
        for (llvm::SmallVector<AST* >::const_iterator I = Node.beginElse(), E = Node.endElse(); I != E; ++I)
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
      for (llvm::SmallVector<AST* >::const_iterator I = Node.begin(), E = Node.end(); I != E; ++I)
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
