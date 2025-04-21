#include "Sema.h"
#include "llvm/ADT/StringSet.h"
#include "llvm/Support/raw_ostream.h"


namespace nms{
class InputCheck : public ASTVisitor {
  llvm::StringSet<> IntScope; // StringSet to store declared int variables
  llvm::StringSet<> BoolScope; // StringSet to store declared int variables
  llvm::StringSet<> FloatScope; // StringSet to store declared int variables
  llvm::StringSet<> CharScope; // StringSet to store declared int variables
  llvm::StringSet<> StringScope; // StringSet to store declared int variables
  llvm::StringSet<> ArrayScope; // StringSet to store declared int variables

  bool HasError; // Flag to indicate if an error occurred

  enum ErrorType { Twice, Not }; // Enum to represent error types: Twice - variable declared twice, Not - variable not declared

  void error(ErrorType ET, llvm::StringRef V) {
    // Function to report errors
    llvm::errs() << "Variable " << V << " is "
                 << (ET == Twice ? "already" : "not")
                 << " declared\n";
    HasError = true; // Set error flag to true
  }

public:
  InputCheck() : HasError(false) {} // Constructor

  bool hasError() { return HasError; } // Function to check if an error occurred

  // Visit function for Program nodes
  virtual void visit(Program &Node) override { 

    for (llvm::SmallVector<AST *>::const_iterator I = Node.begin(), E = Node.end(); I != E; ++I)
    {
      (*I)->accept(*this); // Visit each child node
    }
  };

  virtual void visit(AST &Node) override {
    Node.accept(*this);
  }

  // Visit function for Final nodes
  virtual void visit(Final &Node) override {
    if (Node.getKind() == Final::Ident) {
      // Check if identifier is in the scope
      if (IntScope.find(Node.getVal()) == IntScope.end() && 
          BoolScope.find(Node.getVal()) == BoolScope.end() && 
          FloatScope.find(Node.getVal()) == FloatScope.end() &&
          CharScope.find(Node.getVal()) == CharScope.end() &&
          StringScope.find(Node.getVal()) == StringScope.end() &&
          ArrayScope.find(Node.getVal()) == ArrayScope.end()) {
        error(Not, Node.getVal());
      }
    }
  };

  // Visit function for BinaryOp nodes
  virtual void visit(BinaryOp &Node) override {
    Expr* right = Node.getRight();
    Expr* left = Node.getLeft();
    if (left)
      left->accept(*this);
    else
      HasError = true;

    if (right)
      right->accept(*this);
    else
      HasError = true;

    Final* l = (Final*)left;
    if (l->getKind() == Final::Ident){
      if (BoolScope.find(l->getVal()) != BoolScope.end()) {
        llvm::errs() << "Cannot use binary operation on a boolean variable: " << l->getVal() << "\n";
        HasError = true;
      }
    }

    Final* r = (Final*)right;
    if (r->getKind() == Final::Ident){
      if (BoolScope.find(r->getVal()) != BoolScope.end()) {
        llvm::errs() << "Cannot use binary operation on a boolean variable: " << r->getVal() << "\n";
        HasError = true;
      }
    }
    

    if (Node.getOperator() == BinaryOp::Operator::Div || Node.getOperator() == BinaryOp::Operator::Mod ) {
      Final* f = (Final*)right;

      if (f->getKind() == Final::ValueKind::Number) {
        llvm::StringRef intval = f->getVal();

        if (intval == "0") {
          llvm::errs() << "Division by zero is not allowed." << "\n";
          HasError = true;
        }
      }
    }
    
  };

  // Visit function for Assignment nodes and declarations
  virtual void visit(Assignment &Node) override {
    // Get the left-hand side expression
    Expr *array_dest = Node.getLeft();
    if (!array_dest)
        return;

    // Check if this is an array access by checking if the variable is in ArrayScope
    llvm::StringRef varName = static_cast<Final*>(array_dest)->getVal();
    if (ArrayScope.count(varName)) {
        // This is an array access assignment
        if (Node.getRightExpr()) {
            Expr *RightExpr = Node.getRightExpr();
            if (!RightExpr)
                return;

            // Check if right side is a Final node
            if (Final *rightFinal = static_cast<Final*>(RightExpr)) {
                // Handle array assignment with Final on right side
                if (rightFinal->getKind() == Final::Number) {
                    // Valid number assignment
                    return;
                }
                error(Not, "Invalid type for array assignment");
            }
        }
        return;
    }

    // Regular variable assignment
    if (Node.getRightExpr()) {
        Node.getRightExpr()->accept(*this);
    } else if (Node.getRightLogic()) {
        Node.getRightLogic()->accept(*this);
    }
  };

  virtual void visit(DeclarationInt &Node) override {
    for (llvm::SmallVector<Expr *>::const_iterator I = Node.valBegin(), E = Node.valEnd(); I != E; ++I){
      (*I)->accept(*this); // If the Declaration node has an expression, recursively visit the expression node
    }
    for (llvm::SmallVector<llvm::StringRef>::const_iterator I = Node.varBegin(), E = Node.varEnd(); I != E;
         ++I) {
      if(BoolScope.find(*I) != BoolScope.end()){
        llvm::errs() << "Variable " << *I << " is already declared as an boolean" << "\n";
        HasError = true; 
      }
      else{
        if (!IntScope.insert(*I).second)
          error(Twice, *I); // If the insertion fails (element already exists in Scope), report a "Twice" error
      }
    }
  };

  virtual void visit(DeclarationFloat &Node) override {
    for (llvm::SmallVector<Expr *>::const_iterator I = Node.valBegin(), E = Node.valEnd(); I != E; ++I){
      (*I)->accept(*this); // If the Declaration node has an expression, recursively visit the expression node
    }
    for (llvm::SmallVector<llvm::StringRef>::const_iterator I = Node.varBegin(), E = Node.varEnd(); I != E;
         ++I) {
      if(BoolScope.find(*I) != BoolScope.end()){
        llvm::errs() << "Variable " << *I << " is already declared as an boolean" << "\n";
        HasError = true; 
      }
      else if(IntScope.find(*I) != IntScope.end()){
        llvm::errs() << "Variable " << *I << " is already declared as an boolean" << "\n";
        HasError = true; 
      }
      else{
        if (!FloatScope.insert(*I).second)
          error(Twice, *I); // If the insertion fails (element already exists in Scope), report a "Twice" error
      }
    }
  };

  virtual void visit(DeclarationChar &Node) override {
    for (llvm::SmallVector<Expr *>::const_iterator I = Node.valBegin(), E = Node.valEnd(); I != E; ++I){
      (*I)->accept(*this); // If the Declaration node has an expression, recursively visit the expression node
    }
    for (llvm::SmallVector<llvm::StringRef>::const_iterator I = Node.varBegin(), E = Node.varEnd(); I != E;
         ++I) {
      if(BoolScope.find(*I) != BoolScope.end()){
        llvm::errs() << "Variable " << *I << " is already declared as an boolean" << "\n";
        HasError = true; 
      }
      else if(IntScope.find(*I) != IntScope.end()){
        llvm::errs() << "Variable " << *I << " is already declared as an boolean" << "\n";
        HasError = true; 
      }
      else if(FloatScope.find(*I) != FloatScope.end()){
        llvm::errs() << "Variable " << *I << " is already declared as an boolean" << "\n";
        HasError = true; 
      }
      else{
        if (!CharScope.insert(*I).second)
          error(Twice, *I); // If the insertion fails (element already exists in Scope), report a "Twice" error
      }
    }
  };

  virtual void visit(DeclarationString &Node) override {
    for (llvm::SmallVector<Expr *>::const_iterator I = Node.valBegin(), E = Node.valEnd(); I != E; ++I){
      (*I)->accept(*this); // If the Declaration node has an expression, recursively visit the expression node
    }
    for (llvm::SmallVector<llvm::StringRef>::const_iterator I = Node.varBegin(), E = Node.varEnd(); I != E;
         ++I) {
      if(BoolScope.find(*I) != BoolScope.end()){
        llvm::errs() << "Variable " << *I << " is already declared as an boolean" << "\n";
        HasError = true; 
      }
      else if(IntScope.find(*I) != IntScope.end()){
        llvm::errs() << "Variable " << *I << " is already declared as an boolean" << "\n";
        HasError = true; 
      }
      else if(FloatScope.find(*I) != FloatScope.end()){
        llvm::errs() << "Variable " << *I << " is already declared as an boolean" << "\n";
        HasError = true; 
      }
      else if(CharScope.find(*I) != CharScope.end()){
        llvm::errs() << "Variable " << *I << " is already declared as an boolean" << "\n";
        HasError = true; 
      }
      else{
        if (!StringScope.insert(*I).second)
          error(Twice, *I); // If the insertion fails (element already exists in Scope), report a "Twice" error
      }
    }
  };

  virtual void visit(DeclarationBool &Node) override {
    for (llvm::SmallVector<Logic *>::const_iterator I = Node.valBegin(), E = Node.valEnd(); I != E; ++I){
      (*I)->accept(*this); // If the Declaration node has an expression, recursively visit the expression node
    }
    for (llvm::SmallVector<llvm::StringRef>::const_iterator I = Node.varBegin(), E = Node.varEnd(); I != E;
         ++I) {
      if(IntScope.find(*I) != IntScope.end()){
        llvm::errs() << "Variable " << *I << " is already declared as an integer" << "\n";
        HasError = true; 
      }
      else{
        if (!BoolScope.insert(*I).second)
          error(Twice, *I); // If the insertion fails (element already exists in Scope), report a "Twice" error
      }
    }
    
  };

  virtual void visit(DeclarationArray &Node) override {
    // Check all elements in the array
    for (auto I = Node.valBegin(), E = Node.valEnd(); I != E; ++I) {
      (*I)->accept(*this);
    }
    
    // Add array variables to the ArrayScope
    for (auto I = Node.varBegin(), E = Node.varEnd(); I != E; ++I) {
      if (IntScope.find(*I) != IntScope.end() ||
          BoolScope.find(*I) != BoolScope.end() ||
          FloatScope.find(*I) != FloatScope.end() ||
          CharScope.find(*I) != CharScope.end() ||
          StringScope.find(*I) != StringScope.end()) {
        llvm::errs() << "Variable " << *I << " is already declared as another type\n";
        HasError = true;
      } else {
        if (!ArrayScope.insert(*I).second)
          error(Twice, *I);
      }
    }
  };

  virtual void visit(ArrayAccess &Node) override {
    // Check if array is declared
    if (!ArrayScope.count(Node.getArrayName())) {
        error(Not, Node.getArrayName());
        return;
    }

    // Check index expression
    Expr *index = Node.getIndex();
    if (!index)
        return;

    // Check if index is a Final node
    if (Final *indexFinal = static_cast<Final*>(index)) {
        if (indexFinal->getKind() == Final::Number) {
            // Valid numeric index
            return;
        }
        error(Not, "Array index must be a number");
    }
  };
  
  
  
  // comparision and logical and other type
  virtual void visit(Comparison &Node) override {
    if(Node.getLeft()){
      Node.getLeft()->accept(*this);
    }
    if(Node.getRight()){
      Node.getRight()->accept(*this);
    }
    // else{
    //   if (Node.getOperator() == Comparison::Ident){
    //     Final* F = (Final*)(Node.getLeft());
    //     if (BoolScope.find(F->getVal()) == BoolScope.end()) {
    //       llvm::errs() << "you need a boolean varaible to compare or assign: "<< F->getVal() << "\n";
    //       HasError = true;
    //     } 
    //   }
    // }

    if (Node.getOperator() != Comparison::True && Node.getOperator() != Comparison::False && Node.getOperator() != Comparison::Ident){
      Final* L = (Final*)(Node.getLeft());
      if(L){
        if (L->getKind() == Final::ValueKind::Ident && IntScope.find(L->getVal()) == IntScope.end()) {
          llvm::errs() << "you can only compare a defined integer variable: "<< L->getVal() << "\n";
          HasError = true;
        } 
      }
      
      Final* R = (Final*)(Node.getRight());
      if(R){
        if (R->getKind() == Final::ValueKind::Ident && IntScope.find(R->getVal()) == IntScope.end()) {
          llvm::errs() << "you can only compare a defined integer variable: "<< R->getVal() << "\n";
          HasError = true;
        } 
      }
    }
  };

  virtual void visit(LogicalExpr &Node) override {
    if(Node.getLeft()){
      Node.getLeft()->accept(*this);
    }
    if(Node.getRight()){
      Node.getRight()->accept(*this);
    }
  };

  virtual void visit(UnaryOp &Node) override {
    if (IntScope.find(Node.getIdent()) == IntScope.end()){
      llvm::errs() << "Variable "<<Node.getIdent() << " is not a defined integer variable." << "\n";
      HasError = true;
    }
  };

  virtual void visit(NegExpr &Node) override {
    Expr *expr = Node.getExpr();
    (*expr).accept(*this);
  };

  virtual void visit(PrintStmt &Node) override {
    // Check if identifier is in the scope
    if (IntScope.find(Node.getVar()) == IntScope.end() && BoolScope.find(Node.getVar()) == BoolScope.end())
      error(Not, Node.getVar());
    
  };

  virtual void visit(IfStmt &Node) override {
    Logic *l = Node.getCond();
    (*l).accept(*this);

    for (llvm::SmallVector<AST *>::const_iterator I = Node.begin(), E = Node.end(); I != E; ++I) {
      (*I)->accept(*this);
    }
    for (llvm::SmallVector<AST *>::const_iterator I = Node.beginElse(), E = Node.endElse(); I != E; ++I){
      (*I)->accept(*this);
    }
    for (llvm::SmallVector<elifStmt *>::const_iterator I = Node.beginElif(), E = Node.endElif(); I != E; ++I){
      (*I)->accept(*this);
    }
  };

  virtual void visit(elifStmt &Node) override {
    Logic* l = Node.getCond();
    (*l).accept(*this);

    for (llvm::SmallVector<AST *>::const_iterator I = Node.begin(), E = Node.end(); I != E; ++I) {
      (*I)->accept(*this);
    }
  };

  virtual void visit(WhileStmt &Node) override {
    Logic* l = Node.getCond();
    (*l).accept(*this);

    for (llvm::SmallVector<AST *>::const_iterator I = Node.begin(), E = Node.end(); I != E; ++I) {
      (*I)->accept(*this);
    }
  };

  virtual void visit(ForStmt &Node) override {
    // Visit the initialization part
    if (Node.getFirst())
    {
        // If it's an assignment from a declaration, add the variable to the symbol table
        if (Node.getFirst()->getLeft() && Node.getFirst()->getRightExpr())
        {
            llvm::StringRef varName = Node.getFirst()->getLeft()->getVal();
            // Add the variable to the symbol table
            IntScope.insert(varName);
        }
        Node.getFirst()->accept(*this);
    }

    // Visit the condition
    if (Node.getSecond())
    {
        Node.getSecond()->accept(*this);
    }

    // Visit the increment part
    if (Node.getThirdAssign())
    {
        Node.getThirdAssign()->accept(*this);
    }
    else if (Node.getThirdUnary())
    {
        Node.getThirdUnary()->accept(*this);
    }

    // Visit the body
    for (auto *S : Node)
    {
        S->accept(*this);
    }
  };

  virtual void visit(SignedNumber &Node) override {
  };

  virtual void visit(LengthFunction &Node) override {
      if (ArrayScope.find(Node.getArrayName()) == ArrayScope.end()) {
          error(Not, Node.getArrayName());
      }
  }

  virtual void visit(MinFunction &Node) override {
      if (ArrayScope.find(Node.getArrayName()) == ArrayScope.end()) {
          error(Not, Node.getArrayName());
      }
  }

};
}

bool Sema::semantic(Program *Tree) {
  if (!Tree)
    return false; // If the input AST is not valid, return false indicating no errors
  nms::InputCheck *Check = new nms::InputCheck();;// Create an instance of the InputCheck class for semantic analysis
  Tree->accept(*Check); // Initiate the semantic analysis by traversing the AST using the accept function

  return Check->hasError(); // Return the result of Check.hasError() indicating if any errors were detected during the analysis
}
