#include "Parser.h"


// main point is that the whole input has been consumed
Program *Parser::parse()
{
    Program *Res = parseProgram();
    return Res;
}

Program *Parser::parseProgram(){
    llvm::SmallVector<AST *> data;
    
    while (!Tok.is(Token::eoi))
    {
        switch (Tok.getKind())
        {
        case Token::KW_int: {
            DeclarationInt *d;
            d = parseIntDec();
            if (d)
                data.push_back(d);
            else
                goto _error;
                
            break;
        }
        case Token::KW_array: {
            DeclarationArray *arr = parseArrayDec();
            if (arr)
                data.push_back(arr);
            else
                goto _error;
            break;
        }
        case Token::KW_string: {
            DeclarationString *d;
            d = parseStringDec();
            if (d)
                data.push_back(d);
            else
                goto _error;
            break;
        }
        case Token::KW_char: {
            // DeclarationChar *d;
            DeclarationChar *d;
            d = parseCharDec();
            if (d)
                data.push_back(d);
            else
                goto _error;
                
            break;
        }
        case Token::KW_float: {
            DeclarationFloat *d;
            d = parseFloatDec();
            if (d)
                data.push_back(d);
            else
                goto _error;
                
            break;
        }
        case Token::KW_boolean: {
            DeclarationBool *dbool;
            dbool = parseBoolDec();
            if (dbool)
                data.push_back(dbool);
            else
                goto _error;

            break;
        }
        case Token::ident: {
            // llvm::errs() << "start ident\n";
            Token prev_token = Tok;
            const char* prev_buffer = Lex.getBuffer();
            UnaryOp *u;
            u = parseUnary();
            if (Tok.is(Token::semicolon))
            {
                if (u)
                {
                    data.push_back(u);
                    break;
                }
                else{
                    goto _error;
                    
                }
            }
            else
            {
                if (u)
                {
                    goto _error;
                }
                else{
                    Tok = prev_token;
                    Lex.setBufferPtr(prev_buffer);
                }
            }
            // llvm::errs() << "end unary\n";
        
            Assignment *a_int;
            Assignment *a_bool;
            prev_token = Tok;
            prev_buffer = Lex.getBuffer();

            a_bool = parseBoolAssign();

            if (a_bool){
                data.push_back(a_bool);
                break;
            }
            Tok = prev_token;
            Lex.setBufferPtr(prev_buffer);
            llvm::errs() << "hereeeee\n";
            a_int = parseIntAssign();
            if (!Tok.is(Token::semicolon))
            {
                goto _error;
            }
            if (a_int){
                llvm::errs() << "get_value ident\n";
                data.push_back(a_int);
            }
            else
                goto _error;
                
            break;
        }
        case Token::KW_if: {
            IfStmt *i;
            i = parseIf();
            if (i)
                data.push_back(i);
            else
                goto _error;
            
            break;
        }
        case Token::KW_while: {
            WhileStmt *w;
            w = parseWhile();
            if (w)
                data.push_back(w);
            else {
                goto _error;
            }
            break;
        }
        case Token::KW_for: {
            ForStmt *f;
            f = parseFor();
            if (f)
                data.push_back(f);
            else {
                goto _error;
            }
            break;
        }
        case Token::KW_print: {
            PrintStmt *p;
            p = parsePrint();
            if (p)
                data.push_back(p);
            else {
                goto _error;
            }
            break;
        }
        case Token::start_comment: {
            parseComment();
            if (!Tok.is(Token::end_comment))
                goto _error;
            break;
        }
        default: {
            error();

            goto _error;
            break;
        }
        }
        advance();
        
    }
    return new Program(data);
_error:
    while (Tok.getKind() != Token::eoi)
        advance();
    return nullptr;
}

// declarations
DeclarationArray* Parser::parseArrayDec() {
    // Initialize variables at the start
    llvm::StringRef varName;
    llvm::SmallVector<Expr*> elements;
    llvm::SmallVector<llvm::StringRef> varNames;
    
    // Parse array keyword
    if (expect(Token::KW_array)) {
        while (Tok.getKind() != Token::eoi)
            advance();
        return nullptr;
    }
    advance();
    
    // Parse identifier
    if (expect(Token::ident)) {
        while (Tok.getKind() != Token::eoi)
            advance();
        return nullptr;
    }
    varName = Tok.getText();
    advance();
    
    // Parse assignment
    if (expect(Token::assign)) {
        while (Tok.getKind() != Token::eoi)
            advance();
        return nullptr;
    }
    advance();
    
    // Parse left bracket
    if (expect(Token::l_bracket)) {
        while (Tok.getKind() != Token::eoi)
            advance();
        return nullptr;
    }
    advance();
    
    // Parse array elements
    elements = parseArrayElements();
    // for (auto e : elements) {
    //      llvm::errs() <<
    // }

    
    // Parse right bracket
   

    if (expect(Token::r_bracket)) {
         llvm::errs() << Tok.getKind() << '\n';
        while (Tok.getKind() != Token::eoi){
             llvm::errs() << Tok.getKind() << '\n';
            advance();
        }
        return nullptr;
    }
    advance();
     //  exit(0);
   
    // Parse semicolon
    if (expect(Token::semicolon)) {
        while (Tok.getKind() != Token::eoi)
            advance();
        return nullptr;
    }
    
    // Add variable name to vector
    varNames.push_back(varName);
    
    return new DeclarationArray(varNames, elements);
}

llvm::SmallVector<Expr*> Parser::parseArrayElements() {
    llvm::SmallVector<Expr*> elements;
    
    // Handle empty array case
    if (Tok.is(Token::r_bracket))
        return elements;
    
    // Parse first element
    Expr* elem = parseExpr();
    if (!elem)
        goto _error;
    elements.push_back(elem);
    
    // Parse remaining elements
    while (Tok.is(Token::comma)) {
        advance();
        
        elem = parseExpr();
        if (!elem)
            goto _error;
        elements.push_back(elem);
    }
    
    return elements;

_error:
    while (Tok.getKind() != Token::eoi)
        advance();
    return llvm::SmallVector<Expr*>();
}

DeclarationFloat *Parser::parseFloatDec()
{
    Expr *E = nullptr;
    llvm::SmallVector<llvm::StringRef> Vars;
    llvm::SmallVector<Expr *> Values;
    
    if (expect(Token::KW_float)){
        goto _error;
    }
    advance();
    if (expect(Token::ident)){
        goto _error;
    }

    Vars.push_back(Tok.getText());
    advance();

    if (Tok.is(Token::assign))
    {
        advance();
        E = parseExpr();
        if(E){
            Values.push_back(E);
        }
        else{
            goto _error;
        }
    }
    else
    {
        Values.push_back(new Final(Final::Number, llvm::StringRef("0")));
    }
    
    
    while (Tok.is(Token::comma))
    {
        advance();
        if (expect(Token::ident)){
            goto _error;
        }
            
        Vars.push_back(Tok.getText());
        advance();

        if(Tok.is(Token::assign)){
            advance();
            E = parseExpr();
            if(E){
                Values.push_back(E);
            }
            else{
                goto _error;
            }
        }
        else{
            Values.push_back(new Final(Final::Number, llvm::StringRef("0")));
        }
    }

    if (expect(Token::semicolon)){
        goto _error;
    }


    return new DeclarationFloat(Vars, Values);
_error: 
    while (Tok.getKind() != Token::eoi)
        advance();
    return nullptr;
}

DeclarationInt *Parser::parseIntDec()
{
    Expr *E = nullptr;
    llvm::SmallVector<llvm::StringRef> Vars;
    llvm::SmallVector<Expr *> Values;
    
    if (expect(Token::KW_int)){
        goto _error;
    }
    advance();
    
    if (expect(Token::ident)){
        goto _error;
    }

    Vars.push_back(Tok.getText());
    advance();

    if (Tok.is(Token::assign))
    {
        advance();
        E = parseExpr();
        if(E){
            Values.push_back(E);
        }
        else{
            goto _error;
        }
    }
    else
    {
        Values.push_back(new Final(Final::Number, llvm::StringRef("0")));
    }
    
    
    while (Tok.is(Token::comma))
    {
        advance();
        if (expect(Token::ident)){
            goto _error;
        }
            
        Vars.push_back(Tok.getText());
        advance();

        if(Tok.is(Token::assign)){
            advance();
            E = parseExpr();
            if(E){
                Values.push_back(E);
            }
            else{
                goto _error;
            }
        }
        else{
            Values.push_back(new Final(Final::Number, llvm::StringRef("0")));
        }
    }

    if (expect(Token::semicolon)){
        goto _error;
    }


    return new DeclarationInt(Vars, Values);
_error: 
    while (Tok.getKind() != Token::eoi)
        advance();
    return nullptr;
}

DeclarationChar *Parser::parseCharDec()
{
    Expr *E = nullptr;
    llvm::SmallVector<llvm::StringRef> Vars;
    llvm::SmallVector<Expr *> Values;
    
    if (expect(Token::KW_char)){
        goto _error;
    }
    advance();
    
    if (expect(Token::ident)){
        goto _error;
    }

    Vars.push_back(Tok.getText());
    advance();

    if (Tok.is(Token::assign))
    {
        advance();
        E = parseExpr();
        if(E){
            Values.push_back(E);
        }
        else{
            goto _error;
        }
    }
    else
    {
        Values.push_back(new Final(Final::Number, llvm::StringRef("\\0")));
    }
    
    
    while (Tok.is(Token::comma))
    {
        advance();
        if (expect(Token::ident)){
            goto _error;
        }
            
        Vars.push_back(Tok.getText());
        advance();

        if(Tok.is(Token::assign)){
            advance();
            E = parseExpr();
            if(E){
                Values.push_back(E);
            }
            else{
                goto _error;
            }
        }
        else{
            Values.push_back(new Final(Final::Number, llvm::StringRef("\\0")));
        }
    }

    if (expect(Token::semicolon)){
        goto _error;
    }


    return new DeclarationChar(Vars, Values);
_error: 
    while (Tok.getKind() != Token::eoi)
        advance();
    return nullptr;
}

DeclarationBool *Parser::parseBoolDec()
{
    Logic *L = nullptr;
    llvm::SmallVector<llvm::StringRef> Vars;
    llvm::SmallVector<Logic *> Values;
    
    if (expect(Token::KW_boolean)){
        goto _error;
    }
    advance();
    
    if (expect(Token::ident)){
        goto _error;
    }

    Vars.push_back(Tok.getText());
    advance();

    if (Tok.is(Token::assign))
    {
        advance();
        L = parseLogic();
        if(L){
            Values.push_back(L);
        }
        else{
            goto _error;
        }
    }
    else
    {
        Values.push_back(new Comparison(nullptr, nullptr, Comparison::False));
    }
    
    
    while (Tok.is(Token::comma))
    {
        advance();
        if (expect(Token::ident)){
            goto _error;
        }
            
        Vars.push_back(Tok.getText());
        advance();

        if(Tok.is(Token::assign)){
            advance();
            L = parseLogic();
            if(L){
                Values.push_back(L);
            }
            else{
                goto _error;
            }
        }
        else{
            Values.push_back(new Comparison(nullptr, nullptr, Comparison::False));
        }
    }

    if (expect(Token::semicolon)){
        goto _error;
    }
    return new DeclarationBool(Vars, Values);
_error: 
    while (Tok.getKind() != Token::eoi)
        advance();
    return nullptr;
}

DeclarationString *Parser::parseStringDec()
{
    llvm::SmallVector<llvm::StringRef> Vars;
    llvm::SmallVector<Expr *> Values;  // Changed from Logic* to Expr*
    
    if (expect(Token::KW_string)) {
        goto _error;
    }
    advance();
    
    if (expect(Token::ident)) {
        goto _error;
    }

    Vars.push_back(Tok.getText());
    advance();

    if (Tok.is(Token::assign)) {
        advance();
        if (expect(Token::string)) {
            goto _error;
        }
        // Create a Final node for the string literal
        Values.push_back(new Final(Final::String, Tok.getText()));
        advance();
    } else {
        Values.push_back(nullptr); // Default empty string
    }
    
    while (Tok.is(Token::comma)) {
        advance();
        if (expect(Token::ident)) {
            goto _error;
        }
            
        Vars.push_back(Tok.getText());
        advance();

        if (Tok.is(Token::assign)) {
            advance();
            if (expect(Token::string)) {
                goto _error;
            }
            Values.push_back(new Final(Final::String, Tok.getText()));
            advance();
        } else {
            Values.push_back(nullptr); // Default empty string
        }
    }

    if (expect(Token::semicolon)) {
        goto _error;
    }

    return new DeclarationString(Vars, Values);

_error:
    while (Tok.getKind() != Token::eoi)
        advance();
    return nullptr;
}


// assignments
Assignment *Parser::parseBoolAssign()
{
    Logic *L = nullptr;
    Final *F = nullptr;
    // Assignment::AssignKind AK;

    // F = (Final *)(parseFinal());
    // if (F == nullptr)
    // {
    //     goto _error;
    // }
    Expr *LHS = nullptr;
    Assignment::AssignKind AK;
    
    // Try to parse array access as LHS
    Token prev_tok = Tok;
    const char* prev_buffer = Lex.getBuffer();
    LHS = parseArrayAccess();
    if (!LHS) {
        // Not array access, try regular variable
        Tok = prev_tok;
        Lex.setBufferPtr(prev_buffer);
        // LHS = parseFinal();
        F = (Final *)(parseFinal());
    }
    
    if (Tok.is(Token::assign))
    {
        AK = Assignment::Assign;
        advance();
        L = parseLogic();   // check if expr is logical

        if(L)
        {
            if (!Tok.is(Token::semicolon))
            {
                goto _error;
            }
            return new Assignment(F, nullptr, AK, L);
        }
        else
            goto _error;
    }
    else
    {
        goto _error;
    }
    
_error:
        while (Tok.getKind() != Token::eoi)
            advance();
        return nullptr;
    
}

Assignment *Parser::parseIntAssign()
{
    // Try to parse array access first
    AST *LHS = parseArrayAccess();
    if (LHS) {
        // If we have an array access, check for assignment operators
        if (Tok.is(Token::assign))
        {
            advance();
            Expr *E = parseExpr();
            if (!E)
                return nullptr;
            return new Assignment(nullptr, E, Assignment::Assign, nullptr);
        }
        else if (Tok.is(Token::plus_assign))
        {
            advance();
            Expr *E = parseExpr();
            if (!E)
                return nullptr;
            return new Assignment(nullptr, E, Assignment::Plus_assign, nullptr);
        }
        else if (Tok.is(Token::minus_assign))
        {
            advance();
            Expr *E = parseExpr();
            if (!E)
                return nullptr;
            return new Assignment(nullptr, E, Assignment::Minus_assign, nullptr);
        }
        else if (Tok.is(Token::star_assign))
        {
            advance();
            Expr *E = parseExpr();
            if (!E)
                return nullptr;
            return new Assignment(nullptr, E, Assignment::Star_assign, nullptr);
        }
        else if (Tok.is(Token::slash_assign))
        {
            advance();
            Expr *E = parseExpr();
            if (!E)
                return nullptr;
            return new Assignment(nullptr, E, Assignment::Slash_assign, nullptr);
        }
        else if (Tok.is(Token::plus_plus))
        {
            advance();
            // Create a Final node with value "1" for the increment
            Final *one = new Final(Final::Number, "1");
            return new Assignment(nullptr, one, Assignment::Plus_assign, nullptr);
        }
        else
        {
            // If no assignment operator, return nullptr
            return nullptr;
        }
    }

    // If not an array access, try to parse a regular variable
    Expr *F_expr = parseFinal();
    if (!F_expr)
        return nullptr;

    // Cast the Expr* to Final* since we know parseFinal returns a Final*
    Final *F = static_cast<Final*>(F_expr);

    Assignment::AssignKind AK;
    if (Tok.is(Token::assign))
    {
        advance();
        AK = Assignment::Assign;
    }
    else if (Tok.is(Token::plus_assign))
    {
        advance();
        AK = Assignment::Plus_assign;
    }
    else if (Tok.is(Token::minus_assign))
    {
        advance();
        AK = Assignment::Minus_assign;
    }
    else if (Tok.is(Token::star_assign))
    {
        advance();
        AK = Assignment::Star_assign;
    }
    else if (Tok.is(Token::slash_assign))
    {
        advance();
        AK = Assignment::Slash_assign;
    }
    else if (Tok.is(Token::plus_plus))
    {
        advance();
        // Create a Final node with value "1" for the increment
        Final *one = new Final(Final::Number, "1");
        return new Assignment(F, one, Assignment::Plus_assign, nullptr);
    }
    else
    {
        return nullptr;
    }

    Expr *E = parseExpr();
    if (!E)
        return nullptr;
    return new Assignment(F, E, AK, nullptr);
}

Assignment *Parser::parseFloatAssign()
{
    Expr *E = nullptr;
    Final *F = nullptr;
    // Assignment::AssignKind AK;
    // F = (Final *)(parseFinal());
    // if (F == nullptr)
    // {
    //     goto _error;
    // }
    Expr *LHS = nullptr;
    Assignment::AssignKind AK;
    
    // Try to parse array access as LHS
    Token prev_tok = Tok;
    const char* prev_buffer = Lex.getBuffer();
    LHS = parseArrayAccess();
    if (!LHS) {
        // Not array access, try regular variable
        Tok = prev_tok;
        Lex.setBufferPtr(prev_buffer);
        // LHS = parseFinal();
        F = (Final *)(parseFinal());
    }
    
    if (Tok.is(Token::assign))
    {
        AK = Assignment::Assign;
    }
    else if (Tok.is(Token::plus_assign))
    {
        AK = Assignment::Plus_assign;
    }
    else if (Tok.is(Token::minus_assign))
    {
        AK = Assignment::Minus_assign;
    }
    else if (Tok.is(Token::star_assign))
    {
        AK = Assignment::Star_assign;
    }
    else if (Tok.is(Token::slash_assign))
    {
        AK = Assignment::Slash_assign;
    }
    else
    {
        goto _error;
    }
    advance();
    E = parseExpr();    // check for mathematical expr
    if(E){
        return new Assignment(F, E, AK, nullptr);
    }
    else{
        goto _error;
    }

_error:
        while (Tok.getKind() != Token::eoi)
            advance();
        return nullptr;
}

Assignment *Parser::parseCharAssign()
{
    Final *F = nullptr;
    Assignment::AssignKind AK;
    Expr *E = nullptr;

    F = (Final *)(parseFinal());
    if (F == nullptr)
    {
        goto _error;
    }
    
    if (Tok.is(Token::assign))
    {
        AK = Assignment::Assign;
        advance();
        if (Tok.is(Token::character)) {
            E = new Final(Final::Char, Tok.getText());
            advance();
        }
        else {
            goto _error;
        }
        if(E){
            return new Assignment(F, E, AK, nullptr);
        }
        else{
            goto _error;
        }
    }
    else
    {
        goto _error;
    }
    
_error:
    while (Tok.getKind() != Token::eoi)
        advance();
    return nullptr;
}

Assignment *Parser::parseStringAssign()
{
    Final *F = nullptr;
    Assignment::AssignKind AK;
    Expr *E = nullptr;

    F = (Final *)(parseFinal());
    if (F == nullptr)
    {
        goto _error;
    }
    
    if (Tok.is(Token::assign))
    {
        AK = Assignment::Assign;
        advance();
        if (Tok.is(Token::string)) {
            E = new Final(Final::String, Tok.getText());
            advance();
        }
        else {
            goto _error;
        }
        if(E){
            return new Assignment(F, E, AK, nullptr);
        }
        else{
            goto _error;
        }
    }
    else
    {
        goto _error;
    }
    
_error:
    while (Tok.getKind() != Token::eoi)
        advance();
    return nullptr;
}


// unary
UnaryOp *Parser::parseUnary()
{
    UnaryOp* Res = nullptr;
    llvm::StringRef var;

    if (expect(Token::ident)){
        goto _error;
    }

    var = Tok.getText();
    advance();
    if (Tok.getKind() == Token::plus_plus){
        Res = new UnaryOp(UnaryOp::Plus_plus, var);
    }
    else if(Tok.getKind() == Token::minus_minus){
        Res = new UnaryOp(UnaryOp::Minus_minus, var);
    }
    else{
        goto _error;
    }

    advance();

    return Res;

_error:
    while (Tok.getKind() != Token::eoi)
        advance();
    return nullptr;
}

// expressions
Expr *Parser::parseExpr()
{
    Expr *Left = parseTerm();

    if (Left == nullptr)
    {
        goto _error;
    }
    
    while (Tok.isOneOf(Token::plus, Token::minus))
    {
        BinaryOp::Operator Op;
        if (Tok.is(Token::plus))
            Op = BinaryOp::Plus;
        else if (Tok.is(Token::minus))
            Op = BinaryOp::Minus;
        else {
            error();

            goto _error;
        }
        advance();
        Expr *Right = parseTerm();
        if (Right == nullptr)
        {
            goto _error;
        }
        Left = new BinaryOp(Op, Left, Right);
    }
    return Left;

_error:
    while (Tok.getKind() != Token::eoi)
        advance();
    return nullptr;
}

Expr *Parser::parseTerm()
{
    Expr *Left = parseFactor();
    if (Left == nullptr)
    {
        goto _error;
    }
    while (Tok.isOneOf(Token::star, Token::mod, Token::slash))
    {
        BinaryOp::Operator Op;
        if (Tok.is(Token::star))
            Op = BinaryOp::Mul;
        else if (Tok.is(Token::slash))
            Op = BinaryOp::Div;
        else if (Tok.is(Token::mod))
            Op = BinaryOp::Mod;
        else {
            error();

            goto _error;
        }
        advance();
        Expr *Right = parseFactor();
        if (Right == nullptr)
        {
            goto _error;
        }
        Left = new BinaryOp(Op, Left, Right);
    }
    return Left;

_error:
    while (Tok.getKind() != Token::eoi)
        advance();
    return nullptr;
}

Expr *Parser::parseFactor() {
    // Try to parse length function first
    if (Tok.is(Token::KW_length)) {
        return parseLengthFunction();
    }
    else if (Tok.is(Token::KW_index)) {
        return parseIndexFunction();
    }
    else if (Tok.is(Token::KW_min)) {
        return parseMinFunction();
    }
    else if (Tok.is(Token::KW_max)) {
        return parseMaxFunction();
    }
    else if (Tok.is(Token::KW_pow)) {
        return parsePowFunction();
    }

    Expr *Left = parseFinal();
    if (Left == nullptr) {
        goto _error;
    }
    while (Tok.is(Token::exp)) {
        BinaryOp::Operator Op;
        if (Tok.is(Token::exp))
            Op = BinaryOp::Exp;
        else {
            error();
            goto _error;
        }
        advance();
        Expr *Right = parseFactor();
        if (Right == nullptr) {
            goto _error;
        }
        Left = new BinaryOp(Op, Left, Right);
    }
    return Left;

_error:
    while (Tok.getKind() != Token::eoi)
        advance();
    return nullptr;
}

Expr *Parser::parseFinal() {
    Expr *Res = nullptr;
    Token prev_tok = Tok;
    const char* prev_buffer = Lex.getBuffer();
    Res = parseArrayAccess();
    if (Res) {
        return Res;
    }

    switch (Tok.getKind()) {
    case Token::number: {
        Res = new Final(Final::Number, Tok.getText());
        advance();
        break;
    }
    case Token::KW_true: {
        Res = new Final(Final::Number, "1");
        advance();
        break;
    }
    case Token::KW_false: {
        Res = new Final(Final::Number, "0");
        advance();
        break;
    }
    case Token::float_num: {
        Res = new Final(Final::Float, Tok.getText());
        advance();
        break;
    }
    case Token::character: {
        Res = new Final(Final::Char, Tok.getText());
        advance();
        break;
    }
    case Token::string: {
        Res = new Final(Final::String, Tok.getText());
        advance();
        break;
    }
    case Token::ident: {
        Res = new Final(Final::Ident, Tok.getText());
        Token prev_tok = Tok;
        const char* prev_buffer = Lex.getBuffer();
        Expr* u = parseUnary();
        if(u)
            return u;
        else {
            Tok = prev_tok;
            Lex.setBufferPtr(prev_buffer);
            advance();
        }
        break;
    }
    case Token::plus: {
        advance();
        if(Tok.getKind() == Token::number) {
            Res = new SignedNumber(SignedNumber::Plus, Tok.getText());
            advance();
            break;
        }
        goto _error;
    }
    case Token::minus: {
        advance();
        if (Tok.getKind() == Token::number) {
            Res = new SignedNumber(SignedNumber::Minus, Tok.getText());
            advance();
            break;
        }
        goto _error;
    }
    case Token::minus_paren: {
        advance();
        Expr *math_expr = parseExpr();
        if(math_expr == nullptr)
            goto _error;
        Res = new NegExpr(math_expr);
        if (!consume(Token::r_paren))
            break;
        
        goto _error;
    }
    case Token::l_paren: {
        advance();
        Res = parseExpr();
        if(Res == nullptr) {
            goto _error;
        }
        if (!consume(Token::r_paren))
            break;
    }
    default: {
        goto _error;
    }
    }
    return Res;

_error:
    while (Tok.getKind() != Token::eoi)
        advance();
    return nullptr;
}

Expr *Parser::parseArrayAccess() {
    // llvm::errs() <<"first we getin accessarray  "<< Tok.getText()<< "\n";
    if (!Tok.is(Token::ident)) {
        // llvm::errs() << "didnt got the ident\n";
        return nullptr;
    }
    // llvm::errs() << "passed the firstt part\n";
    llvm::StringRef arrayName = Tok.getText();
    advance();
    
    if (!Tok.is(Token::l_bracket)) {
        // Not an array access, backtrac
        // llvm::errs() << "l_bra\n";
        return nullptr;
    }
    // llvm::errs() << "passed the 2 part\n";
    advance();
    
    Expr *index = parseExpr();
    // llvm::errs() << "passed the 3 part\n";
    if (!index) {
        // llvm::errs() << "expression \n";
        goto _error;
    }
    // llvm::errs() << "passed the 4 part\n";
    // llvm::errs() << Tok.getText()<< "\n";
    if (!Tok.is(Token::r_bracket)) {
        // llvm::errs() << "r_bra\n";
        goto _error;
    }
    advance();
    // llvm::errs() << "passed the 5 part\n";
    
    return new ArrayAccess(arrayName, index);

_error:
    while (Tok.getKind() != Token::eoi)
        advance();
    return nullptr;
}

Expr* Parser::parseLengthFunction() {
    // Check for length keyword
    if (!Tok.is(Token::KW_length)) {
        return nullptr;
    }
    advance();

    // Check for opening parenthesis
    if (!Tok.is(Token::l_paren)) {
        return nullptr;
    }
    advance();

    // Check for array identifier
    if (!Tok.is(Token::ident)) {
        return nullptr;
    }
    llvm::StringRef arrName = Tok.getText();
    advance();

    // Check for closing parenthesis
    if (!Tok.is(Token::r_paren)) {
        return nullptr;
    }
    advance();

    return new LengthFunction(arrName);
}

Expr* Parser::parseMinFunction() {
    if (expect(Token::KW_min)) return nullptr;
    advance();

    if (expect(Token::l_paren)) return nullptr;
    advance();

    if (expect(Token::ident)) return nullptr;
    llvm::StringRef arrName = Tok.getText();
    advance();

    if (expect(Token::r_paren)) return nullptr;
    advance();

    return new MinFunction(arrName);
}

Expr* Parser::parseMaxFunction() {
    if (expect(Token::KW_max)) return nullptr;
    advance();

    if (expect(Token::l_paren)) return nullptr;
    advance();

    if (expect(Token::ident)) return nullptr;
    llvm::StringRef arrName = Tok.getText();
    advance();

    if (expect(Token::r_paren)) return nullptr;
    advance();

    return new MaxFunction(arrName);
}

Expr* Parser::parseIndexFunction() {
    // Check for index keyword
    if (!Tok.is(Token::KW_index)) {
        return nullptr;
    }
    advance();

    // Check for opening parenthesis
    if (!Tok.is(Token::l_paren)) {
        return nullptr;
    }
    advance();

    // Check for array identifier
    if (!Tok.is(Token::ident)) {
        return nullptr;
    }
    llvm::StringRef arrName = Tok.getText();
    advance();

    // Check for comma
    if (!Tok.is(Token::comma)) {
        return nullptr;
    }
    advance();

    // Parse the index expression
    Expr* indexExpr = parseExpr();
    if (!indexExpr) {
        return nullptr;
    }

    // Check for closing parenthesis
    if (!Tok.is(Token::r_paren)) {
        return nullptr;
    }
    advance();

    // Create an ArrayAccess node using the same implementation as a[i]
    return new ArrayAccess(arrName, indexExpr);
}

Expr* Parser::parsePowFunction() {
    // Check for pow keyword
    if (!Tok.is(Token::KW_pow)) {
        return nullptr;
    }
    advance();

    // Check for opening parenthesis
    if (!Tok.is(Token::l_paren)) {
        return nullptr;
    }
    advance();

    // Parse the base expression
    Expr* baseExpr = parseExpr();
    if (!baseExpr) {
        return nullptr;
    }

    // Check for comma
    if (!Tok.is(Token::comma)) {
        return nullptr;
    }
    advance();

    // Parse the exponent expression
    Expr* expExpr = parseExpr();
    if (!expExpr) {
        return nullptr;
    }

    // Check for closing parenthesis
    if (!Tok.is(Token::r_paren)) {
        return nullptr;
    }
    advance();

    // Create a BinaryOp with Exp operator, reusing the existing ^ implementation
    return new BinaryOp(BinaryOp::Exp, baseExpr, expExpr);
}

// logic and comparisions
Logic *Parser::parseComparison()
{
    Logic *Res = nullptr;
    Final *Ident = nullptr;
    Expr *Left = nullptr;
    Expr *Right = nullptr;
    Token prev_Tok;
    const char* prev_buffer;

    if (Tok.is(Token::l_paren)) {
        advance();
        Res = parseLogic();
        if (Res == nullptr)
        {
            goto _error;
        }
        if (consume(Token::r_paren))
            goto _error;
    }
    else {
        if(Tok.is(Token::KW_true)){
            Res = new Comparison(nullptr, nullptr, Comparison::True);
            advance();
            return Res;
        }
        else if(Tok.is(Token::KW_false)){
            Res = new Comparison(nullptr, nullptr, Comparison::False);
            advance();
            return Res;
        }
        else if(Tok.is(Token::ident)){
            // Save the identifier
            Ident = new Final(Final::Ident, Tok.getText());
            advance();
            
            // Check if this is a boolean variable comparison
            if (Tok.isOneOf(Token::eq, Token::neq, Token::gt, Token::lt, Token::gte, Token::lte)) {
                Comparison::Operator Op;
                if (Tok.is(Token::eq))
                    Op = Comparison::Equal;
                else if (Tok.is(Token::neq))
                    Op = Comparison::Not_equal;
                else if (Tok.is(Token::gt))
                    Op = Comparison::Greater;
                else if (Tok.is(Token::lt))
                    Op = Comparison::Less;
                else if (Tok.is(Token::gte))
                    Op = Comparison::Greater_equal;
                else if (Tok.is(Token::lte))
                    Op = Comparison::Less_equal;
                advance();
                
                Right = parseExpr();
                if (Right == nullptr) {
                    goto _error;
                }
                Res = new Comparison(Ident, Right, Op);
            } else {
                // If no comparison operator, treat as boolean variable
                Res = new Comparison(Ident, nullptr, Comparison::Ident);
            }
            return Res;
        }
        
        // Try to parse a regular comparison
        prev_Tok = Tok;
        prev_buffer = Lex.getBuffer();
        Left = parseExpr();
        if(Left == nullptr)
            goto _error;
        
        Comparison::Operator Op;
        if (Tok.is(Token::eq))
            Op = Comparison::Equal;
        else if (Tok.is(Token::neq))
            Op = Comparison::Not_equal;
        else if (Tok.is(Token::gt))
            Op = Comparison::Greater;
        else if (Tok.is(Token::lt))
            Op = Comparison::Less;
        else if (Tok.is(Token::gte))
            Op = Comparison::Greater_equal;
        else if (Tok.is(Token::lte))
            Op = Comparison::Less_equal;    
        else {
            goto _error;
        }
        advance();
        Right = parseExpr();
        if (Right == nullptr)
        {
            goto _error;
        }
        
        Res = new Comparison(Left, Right, Op);
    }
    
    return Res;

_error:
    while (Tok.getKind() != Token::eoi)
        advance();
    return nullptr;
}

Logic *Parser::parseLogic()
{
    Logic *Left = parseComparison();
    Logic *Right;
    if (Left == nullptr)
    {
        goto _error;
    }
    while (Tok.isOneOf(Token::KW_and, Token::KW_or))
    {
        LogicalExpr::Operator Op;
        if (Tok.is(Token::KW_and))
            Op = LogicalExpr::And;
        else if (Tok.is(Token::KW_or))
            Op = LogicalExpr::Or;
        else {
            error();

            goto _error;
        }
        advance();
        Right = parseComparison();
        if (Right == nullptr)
        {
            goto _error;
        }
        Left = new LogicalExpr(Left, Right, Op);
    }
    return Left;

_error:
    while (Tok.getKind() != Token::eoi)
        advance();
    return nullptr;
}

// if,print,while,for,comments
IfStmt *Parser::parseIf()
{
    llvm::SmallVector<AST *> ifStmts;
    llvm::SmallVector<AST *> elseStmts;
    llvm::SmallVector<elifStmt *> elifStmts;
    llvm::SmallVector<AST *> Stmts;
    Logic *Cond = nullptr;
    Token prev_token_if;
    const char* prev_buffer_if;
    Token prev_token_elif;
    const char* prev_buffer_elif;
    bool hasElif = false;
    bool hasElse = false;


    if (expect(Token::KW_if)){
        goto _error;
    }

    advance();

    if (expect(Token::l_paren)){
        goto _error;
    }

    advance();

    Cond = parseLogic();
    if (Cond == nullptr)
    {
        goto _error;
    }

    if (expect(Token::r_paren)){
        goto _error;
    }
        
    advance();

    if (expect(Token::l_brace)){
        goto _error;
    }

    advance();
    
    ifStmts = getBody();
        
    if(ifStmts.empty())
        goto _error;
    
    prev_token_if = Tok;
    prev_buffer_if = Lex.getBuffer();
    
    advance();

    while (true)
    {
        if (Tok.is(Token::KW_else))
        {
            advance();
            if (Tok.is(Token::KW_if))
            {
                hasElif = true;
                advance();
                
                if (expect(Token::l_paren)){
                    goto _error;
                }

                advance();

                Logic *Cond = parseLogic();

                if (Cond == nullptr)
                {
                    goto _error;
                }

                if (expect(Token::r_paren)){
                    goto _error;
                }

                if (expect(Token::l_brace)){
                    goto _error;
                }

                advance();

                Stmts = getBody();
                prev_token_elif = Tok;
                prev_buffer_elif = Lex.getBuffer();
                
                if(!Stmts.empty())
                    advance();
                else
                    goto _error;
                
                elifStmt *elif = new elifStmt(Cond, Stmts);
                elifStmts.push_back(elif);
            }
            else
            {
                hasElse = true;

                if (expect(Token::l_brace)){
                    goto _error;
                }

                advance();

                elseStmts = getBody();
                
                if(elseStmts.empty())
                    goto _error;

                break;
            }
        }
        else
            break;
    }

    if(hasElif && !hasElse){
        Tok = prev_token_elif;
        Lex.setBufferPtr(prev_buffer_elif);
    }
    else if(!hasElif && !hasElse){
        Tok = prev_token_if;
        Lex.setBufferPtr(prev_buffer_if);
    }
        
    return new IfStmt(Cond, ifStmts, elseStmts, elifStmts);

_error:
    while (Tok.getKind() != Token::eoi)
        advance();
    return nullptr;
}

PrintStmt *Parser::parsePrint()
{
    llvm::StringRef Var;
    if (expect(Token::KW_print)){
        goto _error;
    }
    advance();
    if (expect(Token::l_paren)){
        goto _error;
    }
    advance();
    if (expect(Token::ident)){
        goto _error;
    }
    Var = Tok.getText();
    advance();
    if (expect(Token::r_paren)){
        goto _error;
    }
    advance();
    if (expect(Token::semicolon)){
        goto _error;
    }
    return new PrintStmt(Var);

_error:
    while (Tok.getKind() != Token::eoi)
        advance();
    return nullptr;

}

WhileStmt *Parser::parseWhile()
{
    llvm::SmallVector<AST *> Body;
    Logic *Cond = nullptr;

    if (expect(Token::KW_while)){
        goto _error;
    }
        
    advance();

    if(expect(Token::l_paren)){
        goto _error;
    }

    advance();

    Cond = parseLogic();
    if (Cond == nullptr)
    {
        goto _error;
    }
    if(expect(Token::r_paren)){
        goto _error;
    }

    advance();

    if (expect(Token::l_brace)){
        goto _error;
    }

    advance();

    Body = getBody();
    if(Body.empty())
        goto _error;
        

    return new WhileStmt(Cond, Body);

_error:
    while (Tok.getKind() != Token::eoi)
        advance();
    return nullptr;
}

ForStmt *Parser::parseFor()
{
    Assignment *First = nullptr;
    Logic *Second = nullptr;
    Assignment *ThirdAssign = nullptr;
    UnaryOp *ThirdUnary = nullptr;
    llvm::SmallVector<AST *> Body;
    Token prev_token;
    const char* prev_buffer;

    if (expect(Token::KW_for)) {
        goto _error;
    }
    advance();

    if (expect(Token::l_paren)) {
        goto _error;
    }
    advance();

    // Parse the initializer as a declaration (e.g., int i = 0)
    if (Tok.is(Token::KW_int)) {
        DeclarationInt *decl = parseIntDec();
        if (!decl || decl->varBegin() == decl->varEnd()) {
            goto _error; // Invalid declaration or no variables
        }

        // Extract the first declared variable and its initial value
        llvm::StringRef varName = *decl->varBegin();
        Expr *initValue = *decl->valBegin();

        // Create an Assignment node from the declaration
        Final *F = new Final(Final::Ident, varName);
        First = new Assignment(F, initValue, Assignment::Assign, nullptr);
    } else {
        goto _error; // Only int declarations supported in initializer for this example
    }

    if (expect(Token::semicolon)) {
        goto _error;
    }
    advance();

    // Parse condition
    Second = parseLogic();
    if (!Second) {
        goto _error;
    }

    if (expect(Token::semicolon)) {
        goto _error;
    }
    advance();

    // Parse increment part
    prev_token = Tok;
    prev_buffer = Lex.getBuffer();
    ThirdAssign = parseIntAssign();

    if (!ThirdAssign) {
        Tok = prev_token;
        Lex.setBufferPtr(prev_buffer);
        ThirdUnary = parseUnary();
        if (!ThirdUnary) {
            goto _error;
        }
    } else {
        if (ThirdAssign->getAssignKind() == Assignment::Assign) {
            goto _error; // Third part cannot be simple assignment
        }
    }

    if (expect(Token::r_paren)) {
        goto _error;
    }
    advance();

    if (expect(Token::l_brace)) {
        goto _error;
    }
    advance();

    Body = getBody();
    if (Body.empty()) {
        goto _error;
    }

    return new ForStmt(First, Second, ThirdAssign, ThirdUnary, Body);

_error:
    while (Tok.getKind() != Token::eoi)
        advance();
    return nullptr;
}

void Parser::parseComment()
{
    if (expect(Token::start_comment)) {
        goto _error;
    }
    advance();

    while (!Tok.isOneOf(Token::end_comment, Token::eoi)) advance();

    return;
_error: 
    while (Tok.getKind() != Token::eoi)
        advance();
}

// the other parts of the codes
llvm::SmallVector<AST *> Parser::getBody()
{
    llvm::SmallVector<AST *> body;
    while (!Tok.is(Token::r_brace))
    {
        switch (Tok.getKind())
        {
        case Token::KW_int: {
            DeclarationInt *d;
            d = parseIntDec();
            if (d)
                body.push_back(d);
            else
                goto _error;
            break;
        }
        case Token::KW_float: {
            DeclarationFloat *d;
            d = parseFloatDec();
            if (d)
                body.push_back(d);
            else
                goto _error;
            break;
        }
        case Token::KW_char: {
            DeclarationChar *d;
            d = parseCharDec();
            if (d)
                body.push_back(d);
            else
                goto _error;
            break;
        }
        case Token::KW_string: {
            DeclarationString *d;
            d = parseStringDec();
            if (d)
                body.push_back(d);
            else
                goto _error;
            break;
        }
        case Token::KW_boolean: {
            DeclarationBool *d;
            d = parseBoolDec();
            if (d)
                body.push_back(d);
            else
                goto _error;
            break;
        }
        case Token::KW_array: {
            DeclarationArray *d;
            d = parseArrayDec();
            if (d)
                body.push_back(d);
            else
                goto _error;
            break;
        }
        case Token::ident:{
            Token prev_token = Tok;
            const char* prev_buffer = Lex.getBuffer();
            UnaryOp *u;
            u = parseUnary();
            if (Tok.is(Token::semicolon))
            {
                if (u)
                {
                    body.push_back(u);
                    break;
                }
                else{
                    goto _error;
                }
            }
            else
            {
                if (u)
                {
                    goto _error;
                }
                else{
                    Tok = prev_token;
                    Lex.setBufferPtr(prev_buffer);
                }
            }

            
            Assignment *a_int;
            Assignment *a_bool;
            Assignment *a_float;
            Assignment *a_char;
            Assignment *a_string;
            prev_token = Tok;
            prev_buffer = Lex.getBuffer();

            a_bool = parseBoolAssign();

            if (a_bool){
                body.push_back(a_bool);
                break;
            }
            Tok = prev_token;
            Lex.setBufferPtr(prev_buffer);

            a_int = parseIntAssign();
            if (a_int)
                body.push_back(a_int);
                
            Tok = prev_token;
            Lex.setBufferPtr(prev_buffer);

            a_float = parseFloatAssign();
            if (a_float)
                body.push_back(a_float);
                
            Tok = prev_token;
            Lex.setBufferPtr(prev_buffer);

            a_char = parseCharAssign();
            if (a_char)
                body.push_back(a_char);
                
            Tok = prev_token;
            Lex.setBufferPtr(prev_buffer);

            a_string = parseStringAssign();
            if (a_string)
                body.push_back(a_string);
            else
                goto _error;
            if (!Tok.is(Token::semicolon))
            {
                goto _error;
            }

            break;
        }
        case Token::KW_if: {
            IfStmt *i;
            i = parseIf();
            if (i)
                body.push_back(i);
            else
                goto _error;
            
            break;
        }
        case Token::KW_while:{
            WhileStmt *w;
            w = parseWhile();
            if (w)
                body.push_back(w);
            else {
                goto _error;
            }
            break;
        }
        case Token::KW_for:{
            ForStmt *f;
            f = parseFor();
            if (f)
                body.push_back(f);
            else {
                goto _error;
            }
            break;
        }
        case Token::KW_print: {
            PrintStmt *p;
            p = parsePrint();
            if (p)
                body.push_back(p);
            else {
                goto _error;
            }
            break;
        }
        case Token::start_comment: {
            parseComment();
            if (!Tok.is(Token::end_comment))
                goto _error;
            break;
        }
        default:{
            error();
            goto _error;
            break;
        }
        }
        advance();
    }
    if(Tok.is(Token::r_brace)){
        return body;
    }

_error:
    while (Tok.getKind() != Token::eoi)
        advance();
    return body;
}
