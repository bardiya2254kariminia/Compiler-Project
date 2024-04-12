#include "Parser.h"

bool haveElse;

// main point is that the whole input has been consumed
Program *Parser::parse()
{
    Program *Res = parseProgram();
    return Res;
}

Program *Parser::parseProgram()
{
    llvm::SmallVector<AST *> data;
    
    while (!Tok.is(Token::eoi))
    {
        haveElse = true;
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
        case Token::KW_bool: {
            DeclarationBool *dbool;
            dbool = parseBoolDec();
            if (dbool)
                data.push_back(dbool);
            else
                goto _error;

            break;
        }
        case Token::ident: {
            Token prev_token = Tok;
            UnaryOp *u;
            u = parseUnary();
            if (Tok.is(Token::semicolon))
            {
                if (u)
                {
                    data.push_back(u);
                    break;
                }
                else
                    Tok = prev_token;
            }
            else
            {
                if (u)
                {
                    error();
                    goto _error;
                }
                else{
                    Tok = prev_token;
                }
            }
        
            
            Assignment *a;
            a = parseAssign();
            if (!Tok.is(Token::semicolon))
            {
                goto _error;
            }

            if (a)
                data.push_back(a);
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
        default: {
            error();
            goto _error;
            break;
        }
        }
        if(haveElse){
            advance();
        }
    }
    return new Program(data);
_error:
    while (Tok.getKind() != Token::eoi)
        advance();
    return nullptr;
}

DeclarationInt *Parser::parseIntDec()
{
    Expr *E;
    llvm::SmallVector<llvm::StringRef> Vars;
    llvm::SmallVector<Expr *> Values;
    int count = 1;
    
    if (expect(Token::KW_int)){
        goto _error;
    }
    advance();
    
    if (expect(Token::ident)){
        goto _error;
    }

    Vars.push_back(Tok.getText());
    advance();
    
    
    while (Tok.is(Token::comma))
    {
        advance();
        if (expect(Token::ident)){
            goto _error;
        }
            
        Vars.push_back(Tok.getText());
        count++;
        advance();
    }

    if (Tok.is(Token::assign))
    {
        advance();
        E = parseExpr();
        if(E){
            Values.push_back(E);
            count--; 
        }
        else{
            goto _error;
        }
        
        while (Tok.is(Token::comma))
        {   
            if (count == 0){
                error();
                goto _error;
            }

            advance();
            E = parseExpr();
            if(E){
                Values.push_back(E);
                count--; 
            }
            else{
                goto _error;
            }
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


DeclarationBool *Parser::parseBoolDec()
{
    Logic *L;
    llvm::SmallVector<llvm::StringRef> Vars;
    llvm::SmallVector<Logic *> Values;
    int count = 1;
    
    if (expect(Token::KW_bool)){
        goto _error;
    }

    advance();
    
    if (expect(Token::ident)){
        goto _error;
    }

    Vars.push_back(Tok.getText());
    advance();

    
    while (Tok.is(Token::comma))
    {
        advance();
        if (expect(Token::ident)){
            goto _error;
        }
            
        Vars.push_back(Tok.getText());
        count++;
        advance();
    }

    if (Tok.is(Token::assign))
    {
        advance();
        L = parseLogic();
        if(L){
            Values.push_back(L);
            count--; 
        }
        else{
            goto _error;
        }
        
        while (Tok.is(Token::comma))
        {   
            if (count == 0){
                error();
                goto _error;
            }

            advance();
            L = parseLogic();
            if(L){
                Values.push_back(L);
                count--; 
            }
            else{
                goto _error;
            }
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



Assignment *Parser::parseAssign()
{
    Expr *E;
    Final *F;
    Assignment::AssignKind AK;
    Logic *L;
    Token prev_Tok;

    F = (Final *)(parseFinal());
    if (F == nullptr)
    {
        goto _error;
    }
    prev_Tok = Tok;
    
    if (Tok.is(Token::assign))
    {
        AK = Assignment::Assign;
        advance();
        L = parseLogic();   // check if expr is logical
        if(L)
        {
            return new Assignment(F, nullptr, AK, L);
        }
        else
        {
            Tok = prev_Tok;
        }
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
        error();
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

Expr *Parser::parseFactor()
{
    Expr *Left = parseFinal();
    if (Left == nullptr)
    {
        goto _error;
    }
    while (Tok.is(Token::exp))
    {
        BinaryOp::Operator Op;
        if (Tok.is(Token::exp))
            Op = BinaryOp::Exp;
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

Expr *Parser::parseFinal()
{
    Expr *Res = nullptr;
    switch (Tok.getKind())
    {
    case Token::number:{
        Res = new Final(Final::Number, Tok.getText());
        advance();
    }
    case Token::ident: {
        Res = new Final(Final::Ident, Tok.getText());
        Token prev_tok = Tok;
        Expr* u = parseUnary();
        if(u)
            Res = u;
        else
            Tok = prev_tok;
        advance();
        return Res;
    }
    case Token::plus:{
        advance();
        if(Tok.getKind() == Token::number){
            Res = new SignedNumber(SignedNumber::Plus, Tok.getText());
            advance();
            break;
        }
        goto _error;
    }

    case Token::minus:{
        advance();
        if (Tok.getKind() == Token::number){
            Res = new SignedNumber(SignedNumber::Minus, Tok.getText());
            advance();
            break;
        }
        else if(Tok.getKind() == Token::l_paren){
            advance();
            Expr *math_expr = parseExpr();
            if(math_expr == nullptr)
                goto _error;
            Res = new NegExpr(math_expr);
            if (!consume(Token::r_paren))
                break;
        }
        goto _error;
    }
    case Token::l_paren:{
        advance();
        Res = parseExpr();
        if(Res == nullptr){
            goto _error;
        }
        if (!consume(Token::r_paren))
            break;
        
        goto _error;        //CHECK??
    }
    default:{
        error();
        goto _error;
    }
    }
    return Res;

_error:
    while (Tok.getKind() != Token::eoi)
        advance();
    return nullptr;
}

Logic *Parser::parseComparison()
{
    Logic *Res = nullptr;
    Expr *Ident;
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
            Ident = parseFinal();
            if(Ident == nullptr){
                goto _error;
            }
        }
        Expr *Left = parseExpr();
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
                    if(Ident){
                        Res = new Comparison(Ident, nullptr, Comparison::Ident);
                        advance();
                        return Res;
                    }
                    error();
                    goto _error;
                }
            advance();
            Expr *Right = parseExpr();
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
        Logic *Right = parseComparison();
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

IfStmt *Parser::parseIf()
{
    llvm::SmallVector<AST *> ifStmts;
    llvm::SmallVector<AST *> elseStmts;
    llvm::SmallVector<elifStmt *> elifStmts;
    Logic *Cond;

    // haveElse = false;

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
        
    if(!ifStmts.empty())
        advance();
    else
        goto _error;
    

    while (Tok.is(Token::KW_elseif)) {

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

        advance();

        if (expect(Token::l_brace)){
            goto _error;
        }

        advance();

        llvm::SmallVector<AST *> Stmts = getBody();
        
        if(!Stmts.empty())
            advance();
        else
            goto _error;
        
        elifStmt *elif = new elifStmt(Cond, Stmts);
        elifStmts.push_back(elif);
        advance();
    }

    if (Tok.is(Token::KW_else))
    {
        // haveElse = true;

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

        elseStmts = getBody();
        
        if(!elseStmts.empty())
            advance();
        else
            goto _error;

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
    if (expect(Token::l_paren)){
        goto _error;
    }
    advance();
    if (expect(Token::semicolon)){
        goto _error;
    }
    advance();
    return new PrintStmt(Var);

_error:
    while (Tok.getKind() != Token::eoi)
        advance();
    return nullptr;

}

WhileStmt *Parser::parseWhile()
{
    llvm::SmallVector<AST *> Body;
    Logic *Cond;

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
    if(!Body.empty())
        advance();
    else
        goto _error;
        

    return new WhileStmt(Cond, Body);

_error:
    while (Tok.getKind() != Token::eoi)
        advance();
    return nullptr;
}

ForStmt *Parser::parseFor()
{
    Assignment *First;
    Comparison *Second;
    Assignment *ThirdAssign;
    UnaryOp *ThirdUnary;
    llvm::SmallVector<AST *> Body;
    Token prev_token;

    if (expect(Token::KW_for)){
        goto _error;
    }
        
    advance();

    if(expect(Token::l_paren)){
        goto _error;
    }

    advance();

    First = parseAssign();

    if (First == nullptr)
        goto _error;
        
    if (First->getAssignKind() != Assignment::Assign)    // The first part can only have a '=' sign
        goto _error;

    if(expect(Token::semicolon)){
        goto _error;
    }

    advance();

    Second = (Comparison*)parseComparison();

    if (Second == nullptr)
        goto _error;
        
    if(expect(Token::semicolon)){
        goto _error;
    }

    advance();

    prev_token = Tok;

    ThirdAssign = parseAssign();

    if (ThirdAssign == nullptr){
        Tok = prev_token;
        ThirdUnary = parseUnary();
        if (ThirdUnary == nullptr){
            goto _error;
        }

    }
    else{
        if(ThirdAssign->getAssignKind() == Assignment::Assign)   // The third part cannot have only '=' sign
            goto _error;
    }


    if(expect(Token::r_paren)){
        goto _error;
    }

    advance();

    if(expect(Token::l_brace)){
        goto _error;
    }

    advance();

    Body = getBody();

    if (!Body.empty())
        advance();
    else
        goto _error;

    return new ForStmt(First, Second, ThirdAssign, ThirdUnary, Body);

_error:
    while (Tok.getKind() != Token::eoi)
        advance();
    return nullptr;  

}

llvm::SmallVector<AST *> Parser::getBody()
{
    llvm::SmallVector<AST *> body;
    haveElse = true;
    while (!Tok.is(Token::r_brace))
    {
        switch (Tok.getKind())
        {
        
        case Token::ident:{
            Token prev_token = Tok;
            UnaryOp *u;
            u = parseUnary();
            if (Tok.is(Token::semicolon))
            {
                if (u)
                {
                    body.push_back(u);
                    break;
                }
                else
                    Tok = prev_token;
            }
            else
            {
                if (u)
                {
                    error();
                    goto _error;
                }
                else
                    Tok = prev_token;
            }

            
            Assignment *a;
            a = parseAssign();
            if (!Tok.is(Token::semicolon))
            {
                error();
                goto _error;
            }

            if (a)
                body.push_back(a);
            else
                goto _error;

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
        default:{
            error();
            goto _error;
            break;
        }
        }
        if(haveElse){
            advance();
        }
    }
    return body;

_error:
    while (Tok.getKind() != Token::eoi)
        advance();
    return body;

}
