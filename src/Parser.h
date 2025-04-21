#ifndef PARSER_H
#define PARSER_H

#include "AST.h"
#include "Lexer.h"
#include "llvm/Support/raw_ostream.h"

class Parser
{
    Lexer &Lex;    // retrieve the next token from the input
    Token Tok;     // stores the next token
    bool HasError; // indicates if an error was detected

    void error()
    {
        llvm::errs() << "Unexpected: " << Tok.getText() <<" "<< Tok.getKind() << "\n";
        HasError = true;
    }

    // retrieves the next token from the lexer.expect()
    // tests whether the look-ahead is of the expected kind
    void advance() { Lex.next(Tok);
     llvm::errs() << Tok.getKind() + 16 << " " << Tok.getText() << '\n';
    }
    

    bool expect(Token::TokenKind Kind)
    {
        if (Tok.getKind() != Kind)
        {
            //error();
            return true;
        }
        return false;
    }

    // retrieves the next token if the look-ahead is of the expected kind
    bool consume(Token::TokenKind Kind)
    {
        if (expect(Kind))
            return true;
        advance();
        return false;
    }

    Program *parseProgram();
    // declarations
    DeclarationInt *parseIntDec();
    DeclarationFloat *parseFloatDec();
    DeclarationChar *parseCharDec();
    DeclarationBool *parseBoolDec();
    DeclarationString *parseStringDec();
    DeclarationArray* parseArrayDec();
    llvm::SmallVector<Expr*> parseArrayElements();
    // assignments
    Assignment *parseBoolAssign();
    Assignment *parseIntAssign();
    Assignment *parseCharAssign();
    Assignment *parseFloatAssign();
    Assignment *parseStringAssign();
    
    // unary
    UnaryOp *parseUnary();
    // expressions
    Expr *parseExpr();
    Expr *parseTerm();
    Expr *parseFinal();
    Expr *parseFactor();
    Expr *parseArrayAccess();
    Expr *parseLengthFunction();
    Expr *parseMinFunction();

    // logic and comparision
    Logic *parseLogic();
    Logic *parseComparison();
    // if,else,for,print,comment expression
    IfStmt *parseIf();
    WhileStmt *parseWhile();
    ForStmt *parseFor();
    PrintStmt *parsePrint();
    void parseComment();
    // the body part for other results
    llvm::SmallVector<AST *> getBody();

public:
    // initializes all members and retrieves the first token
    Parser(Lexer &Lex) : Lex(Lex), HasError(false)
    {
        advance();
    }

    // get the value of error flag
    bool hasError() { return HasError; }

    Program *parse();
};

#endif