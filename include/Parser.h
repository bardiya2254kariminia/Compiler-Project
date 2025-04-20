#ifndef PARSER_H
#define PARSER_H

#include "AST.h"
#include "Lexer.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/StringRef.h"

class Parser {
    Lexer &Lex;
    Token Tok;
    bool HasError;

    void advance() { Lex.next(Tok); }
    bool expect(Token::TokenKind Kind);
    bool consume(Token::TokenKind Kind);
    void error();

public:
    Parser(Lexer &Lex) : Lex(Lex), HasError(false) { advance(); }
    bool hasError() { return HasError; }

    Program *parse();
    Program *parseProgram();
    DeclarationArray *parseArrayDec();
    Expr *parseArrayAccess();
    llvm::SmallVector<Expr *> parseArrayElements();
    // ... rest of the existing declarations ...
};

#endif // PARSER_H 