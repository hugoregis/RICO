package kbingest.token;

/**
 * Defines the Token types allowed in Scott's (2016) Simple Calculator
 * LL(1) grammar (with minor extensions).
 * 
 */
public enum TokenType {
    NECK,        // Lexeme: ":="
    MODULE,
    MULTI,
    LPAREN,
    RPAREN,
    LBRACK,
    RBRACK,
    ARITY1,
    ARITY2,
    COMA,
    DOT,
    ID,
    FUNCTOR,
    ERROR,
    EOF;
}