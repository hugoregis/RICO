package kbingest.parser;

import kbingest.token.TokenType;

public class ParseError extends Exception {
    public ParseError(TokenType type, int lineNo) {
        this("Expecting " + type + " at " + lineNo);
    }
    
    public ParseError(String msg) {
        super(msg);
    }
}
