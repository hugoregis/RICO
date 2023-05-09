package kbingest.token;

/**
 * A Token model with a type and lexeme, as produced by a Scanner.
 * 
 */
public class Token {
    /**
     * This token's type (see TokenType)
     */
    public TokenType type;
    
    /**
     * This token's lexeme
     */
    public String lexeme;
    
    /**
     * Instantiate this token with the given lexeme and type
     * 
     * @param lexeme the lexeme for this token
     * @param type  the lexeme's type
     */
    public Token (String lexeme, TokenType type) {
        this.lexeme = lexeme;
        this.type = type;
    }
}
