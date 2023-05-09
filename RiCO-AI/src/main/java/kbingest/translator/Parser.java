package kbingest.translator;

import kbingest.token.Token;
import kbingest.token.TokenType;

import java.io.*;
import java.util.ArrayList;

public class Parser {
	
	/**
     * Maximum number of characters allowed in one source file line.
     */
    private static final int MAX_LINE_SIZE = 10000;
    
	/**
     * The input source program's file path 
     */
    private final String sourceFile;
    
    /**
     * Used to read the source file a char at a time (with peek ahead).
     */
    private final PushbackReader buffer;

    /**
     * The current lexeme being read from the source file buffer.
     * (from 0 to endPos)
     */
    private final char[] lexeme;

    /**
     * Current position of the last character in the lexeme being read.
     */
    private int endPos = 0;
    
    /**
     * True, in the middle of scanning a lexeme waiting for a delimiter.
     */
    private boolean isLexeme;
    
    /**
     * The current line number being read in the input source file.
     */
    private int lineNo = 0;
    
	public Parser(String filePath) throws IOException {
	  sourceFile = filePath;
	  
	  File file = new File(filePath);
	  
	  lexeme = new char[MAX_LINE_SIZE];
      
      buffer = new PushbackReader(new BufferedReader(new FileReader(filePath)));
	}
	
	
	/**
     * Read and return the next character in the source file.
     * 
     * @return the next char or '\0', if EOR 
     */
    private char nextChar() throws IOException {
        int ch = buffer.read();
        
        switch (ch) {
            case '\n':            // ASCII Line feed, LF or
            case '\r':            //  carriage return, CR
                if (ch == '\r') {
                    nextChar();
                } else {
                    lineNo++;
                }
                
                return (char) ch;
                
            case ' ':                // space
            case 255:                // non-breaking space
                return (char) ch;
                        
            case -1:                 // Java read nothing, so
                return '\0';         // we're at EOF
              
            default:
                lexeme[endPos++] = (char) ch;
                return (char) ch;
        } 
    }
    
    /**
     * Return the next Token in the input source file
     * 
     * @return a Token with TokenType and Lexeme
     * @throws IOException an unexpected non-recoverable error occurred
     */
    public Token next() throws IOException {
        isLexeme = false;
        
        while (true) {
            
            char ch = nextChar();
               
            
            switch (ch) {
                case '\0':
                    if (isLexeme)
                        return scanLexeme();
                    else
                        return new Token("", TokenType.EOF);
                        
                case '\n':
                case '\r':                        
                case ' ':
                case 255:      // non-breaking space
                case '\t':
                    if (isLexeme)
                        return scanLexeme();
                    else 
                        return next();
                
                case '\'':
                    return scanQuotedAtom(ch);
                
                case ':':
                    return scanColon(ch);
                case '/':
                    return scanArity(ch);
                
                    
                case '(': 
                case ')': 
                case '[': 
                case ']':
                case ',':
                case '.':
                    return scanSingleCharToken(ch);
                
                default:
                    isLexeme = true;
            }
        }
    }
    
    /**
     * Return the current line number being read in the source file. 
     * 
     * @return the current line number
     */
    public int getLineNo() {
        return lineNo;
    }
    
    /**
     * As a delimiter was encountered during scanning, determine
     * and return the Token for the current lexeme.
     * 
     * @return a Token encapsulating the current lexeme 
     */
    private Token scanLexeme() {
        String lexemeStr = String.copyValueOf(lexeme, 0, endPos);
        
        endPos = 0;
        isLexeme = false;
        
        if (lexemeStr.equals("module")) {
            return new Token(lexemeStr, TokenType.MODULE);
           
        } else if (lexemeStr.equals("multifile")) {
            return new Token(lexemeStr, TokenType.MULTI);       
        
        } else { // it's an ID
            return new Token(lexemeStr, TokenType.ID);
        }
     }
    
    /**
     * If we're not in the middle of reading another token, return
     * a token for the given single character, otherwise return the
     * token we're in the middle of reading and push the character
     * back into the input buffer.
     * 
     * @param ch a single character token (see TokenType)
     */
    private Token scanSingleCharToken(char ch) throws IOException {
        if (isLexeme) {              // Middle of reading another token
            buffer.unread((int) ch);
            endPos--;
            return scanLexeme();
                    
        } else {
            endPos = 0;
            
            switch ((int) ch) {
                case '(':  
                    return new Token(String.valueOf(ch), TokenType.LPAREN);
                    
                case ')':
                    return new Token(String.valueOf(ch), TokenType.RPAREN);
                    
                case '[':
                    return new Token(String.valueOf(ch), TokenType.LBRACK);
                
                case ']':
                    return new Token(String.valueOf(ch), TokenType.RBRACK);
                
                case ',':
                    return new Token(String.valueOf(ch), TokenType.COMA);
                
                case '.':
                    return new Token(String.valueOf(ch), TokenType.DOT);
                
                            
          
                 default:
                    return new Token(String.valueOf(ch), TokenType.ERROR);
            }
        }
    }
    
    
    private Token scanQuotedAtom(char ch) throws IOException {
    	if (isLexeme) {
        	buffer.unread((int) ch);
            endPos--;
            return scanLexeme();
        } else while (true) {
        	if (nextChar() == '\'') {
        		return scanLexeme();
        	}
        }
    }
    
    
    /**
     * As a single colon has been read, if were within a lexeme, treat it as
     * a delimiter, otherwise check for an assignment statement or error.
     * 
     * @return token that is an ID, ASSIGN, or ERROR
     * @throws IOException an unexpected non-recoverable error occurred
     */
    private Token scanColon(char ch) throws IOException {
        if (isLexeme) {
        	buffer.unread((int) ch);
            endPos--;
            return scanLexeme();
        } else if (nextChar() == '-') {
            endPos = 0;
            return new Token(":-", TokenType.NECK);
        } else {
            endPos = 0;
            return new Token(":", TokenType.ERROR);
        }
    }
    
    private Token scanArity(char ch) throws IOException {
        if (isLexeme) {
        	buffer.unread((int) ch);
            endPos--;
            return scanLexeme();
        } else {
        	char nextCh = nextChar(); 
        	if (nextCh == '1') {
        		endPos = 0;
                return new Token("/1", TokenType.ARITY1);
            } else if (nextCh == '2') {
                endPos = 0;
                return new Token("/2", TokenType.ARITY2);
            } else {
                endPos = 0;
                return new Token("/", TokenType.ERROR);
            }
        }
            
    }
    
    /** 
     * A debugging utility for batch scanning the entire source file
     * 
     * @return a list of tokens in the source file
     * @throws IOException an unexpected non-recoverable error occurred
     */
    public ArrayList<Token> scanAll() throws IOException {
        ArrayList<Token> tokens = new ArrayList<>();
        
        Token token;
            
        do {
            token = next();
            tokens.add(token);
                
        } while (token.type != TokenType.EOF);
        
        return tokens;
    }
    
}
