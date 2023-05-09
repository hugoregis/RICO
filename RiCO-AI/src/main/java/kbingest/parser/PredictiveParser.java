package kbingest.parser;

import kbingest.token.Token;
import kbingest.token.TokenType;


import java.io.IOException;
import java.util.*;

import kbingest.translator.Functor;
import kbingest.translator.Parser;
import kbingest.translator.PlModule;


/**
 * A top-down Predictive Parser 
 * LL(1) grammar.
 * 
 */
public class PredictiveParser {
    /**
     * The most recently scanned Token.
     */
    private Token currentToken;
    
    private ArrayList<Functor> componentListContent;
    
    
    /**
     * The lexical analysis Scanner used by this Parser
     */
    private final Parser scanner;
    
    private TreeNode parseTree;
    
    private PlModule module;
    /**
    
     * Initialize this Parser with the given Scanner
     * 
     * @param scanner lexical analysis Scanner that returns Tokens. 
     */
    public PredictiveParser(Parser scanner) {
        this.scanner = scanner;
    }
    
    public PlModule parseToModule() throws ParseError, IOException {
    	this.module = new PlModule();
        currentToken = scanner.next();
        
        
        kb();
        
        return this.module;
    }
    
    /**
     * Entry point that begins a parse of the input source file
     * beginning with the Program start non-terminal
     * 
     * @throws ParseError an unexpected token or IO error occurred.
     * @throws IOException an unexpected non-recoverable error occurred
     */
    public TreeNode parse() throws ParseError, IOException {
        currentToken = scanner.next();
        
        
        kb();
        
        return parseTree;
    }
    
    /**
     * Parses the production: program -> stmt_list $$
     * 
     * @throws ParseError the expected token wasn't found
     * @throws IOException an unexpected non-recoverable error occurred
     */
    private void kb() throws ParseError, IOException {
        switch(currentToken.type) {
            case NECK:
            case EOF:
                parseTree = new TreeNode(TreeNode.PRODUCTION.KB, null);
               
                kbClauses(parseTree);
                match(TokenType.EOF, parseTree);
               
                System.out.println("Parse Sucessful");
                break;
            
            default:
                throw new ParseError("program() unmatched token: " +
                                      currentToken.type);
        }
    }
    
    /**
     * Parses the production: stmtList -> stm stmtList | null
     * 
     * @throws ParseError the expected token wasn't found
     * @throws IOException an unexpected non-recoverable error occurred
     */
    private void kbClauses(TreeNode parent) throws ParseError, IOException {
        TreeNode node = new TreeNode(TreeNode.PRODUCTION.KB_CLAUSES, parent);
        
        switch (currentToken.type) {
            case NECK:
                moduleDef(node);
//                multifileList(node);
                factList(node);
                break;
                
            case EOF:
                node.add(new TreeNode(TreeNode.PRODUCTION.EMPTY, null));
                break;
                
            default:
                throw new ParseError("stmtList() unmatched token: " +
                                      currentToken.type);
        }
    }
    
    private void moduleDef(TreeNode parent) throws ParseError, IOException {
        TreeNode node = new TreeNode(TreeNode.PRODUCTION.MODULE_DEF, parent);
        
        switch (currentToken.type) {
            case NECK:
                match(TokenType.NECK, node);
                match(TokenType.MODULE, node);
                match(TokenType.LPAREN, node);
                this.module.setName(currentToken.lexeme);
                match(TokenType.ID, node);
                match(TokenType.COMA, node);
                exportList(node);
                match(TokenType.RPAREN, node);
                match(TokenType.DOT, node);
                break;
                
            
            default:
                throw new ParseError("stmt() unmatched token: " +
                                      currentToken.type);
        }
    }
    
    private void exportList(TreeNode parent) throws ParseError, IOException {
        TreeNode node = new TreeNode(TreeNode.PRODUCTION.EXPORT_LIST, parent);
        
        switch (currentToken.type) {
        	
        	case LBRACK:
        		match(TokenType.LBRACK, node);
        		exportPred(node);
        		exportPredTail(node);
        		break;
        		
            default:
               throw new ParseError("expr() unmatched token: " +
                                      currentToken.type); 
        }
    }
    
    private void exportPred(TreeNode parent) throws ParseError, IOException {
        TreeNode node = new TreeNode(TreeNode.PRODUCTION.EXPORT_PRED, parent);
        
        switch (currentToken.type) {
        	
        	case ID:
        		String predName = currentToken.lexeme;
        		match(TokenType.ID, node);
        		this.module.addPredicate(predName, getArity(currentToken));
        		arity(node);
        		break;
        	
        	case RBRACK:
        		node.add(new TreeNode(TreeNode.PRODUCTION.EMPTY, null));
                break;
        		
            default:
               throw new ParseError("expr() unmatched token: " +
                                      currentToken.type); 
        }
    }


    private int getArity(Token token) {
    	switch (token.type) {
    	case ARITY1:
    		return 1;
    	case ARITY2:
    		return 2;
    	default:
    		return 0;
    	}
    }
    
    private void arity(TreeNode parent) throws ParseError, IOException {
        TreeNode node = new TreeNode(TreeNode.PRODUCTION.ARITY, parent);
        
        switch (currentToken.type) {
        	
        	case ARITY1:
        		match(TokenType.ARITY1, node);
        		break;
        	
        	case ARITY2:
        		match(TokenType.ARITY2, node);
        		break;
        	
            default:
               throw new ParseError("expr() unmatched token: " +
                                      currentToken.type); 
        }
    }
    
    private void exportPredTail(TreeNode parent) throws ParseError, IOException {
        TreeNode node = new TreeNode(TreeNode.PRODUCTION.EXPORT_PRED_TAIL, parent);
        
        switch (currentToken.type) {
        	
        	case COMA:
        		match(TokenType.COMA, node);
        		exportPred(node);
        		exportPredTail(node);
        		break;
        	
        	case RBRACK:
        		match(TokenType.RBRACK, node);
        		break;	
        	
        		
            default:
               throw new ParseError("expr() unmatched token: " +
                                      currentToken.type); 
        }
    }
    
    private void multifileList(TreeNode parent) throws ParseError, IOException {
        TreeNode node = new TreeNode(TreeNode.PRODUCTION.MULTIFILE_LIST, parent);
        
        switch (currentToken.type) {
        	
        	case NECK:
        		multifile(node);
        		multifileList(node);
        		break;
        	
        	case ID:
        	case EOF:
        		node.add(new TreeNode(TreeNode.PRODUCTION.EMPTY, null));
                break;
        		
            default:
               throw new ParseError("expr() unmatched token: " +
                                      currentToken.type); 
        }
    }
    
    private void multifile(TreeNode parent) throws ParseError, IOException {
        TreeNode node = new TreeNode(TreeNode.PRODUCTION.MULTIFILE, parent);
        
        switch (currentToken.type) {
        	
        	case NECK:
        		match(TokenType.NECK, node);
        		match(TokenType.MULTI, node);
        		match(TokenType.LPAREN, node);
        		String predName = currentToken.lexeme;
        		match(TokenType.ID, node);
        		if (this.module.getPredicate(predName, getArity(currentToken)) == null) {
        			this.module.addPredicate(predName, getArity(currentToken));
        		}
            	arity(node);
        		match(TokenType.RPAREN, node);
        		match(TokenType.DOT, node);
        		break;
        	
        	case ID:
        	case EOF:
        		node.add(new TreeNode(TreeNode.PRODUCTION.EMPTY, null));
                break;
        		
            default:
               throw new ParseError("expr() unmatched token: " +
                                      currentToken.type); 
        }
    }

    private void factList(TreeNode parent) throws ParseError, IOException {
        TreeNode node = new TreeNode(TreeNode.PRODUCTION.FACT_LIST, parent);
        
        switch (currentToken.type) {
        	
        	case ID:
        		fact(node);
        		factList(node);
        		break;
        	
        	case EOF:
        		node.add(new TreeNode(TreeNode.PRODUCTION.EMPTY, null));
                break;
        		
            default:
               throw new ParseError("expr() unmatched token: " +
                                      currentToken.type); 
        }
    }
    
    private void fact(TreeNode parent) throws ParseError, IOException {
        TreeNode node = new TreeNode(TreeNode.PRODUCTION.FACT, parent);
        
        switch (currentToken.type) {
        	
        	case ID:
        		String predName = currentToken.lexeme;
        		ArrayList<Functor> componentListContent = new ArrayList<>();
        		match(TokenType.ID, node);
        		match(TokenType.LPAREN, node);
        		componentListContent.add(component(node));
        		componentListContent = componentList(node, componentListContent);
        		module.addFact(predName, componentListContent);
        		match(TokenType.DOT, node);
        		break;
        	
        	case EOF:
        		node.add(new TreeNode(TreeNode.PRODUCTION.EMPTY, null));
                break;
        		
            default:
               throw new ParseError("expr() unmatched token: " +
                                      currentToken.type); 
        }
    }
    
    private ArrayList<Functor> componentList(TreeNode parent, ArrayList<Functor> compList) throws ParseError, IOException {
        TreeNode node = new TreeNode(TreeNode.PRODUCTION.COMP_LIST, parent);
        
        switch (currentToken.type) {
        	
        	case COMA:
        		match(TokenType.COMA, node);
        		compList.add(component(node));
        		return componentList(node, compList);
        		
        	case RPAREN:
        		match(TokenType.RPAREN, node);
        		node.add(new TreeNode(TreeNode.PRODUCTION.EMPTY, null));
        		return compList;
        		
            default:
               throw new ParseError("expr() unmatched token: " +
                                      currentToken.type); 
        }
    }
    
    private Functor component(TreeNode parent) throws ParseError, IOException {
        TreeNode node = new TreeNode(TreeNode.PRODUCTION.COMP, parent);
        
        switch (currentToken.type) {
        	
        	case ID:
        		String atom = null;
        		if (currentToken.lexeme.substring(0,1).toString().equals("\'")) {
        			atom = "\'" + currentToken.lexeme.subSequence(1, (currentToken.lexeme.length() - 1)).toString().toLowerCase() + "\'";
        		} else { atom = currentToken.lexeme; }
        		Functor newComponent = new Functor(atom);
        		match(TokenType.ID, node);
        		ArrayList<Functor> emptyComponents = new ArrayList<>();
        		ArrayList<Functor> functorTailComponents = new ArrayList<>();
        		functorTailComponents = functorTail(node, emptyComponents);
        		newComponent.addListArg(functorTailComponents);
        		return newComponent;
        	
        	case LBRACK:
        		match(TokenType.LBRACK, node);	
        		Functor newListComponent = new Functor("LIST");
        		ArrayList<Functor> prologList = new ArrayList<>();
        		Functor firstElem = listElem(node);
        		if (firstElem != null) {prologList.add(firstElem);}
        		newListComponent.addListArg(listTail(node, prologList));
        		return newListComponent;
        		
//        	case RPAREN:
//        		node.add(new TreeNode(TreeNode.PRODUCTION.EMPTY, null));
//                break;
        		
            default:
               throw new ParseError("expr() unmatched token: " +
                                      currentToken.type); 
        }
    }
    
    private ArrayList<Functor> functorTail(TreeNode parent, ArrayList<Functor> compList) throws ParseError, IOException {
        TreeNode node = new TreeNode(TreeNode.PRODUCTION.FUNCTOR_TAIL, parent);
        
        switch (currentToken.type) {
        	
        	case LPAREN:
        		match(TokenType.LPAREN, node);
        		compList.add(component(node));
        		return componentList(node, compList);
        	
        	case COMA:
        	case RPAREN:
        	case RBRACK:	
        		node.add(new TreeNode(TreeNode.PRODUCTION.EMPTY, null));
                return compList;
        		
            default:
               throw new ParseError("expr() unmatched token: " +
                                      currentToken.type); 
        }
    }
    
    
    private Functor listElem(TreeNode parent) throws ParseError, IOException {
        TreeNode node = new TreeNode(TreeNode.PRODUCTION.LISTELEM, parent);
        
        switch (currentToken.type) {
        	
        	case ID:
        		String atom = null;
        		if (currentToken.lexeme.substring(0,1).toString().equals("\'")) {
        			atom = "\"" + currentToken.lexeme.subSequence(1, (currentToken.lexeme.length() - 1)).toString().toLowerCase() + "\"";
        		} else { atom = currentToken.lexeme; }
        		Functor newComponent = new Functor(atom);
        		match(TokenType.ID, node);
        		ArrayList<Functor> emptyComponents = new ArrayList<>();
        		ArrayList<Functor> functorTailComponents = new ArrayList<>();
        		functorTailComponents = functorTail(node, emptyComponents);
        		newComponent.addListArg(functorTailComponents);
        		return newComponent;
        	
        	case RBRACK:
        		node.add(new TreeNode(TreeNode.PRODUCTION.EMPTY, null));
                return null;
        		
            default:
               throw new ParseError("expr() unmatched token: " +
                                      currentToken.type); 
        }
    }
    
    private ArrayList<Functor> listTail(TreeNode parent, ArrayList<Functor> prologList) throws ParseError, IOException {
        TreeNode node = new TreeNode(TreeNode.PRODUCTION.LIST_TAIL, parent);
        
        switch (currentToken.type) {
        	
        	case COMA:
        		match(TokenType.COMA, node);
        		Functor nextElem = listElem(node);
        		if (nextElem != null) {prologList.add(nextElem);}
        		return listTail(node, prologList);
        	
        	case RBRACK:
        		match(TokenType.RBRACK, node);
        		node.add(new TreeNode(TreeNode.PRODUCTION.EMPTY, null));
                return prologList;
        		
            default:
               throw new ParseError("expr() unmatched token: " +
                                      currentToken.type); 
        }
    }
    
    
    /**
     * If the current token has the given token type, read the next token,
     * otherwise, a ParseError is thrown.
     * 
     * @param type the expected token at this point in the parse
     * @throws ParseError the expected token wasn't found
     * @throws IOException an unexpected non-recoverable error occurred
     */
    private TreeNode match (TokenType type, TreeNode parent) 
            throws ParseError, IOException {
        TreeNode node = null;
        
        if (currentToken.type == type) {
            switch (type) {
                case ID:
                    node = new TreeNode(TreeNode.PRODUCTION.ID, parent, currentToken.lexeme);
                    break;
                case NECK:
                    node = new TreeNode(TreeNode.PRODUCTION.PUNCTUATION , parent, ":-");
                    break;
                case EOF:
                    node = new TreeNode(TreeNode.PRODUCTION.PUNCTUATION, parent, "$$");
                    break;
                default:
                     node = new TreeNode(TreeNode.PRODUCTION.PUNCTUATION,
                                        parent,
                                        currentToken.lexeme.toUpperCase());
                    break;
            }
            
            currentToken = scanner.next();
            
            return node;
        } else {
            throw new ParseError(type, scanner.getLineNo());
        }
    }
}
