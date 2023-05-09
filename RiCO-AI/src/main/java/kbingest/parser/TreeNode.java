package kbingest.parser;

import java.util.ArrayList;

/**
 * A node appearing in a parse tree. 
 * 
 * 
 */
public class TreeNode {
    /**
     * Internal nodes correspond to a named LHS production, while leaf
     * nodes may be a punctuation, keyword (e.g. READ, WRITE), or an
     * empty string in a RHS
     */
    public enum PRODUCTION {ID, KB, KB_CLAUSES, MODULE_DEF,
                            EXPORT_LIST, EXPORT_PRED, ARITY, EXPORT_PRED_TAIL,
                            MULTIFILE_LIST, MULTIFILE, FACT_LIST, FACT, COMP_LIST, 
                            COMP, FUNCTOR_TAIL, LISTELEM, LIST_TAIL, PUNCTUATION, EMPTY};
    
    /**
     * The production associated with this node (see PRODUCTION)
     */
    public final PRODUCTION type;
     
    /**
     * The lexeme associated with this node, if any
     */
    public final String lexeme;
  
    /**
     * The name of this node, which is derived.
     */
    public final String name;
    
    /**
     * Convenience pointer to the parent of this node, if any.
     */
    public final TreeNode parent;
    
    /**
     * The children of this node, which will size zero, if there are none.
     */
    private ArrayList<TreeNode> children;
    
    /**
     * Create this node with the given type, parent, and an empty lexeme
     * @param type
     * @param parent 
     */
    public TreeNode(PRODUCTION type, TreeNode parent) {
        this(type, parent, "");
    }
    
    /**
     * Create this node with the given type, parent, and lexeme
     * @param type
     * @param parent
     * @param lexeme 
     */
    public TreeNode(PRODUCTION type, TreeNode parent, String lexeme) {
        this.type = type;
        this.lexeme = lexeme;
        this.parent = parent;
        
        children = new ArrayList<>();
        
        name = buildName(type);
        
        if (parent != null)
            parent.add(this);
    }
    
    /**
     * Return this node's production type
     * @return a PRODUCTION (see PRODUCTION)
     */
    public String getProduction() {
        return name;
    }
    
    /**
     * Add the given child to this node
     * 
     * @param child 
     */
    public void add(TreeNode child) {
        children.add(child);
    }
    
    /**
     * Output this node with no indentation
     */
    public void prettyPrint() {
        prettyPrint(0);
    }
    
    /**
     * Output this node prefixed with the given indentation
     * 
     * @param indent number of spaces to indent
     */
    public void prettyPrint(int indent) {
        for (int i = 0; i < indent; i++)
            System.out.print(" ");
        
        System.out.println(name);
        indent++;
        
        for (TreeNode node : children)
            node.prettyPrint(indent);
    }
    
    /**
     * Return this node's children
     * 
     * @return 
     */
    public ArrayList<TreeNode> getChildren() {
        return children;
    }
    
    /**
     * Derive this node's name from its production type
     * 
     * @param type
     * @return internal nodes are the name of the LHS production
     *   a leaf will have some form of the lexeme
     */    
    private String buildName(PRODUCTION type) {
        switch (type) {
        case KB:
                return "kb";
            
        case EMPTY: // SPecial
            return "e";
            
        case KB_CLAUSES:
            return "kbClauses";
            
        case MODULE_DEF:
            return "moduleDef";
            
        case EXPORT_LIST:
            return "exportList";
            
        case EXPORT_PRED:
            return "exportPred";
        
        case ID:
            return "ID(" + lexeme +")";
        
        case ARITY:
        	return "ARITY(" + lexeme +")";
            
        case EXPORT_PRED_TAIL:
            return "exportPredTail";
        
        case MULTIFILE_LIST:
            return "multiFileList";
            
        case FACT:
            return "fact";

        case FACT_LIST:
            return "factList";
        
        case MULTIFILE:
            return "multifile";
            
        case COMP_LIST:
            return "compList";
            
        case COMP:
            return "comp";
        
        case FUNCTOR_TAIL:
            return "functorTail";
        
        case LIST_TAIL:
            return "listTail";
        
        case LISTELEM:
            return "list";
        
        case PUNCTUATION:
            return lexeme;
        
        
        default:
            return "Illegal";
        }
    }
}
