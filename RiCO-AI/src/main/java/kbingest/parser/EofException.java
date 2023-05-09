package kbingest.parser;


public class EofException extends Exception {
    public EofException (String msg) {
        super(msg);
    }
}
