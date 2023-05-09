package kbingest.translator;

import kbingest.parser.ParseError;

import java.io.IOException;

public class MainTranslator {
    public static final String SOURCE1 = "//Users//hugolinbergier//dev//Clojure//Query2//kb//faucModule.pl";
    public static final String SOURCE2 = "//Users//hugolinbergier//dev//Clojure//Query2//kb//jobModule.pl";
    public static final String SOURCE3 = "//Users//hugolinbergier//dev//Clojure//Query2//kb//programModule.pl";

    public static void run() throws IOException, ParseError {
        String[] filePaths = {SOURCE1, SOURCE2, SOURCE3};
        KB myKB = new KB(filePaths);
//		myKB.functionalWriting();
        myKB.prologFactsWriting();
    }

}
