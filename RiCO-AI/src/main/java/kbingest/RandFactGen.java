package kbingest;

import kbingest.parser.ParseError;
import kbingest.translator.KB;
import kbingest.translator.Predicate;
import org.jpl7.*;

import java.io.FileWriter;
import java.io.IOException;
import java.lang.Integer;
import java.util.Random;

import java.util.concurrent.ThreadLocalRandom;

public class RandFactGen {


    private final KB kb;

    public RandFactGen(KB myKB) {
        this.kb = myKB;
    }

    public void run() throws IOException {
        Query q1 =
                new Query(
                        "use_module",
                        new Term[] {new Atom("resources/random_facts")}
                );
        System.out.println( "use_module resources/random_facts " + (q1.hasSolution() ? "succeeded" : "failed"));

        for (Predicate pred : this.kb.getModuleList().get(0).getPredicateList()) {
            Term[] predTerms = new Term[pred.getArity()];
            for (int i = 0 ; i < pred.getArity() ; i++){
                Variable X = new Variable("X" + Integer.toString(i));
                predTerms[i] = X;
            }
            Query q2 = new Query(
                            "prepare_randomization_metadata",
                            new Term[] {new Compound(pred.getName(), predTerms)}
                    );
            System.out.println( "Query " + q2.toString() + " " + (q2.hasSolution() ? "succeeded" : "failed"));
            Query q3 = new Query(
                    "get_X_random_pred",
                    new Term[] {new Atom (pred.getName()), new org.jpl7.Integer( ThreadLocalRandom.current().nextInt(5,12) ), new Variable("L")}
            );
            System.out.println( "get_X_random_pred results: " + q3.oneSolution().get("L"));
        }
    }

}
