package kbingest;



import kbingest.parser.ParseError;
import kbingest.translator.KB;
import kbingest.translator.Predicate;

import java.io.FileWriter;
import java.io.IOException;

public class ILPGen {
    public static final String SOURCE1 = "RiCO-AI/resources/animals.pl";

    public static void run() throws ParseError, IOException {
        System.out.println("Hello world!");
        String[] filePaths = {SOURCE1};
        KB myKB = new KB(filePaths);
        FileWriter plFile = new FileWriter("auto_ilp.pl");
        plFile.write(":- use_module(aleph_analysis).\n\n");
        Predicate topPredicate = myKB.getModuleList().get(0).getPredicateList().get(0);
        for (Predicate pred : myKB.getModuleList().get(0).getPredicateList()) {
            if ((pred.isEqualToPredicate(topPredicate)) == false) {
                plFile.write(":- [\'" + myKB.alephWriting(myKB.getModuleList().get(0), pred) + "\'].\n");
                plFile.write(":- \\+ aleph:read_all( "+ topPredicate.getName() + ").\n");
                plFile.write(":- induce.\n" +
                        ":- write_rules." +
                        "\n");
            }
        }
        plFile.close();
    }


}