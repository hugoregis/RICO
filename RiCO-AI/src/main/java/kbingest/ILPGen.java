package kbingest;



import kbingest.parser.ParseError;
import kbingest.translator.KB;
import kbingest.translator.Predicate;

import java.io.FileWriter;
import java.io.IOException;
import java.util.ArrayList;

public class ILPGen {

    private KB kb;

    public ILPGen(KB myKB) {
        this.kb = myKB;
    }

    public void run() throws ParseError, IOException {
        FileWriter plFile = new FileWriter("auto_ilp.pl");
        plFile.write(":- use_module('resources/aleph_analysis').\n\n");
//        for (Predicate pred : this.kb.getModuleList().get(0).getPredicateList()) {
//            plFile.write(":- multifile(" + pred.getName() + "/" + Integer.toString(pred.getArity()) + ").\n");
//        }
//        plFile.write("\n\n");
        Predicate topPredicate = this.kb.getModuleList().get(0).getPredicateList().get(0);
        for (Predicate pred : this.kb.getModuleList().get(0).getPredicateList()) {
            if ((pred.isEqualToPredicate(topPredicate)) == false) {
                plFile.write(":- [\'" + this.kb.alephWriting(this.kb.getModuleList().get(0), pred) + "\'].\n");
                plFile.write(":- \\+ aleph:read_all( "+ topPredicate.getName() + ").\n");
                plFile.write(":- induce.\n" +
                        ":- write_rules." +
                        "\n");
            }
        }
        plFile.close();
    }


}