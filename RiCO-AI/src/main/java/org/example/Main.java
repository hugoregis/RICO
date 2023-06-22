package org.example;


//import OpenAPI.Test;
import OpenAPI.CompletionRequest;
import com.theokanning.openai.service.OpenAiService;
import kbingest.ILPGen;
import kbingest.RandFactGen;

import kbingest.parser.ParseError;
import kbingest.translator.KB;
import org.apache.jena.query.QueryExecution;
import org.apache.jena.query.QuerySolution;
import org.apache.jena.query.ResultSet;
import org.apache.jena.query.ResultSetFormatter;
import org.apache.jena.rdf.model.RDFNode;
import org.apache.jena.rdfconnection.RDFConnection;
import org.jpl7.Atom;
import org.jpl7.Query;
import org.jpl7.Term;
import org.jpl7.Variable;

import java.io.*;
import java.util.Scanner;

public class Main {
    public static void main(String[] args) throws ParseError, IOException, InterruptedException {
        System.out.println("Hello world!");
        String system = System.getProperty("os.name");
        String[] filePaths = new String[1];

        if (system.contains("Windows"))
            filePaths[0] = "RICO-AI/resources/animals.pl";
        else
            filePaths[0] = "resources/animals.pl";

        //        testProlog2();  //test JPL

       KB myKB = new KB(filePaths);
       ILPGen ILP = new ILPGen(myKB);
       ILP.run();
       runPrologAutoILP();

       Query q = new Query("fly", new Term[] {new Variable("X")});

       System.out.println("FLying animal : " + q.oneSolution().get("X")); // To demonstrate the persisting working memory of the swipl process

       RandFactGen FactGen = new RandFactGen(myKB);
       String listPred = FactGen.run();


       CompletionRequest GPTFacts = new CompletionRequest(); // is it really going to include all the new rules. We need to import them to make sure it is the case.
       GPTFacts.runPL2English(listPred);
//
//       runRICOAutoILP("init_ILP5");

       runHaskell("predicate", "(Being Bird)", "Delta (Det {detChain = []}) (Being Ostrich)");
       runHaskell("predicate", "(Being Fly)", "Delta (Det {detChain = []}) (Being Ostrich)");
       runHaskell("predicate", "(Being Fly)", "Delta (Det {detChain = []}) (Being Bird)");


        testSPARQLQuery();

    }


    public static void testSPARQLQuery() throws FileNotFoundException {
        String query = "PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>\n" +
                "PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#> \n" +
                "PREFIX skos: <http://www.w3.org/2004/02/skos/core#> \n" +
                "SELECT DISTINCT ?this ?this_label WHERE {\n" +
                "  ?this rdf:type <https://www.ica.org/standards/RiC/ontology#Person>.\n" +
                "  ?this <https://www.ica.org/standards/RiC/ontology#hasOrHadLocation> ?Lieu_1.\n" +
                "  ?Lieu_1 rdf:type <https://www.ica.org/standards/RiC/ontology#Place>.\n" +
                "  ?Lieu_1 <https://www.ica.org/standards/RiC/ontology#hasOrHadPlaceType> <http://data.archives-nationales.culture.gouv.fr/placeType/paroisse>.\n" +
                "  ?Lieu_1 ^<https://www.ica.org/standards/RiC/ontology#hasOrHadLocation> ?Personne_3.\n" +
                "  ?Personne_3 rdf:type <https://www.ica.org/standards/RiC/ontology#Person>.\n" +
                "  \n" +
                "  ?this <https://www.ica.org/standards/RiC/ontology#hasOrHadAgentName>/<http://www.w3.org/2000/01/rdf-schema#label>|<http://www.w3.org/2000/01/rdf-schema#label> ?this_label.\n" +
                "}\n" +
                "LIMIT 10";
        String query2 = "PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>\n" +
                "PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>\n" +
                "PREFIX skos: <http://www.w3.org/2004/02/skos/core#>\n" +
                "PREFIX rico: <https://www.ica.org/standards/RiC/ontology#>\n" +
                "\n" +
                "select ?s ?o WHERE \n" +
                "{?s rico:isAssociatedWithPlace ?o} \n" +
                "LIMIT 10";

        queryServer(query);
        queryServer(query2);
    }


    public static void runPrologAutoILP(){
        Query q = new Query("consult", new Term[] {new Atom("auto_ilp.pl")});

        if (q.hasSolution())
            System.out.println("True");
        else
            System.out.println("False");
    }

    public static void runRICOAutoILP(String goal) throws IOException {
        String system = System.getProperty("os.name");
        File location;

        System.out.println("Executing SWI-Prolog query:" + " " + goal);

        if (system.contains("Windows"))
            location = new File("RiCO-AI\\resources");
        else
            location = new File("resources/");

        String[] cmd = {"swipl", "-g", goal, "rdf2pl.pl"};
        ProcessBuilder builder = new ProcessBuilder(cmd);
        builder.directory(location);
        Process p = builder.start();

        InputStream is = p.getInputStream();
        BufferedReader reader = new BufferedReader(new InputStreamReader(is));

        String line = null;
        while ((line = reader.readLine()) != null) {
            System.out.println(line);
        }

        p.destroy();

    }

    public static void runHaskell(String func, String arg1, String arg2) throws IOException, InterruptedException {

        String system = System.getProperty("os.name");
        File location;

        System.out.println("Executing Haskell query:" + " " + func + " " + arg1 + " " + arg2);

        if (system.contains("Windows"))
            location = new File("RiCO-AI\\resources");
        else
            location = new File("resources/");

        String[] cmd = {"runghc","LODv2.hs",func , arg1, arg2};
        ProcessBuilder builder = new ProcessBuilder(cmd);
        builder.directory(location);
        Process p = builder.start();

        InputStream is = p.getInputStream();
        BufferedReader reader = new BufferedReader(new InputStreamReader(is));

        String line = null;
        while ((line = reader.readLine()) != null) {
            System.out.println(line);
        }


        //Testing stuff I got online
       /* InputStream stderr = p.getErrorStream();
        InputStreamReader isr = new InputStreamReader(stderr);
        BufferedReader br = new BufferedReader(isr);
        String line = null;
        System.out.println("<ERROR>");
        while ( (line = br.readLine()) != null)
            System.out.println(line);
        System.out.println("</ERROR>"); */
//        int exitVal = p.waitFor();
//        System.out.println("Exit Value: " + exitVal);

        p.destroy();
        }

        public static void queryServer(String query) throws FileNotFoundException {

            String system = System.getProperty("os.name");
            File config;


            if (system.contains("Windows"))
                config = new File("RiCO-AI\\config.txt");
            else
                config = new File("config.txt"); //Please put correct directory

            Scanner scan = new Scanner(config);

            String IP = scan.nextLine();
            String DS = scan.nextLine();

            IP = IP.replace("IP: ", "");
            DS = DS.replace("DS: ", "");

            String serviceURI = "http://"+ IP + ":3030/" + DS;

            //String mine = "http://localhost:3030/Sparnatural";

            try {
                RDFConnection conn = RDFConnection.connect(serviceURI);
                QueryExecution q = conn.query(query);
                //QueryExecution q = QueryExecution.service(serviceURI).query(query).build();


                ResultSet results = q.execSelect();

                ResultSetFormatter.out(System.out, results);

                while (results.hasNext()) {
                    QuerySolution soln = results.nextSolution();
                    RDFNode x = soln.get("x");
                    System.out.println(x);
                }


                conn.close();
            }

            catch (Exception e){
                System.out.println("The server is offline right now");
            }
        }


   public static void testProlog1(){  //From: https://jpl7.org/TutorialJavaCallsProlog
        Query q1 =
                new Query(
                        "consult",
                        new Term[] {new Atom("test.pl")}
                );
        System.out.println( "consult " + (q1.hasSolution() ? "succeeded" : "failed"));
        Query q2 =
                new Query(
                        "child_of",
                        new Term[] {new Atom("joe"),new Atom("ralf")}
                );
        System.out.println(
                "child_of(joe,ralf) is " +
                        ( q2.hasSolution() ? "provable" : "not provable" )
        );
        Query q3 =
                new Query(
                        "descendent_of",
                        new Term[] {new Atom("steve"),new Atom("ralf")}
                );
        System.out.println(
                "descendent_of(joe,ralf) is " +
                        ( q3.hasSolution() ? "provable" : "not provable" )
        );


        Variable X = new Variable("X");
        Query q4 =
                new Query(
                        "descendent_of",
                        new Term[] {X,new Atom("ralf")}
                );

        java.util.Map<String,Term> solution;

        solution = q4.oneSolution();

        System.out.println( "first solution of descendent_of(X, ralf)");
        System.out.println( "X = " + solution.get("X"));

        java.util.Map<String,Term>[] solutions = q4.allSolutions();
        for ( int i=0 ; i < solutions.length ; i++ ) {
            System.out.println( "X = " + solutions[i].get("X"));
        }

        System.out.println( "each solution of descendent_of(X, ralf)");
        while ( q4.hasMoreSolutions()) {
            solution = q4.nextSolution();
            System.out.println( "X = " + solution.get("X"));
        }

        Variable Y = new Variable();
        Query q5 =
                new Query(
                        "descendent_of",
                        new Term[] {X,Y}
                );

        while ( q5.hasMoreSolutions() ){
            solution = q5.nextSolution();
            System.out.println( "X = " + solution.get("X") + ", Y = " + solution.get("Y"));
        }
    }

    public static void testProlog2() {

        String system = System.getProperty("os.name");
        System.out.println(system);
        String animals;

        if (system.contains("Windows")){
            animals = "RICO-AI\\resources\\animals.pl";
        }
        else{
            animals = "resources/animals.pl";
        }

        Query q1 =
                new Query(
                        "consult",
                        new Term[] {new Atom(animals)}
                );
        System.out.println( "consult " + (q1.hasSolution() ? "succeeded" : "failed"));
        Query q2 =
                new Query(
                        "animal",
                        new Term[] {new Atom("eagle")}
                );
        System.out.println(
                "animal(eagle) is " +
                        ( q2.hasSolution() ? "provable" : "not provable" )
        );
        Query q3 =
                new Query(
                        "class",
                        new Term[] {new Atom("eagle"),new Atom("bird")}
                );
        System.out.println(
                "class(eagle,bird) is " +
                        ( q3.hasSolution() ? "provable" : "not provable" )
        );
        Query q4 =
                new Query(
                        "class",
                        new Term[] {new Atom("eagle"),new Atom("mammal")}
                );
        System.out.println(
                "class(eagle,mammal) is " +
                        ( q4.hasSolution() ? "provable" : "not provable" )
        );
    }


}