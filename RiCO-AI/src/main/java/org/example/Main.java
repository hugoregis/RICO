package org.example;

import OpenAPI.Test;
import com.theokanning.openai.completion.CompletionRequest;
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

public class Main {
    public static void main(String[] args) throws ParseError, IOException, InterruptedException {
        System.out.println("Hello world!");
        String system = System.getProperty("os.name");
        String[] filePaths = new String[1];

        if (system.contains("Windows"))
            filePaths[0] = "RICO-AI/resources/animals.pl";
        else
            filePaths[0] = "resources/animals.pl";

       KB myKB = new KB(filePaths);
       ILPGen ILP = new ILPGen(myKB);
       ILP.run();
       runHaskell();

        //OpenAI API test:
        //Test t = new Test();
        //t.run();

        runPrologAutoILP();
        testProlog2();
        RandFactGen FactGen = new RandFactGen(myKB);
        FactGen.run();
        
       String query = "SELECT ?subject ?predicate ?object\n" +
                "WHERE {\n" +
                "  ?subject ?predicate ?object\n" +
                "}\n" +
                "LIMIT 25";

        String serviceURI = "http://52.54.229.238:3030/dataset2";
        String mine= "http://localhost:3030/Sp52.54.229.238arnatural";

        RDFConnection conn = RDFConnection.connect(serviceURI);
        QueryExecution q = conn.query(query) ;
        //QueryExecution q = QueryExecution.service(serviceURI).query(query).build();


        ResultSet results = q.execSelect();

        ResultSetFormatter.out(System.out, results);

        while (results.hasNext()) {
            QuerySolution soln = results.nextSolution();
            RDFNode x = soln.get("x");
            System.out.println(x);
        }

        conn.close() ;

    }

    public static void runPrologAutoILP(){
        Query q = new Query("consult", new Term[] {new Atom("auto_ilp.pl")});

        if (q.hasSolution())
            System.out.println("True");
        else
            System.out.println("False");
    }

    public static void runHaskell() throws IOException, InterruptedException {

        String system = System.getProperty("os.name");
        File location;

        System.out.println("Executing Haskell query...");

        if (system.contains("Windows"))
            location = new File("RiCO-AI\\resources");
        else
            location = new File("/resources");

        String[] cmd = {"runghc","LODv2.hs","tau","(Being Bird)"};
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