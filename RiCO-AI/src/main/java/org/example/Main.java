package org.example;

import kbingest.ILPGen;
import kbingest.parser.ParseError;
import org.jpl7.*;

import java.io.*;

public class Main {
    public static void main(String[] args) throws ParseError, IOException, InterruptedException {
        System.out.println("Hello world!");
        ILPGen ILP = new ILPGen();
        ILP.run();
        runPrologAutoILP();
        runHaskell();
        //testProlog2();
    }

    public static void runPrologAutoILP(){
        Query q = new Query("consult", new Term[] {new Atom("auto_ilp.pl")});

        if (q.hasSolution())
            System.out.println("True");
        else
            System.out.println("False");
    }

    public static void runHaskell() throws IOException, InterruptedException {
        File location = new File("RiCO-AI\\resources");
        String[] cmd = {"ghc", "LODv2.hs"};
        ProcessBuilder builder = new ProcessBuilder(cmd);
        builder.directory(location);
        Process p = builder.start();

        //Testing stuff I got online
       /* InputStream stderr = p.getErrorStream();
        InputStreamReader isr = new InputStreamReader(stderr);
        BufferedReader br = new BufferedReader(isr);
        String line = null;
        System.out.println("<ERROR>");
        while ( (line = br.readLine()) != null)
            System.out.println(line);
        System.out.println("</ERROR>"); */
        int exitVal = p.waitFor();
        System.out.println("Exit Value: " + exitVal);

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

    public static void testProlog2(){
        Query q1 =
                new Query(
                        "consult",
                        new Term[] {new Atom("RiCO-AI/resources/animals.pl")}
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