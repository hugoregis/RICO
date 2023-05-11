package org.example;

import kbingest.ILPGen;
import kbingest.parser.ParseError;
import org.jpl7.*;

import java.io.IOException;

public class Main {
    public static void main(String[] args) throws ParseError, IOException {
        System.out.println("Hello world!");
        ILPGen ILP = new ILPGen();
        ILP.run();

        Query q = new Query("consult", new Term[] {new Atom("auto_ilp.pl")});

        if (q.hasSolution())
            System.out.println("True");
        else
            System.out.println("False");
    }
}