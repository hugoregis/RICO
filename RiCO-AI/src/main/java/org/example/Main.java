package org.example;


import kbingest.ILPGen;
import kbingest.parser.ParseError;

import java.io.IOException;

public class Main {
    public static void main(String[] args) throws ParseError, IOException {
        System.out.println("Hello world!");
        ILPGen ILP = new ILPGen();
        ILP.run();
    }
}