package kbingest.translator;

import java.io.*;
import java.util.*;

import kbingest.parser.ParseError;
import kbingest.parser.PredictiveParser;
import kbingest.parser.TreeNode;

public class KB {

	private ArrayList<PlModule> moduleList;

	public KB(String[] filePaths) throws IOException, ParseError {

		this.moduleList  = new ArrayList<>();

		for (String filePath : filePaths) {
			Parser prologParser = new Parser(filePath);
			PredictiveParser parser = new PredictiveParser(prologParser);
			moduleList.add(parser.parseToModule());
			TreeNode parseTree = parser.parse();
			parseTree.prettyPrint();
		}

	}
	public ArrayList<PlModule> getModuleList(){
		return this.moduleList;
	}

	public void functionalWriting() {

		for (PlModule module : this.moduleList) {
			try {
				FileWriter myWriter = new FileWriter(module.getName() + ".hs");
				myWriter.write("module " + module.getName().toUpperCase());
				myWriter.write("\n");
				myWriter.write("( myId");
				for (Predicate pred : module.getPredicateList()) {
					if (pred.hasFacts()) {
						myWriter.write("\n , ");
						myWriter.write(pred.getName());
					}
				}
				for (Predicate pred : module.getPredicateList()) {
					if (pred.hasFacts() && (pred.getArity() == 2) ) {
						myWriter.write("\n , ");
						myWriter.write(pred.getName() + "_List1stComp");
					}
				}
				myWriter.write("\n");
				myWriter.write(") where \n\n"); 
				myWriter.write("myId x = x");
				for (Predicate pred : module.getPredicateList()) {
					if (pred.getCurriedFunc().isEmpty() == false){
						myWriter.write("\n\n");
						if (pred.getArity() == 2) {
//							here we assume that the first component is ALWAYS a simple string component but it's not the case (e.g. pic definition). For a more systematic approach, we might want to handle functors instead of strings.
							for (String simpleComponent : pred.getCurriedFunc()) {
								myWriter.write(pred.getName() + " " + simpleComponent + " = " + pred.getCurriedOperands(simpleComponent).toString() + "\n");
							}
//							for exhaustion of the pattern matching
							myWriter.write(pred.getName() + " x = []\n"); 
//							additional predicate with just list of first components
							myWriter.write(pred.getName() + "_List1stComp = " + pred.getCurriedFunc().toString() + "\n");
						} else {
							myWriter.write(pred.getName() + " " + " = " + pred.getCurriedFunc().toString());							
						}
					}
				}
				myWriter.close();
				System.out.println("Successfully wrote to the file.");
			} catch (IOException e) {
				System.out.println("An error occurred.");
				e.printStackTrace();
			}
		}
	}


	public void prologFactsWriting() {

		for (PlModule module : this.moduleList) {
			try {
				FileWriter myWriter = new FileWriter(module.getName() + "_groupedFacts.pl");
//				myWriter.write("%% List of predicates:\n");
//				for (Predicate pred : module.getPredicateList()) {
//					myWriter.write("%% ");
//					myWriter.write(pred.getName());
//					myWriter.write("\n");
//				}
				for (Predicate pred : module.getPredicateList()) {
					myWriter.write("%% Facts ragarding the predicate: ");
					myWriter.write(pred.getName());
					myWriter.write("\n");
					for (Fact fact : pred.getFactList()) {
						myWriter.write(pred.getName());
						myWriter.write("(");
						String s = "";
						for (Functor comp : fact.getComponents()) {
							s = s + comp.getComponentsAsString() + ", ";
						}
						myWriter.write(s.substring(0, (s.length() - 2)) + "). %%\n");
					}
				}
				myWriter.close();
			} catch (IOException e) {
				System.out.println("An error occurred.");
				e.printStackTrace();
			}
		}
	}


	public void clojureFactsWriting() {
		try {
			FileWriter myWriter = new FileWriter("/Users/hugolinbergier/dev/Clojure/clojquery/src/clojquery/kb.clj");
			myWriter.write("(ns clojquery.kb \n (:require [clojure.core.logic.pldb :as pldb]))");
			for (PlModule module : this.moduleList) {
				myWriter.write("\n\n\n;Module " + module.getName() + "\n\n;List of predicates:\n");
				for (Predicate pred : module.getPredicateList()) {
					myWriter.write("(pldb/db-rel ");
					myWriter.write(pred.getName());
					for (int i = 1; i <= pred.getArity(); i++) {
						myWriter.write(" a" + Integer.toString(i));
					}
					myWriter.write(")");
					myWriter.write("\n");
				}
//				[person 'phil]
				myWriter.write("(def " + module.getName() + " (pldb/db \n");
				for (Predicate pred : module.getPredicateList()) {
					myWriter.write("\n\n;Facts regarding the predicate: ");
					myWriter.write(pred.getName());
					myWriter.write("\n");
					for (Fact fact : pred.getFactList()) {
						myWriter.write("[");
						myWriter.write(pred.getName());
						myWriter.write(" ");
						String s = "";
						for (Functor comp : fact.getComponents()) {
							String s2 = comp.getComponentsAsString();
							if (comp.getArity() > 0) {
								s2 = "\'bogus_component_arity_too_big\'";
							}
							s = s + s2.substring(0, (s2.length() - 1)) + " ";
						}
						myWriter.write(s.substring(0, (s.length() - 1)) + "]\n");
					}
				}
				myWriter.write("))");
			}
			myWriter.close();
			} catch (IOException e) {
				System.out.println("An error occurred.");
				e.printStackTrace();
			}
		}

	public String alephWriting(PlModule m, Predicate p) {
		String fileName = m.getName() + "_aleph_" + p.getName() + ".pl";
		String topPredName = m.getPredicateList().get(0).getName();
		String categoriesName = m.getPredicateList().get(1).getName();
			try {

				FileWriter myWriter = new FileWriter(fileName);

				myWriter.write("% To run do the following:\n" +
						"%       a. consult this file\n" +
						"%       b. aleph:read_all(animals).\n" +
						"%       c. induce.\n");
				myWriter.write(":- use_module(library(aleph)).\n" +
						":- if(current_predicate(use_rendering/1)).\n" +
						":- use_rendering(prolog).\n" +
						":- endif.\n" +
						":- aleph.\n" +
						":- set_random(seed(111)).\n" +
						":- aleph_set(evalfn,posonly).\n" +
//						":- aleph_set(clauselength,2).\n" +
						":- aleph_set(gsamplesize,20).\n" +
						":- style_check(-discontiguous)." +
						"\n");
				myWriter.write("%%%%%%%%%%%%%%%%%%%%%%%%%%%%\n" +
						"% Mode declarations\n\n");

				myWriter.write(":- modeh(1," + p.getName() + "(+" + topPredName);
				if (p.getArity() > 1){
					myWriter.write(",#" + categoriesName + ")).\n");
				} else myWriter.write(")).\n");
				for (Predicate pred : m.getPredicateList()) {
					if (pred.isEqualToPredicate(p) == false) {
						myWriter.write(":- modeb(1," + pred.getName() + "(+" + topPredName);
						if (pred.getArity() > 1) {myWriter.write(",#" + categoriesName + ")).\n");}
						else {myWriter.write(")).\n");}
					}
				}
				myWriter.write("\n\n");
				for (Predicate pred : m.getPredicateList()) {
					if (pred.isEqualToPredicate(p) == false) {
						myWriter.write(":- determination(" + p.getName() + "/" + Integer.toString(p.getArity()) + "," + pred.getName() + "/" + Integer.toString(pred.getArity()) + ").\n");
					}
				}
				myWriter.write("\n\n");
				for (Predicate pred : m.getPredicateList()) {
						myWriter.write(":- dynamic(" + pred.getName() + "/" + Integer.toString(pred.getArity()) + ").\n");
				}
				myWriter.write("\n\n");
				myWriter.write(":- begin_bg.\n");
				for (Predicate pred : m.getPredicateList()) {
					if (pred.isEqualToPredicate(p) == false) {
						myWriter.write("\n%%");
						myWriter.write(pred.getName());
						myWriter.write("\n");
						for (Fact fact : pred.getFactList()) {
							myWriter.write(pred.getName());
							myWriter.write("(");
							String s = "";
							for (Functor comp : fact.getComponents()) {
								s = s + comp.getComponentsAsString() + ", ";
							}
							myWriter.write(s.substring(0, (s.length() - 2)) + ").\n");
						}
					} else {p = pred;}
				}
				myWriter.write("\n" +
						":-end_bg.\n" +
						"\n" +
						"\n" +
						":-begin_in_pos.\n" +
						"\n");
				for (Fact fact : p.getFactList()) {
					myWriter.write(p.getName());
					myWriter.write("(");
					String s = "";
					for (Functor comp : fact.getComponents()) {
						s = s + comp.getComponentsAsString() + ", ";
					}
					myWriter.write(s.substring(0, (s.length() - 2)) + ").\n");
				}
				myWriter.write("\n" +
						":-end_in_pos.\n");
				myWriter.close();
			} catch (IOException e) {
				System.out.println("An error occurred.");
				e.printStackTrace();
			}
		return fileName;
	}

}
