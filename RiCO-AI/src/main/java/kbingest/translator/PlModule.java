package kbingest.translator;

import java.io.IOException;
import java.util.*;

public class PlModule {

	private ArrayList<Predicate> predicateList;
	
//	private int nbPredicates;
	
	private String name;
//	private String filePath;
	
	public PlModule() throws IOException {
//		this.nbPredicates = 0;
//		this.filePath = filePath;
		this.predicateList = new ArrayList<>();
		
	}
	
	public void setName(String name) {
		this.name = name;
	}
	public String getName() {
		return this.name;
	}
	
	public ArrayList<Predicate> getPredicateList() {
		return this.predicateList;
	}
	
	public int addPredicate(String name, int arity) {
		Predicate newPred = new Predicate(name, arity);
		this.predicateList.add(newPred);
//		this.nbPredicates ++; 
		return (this.predicateList.size());
	}
	
	public void addFact(String name, ArrayList<Functor> components) {
		if (this.isPredicate(name, components.size())){
			this.getPredicate(name, components.size()).addFact(components);
		} else {
			System.out.println("Predicate " + this.name + ":" + name + "/" + String.valueOf(components.size()) + " is not exported - Fact not added");
		}
	}
	
	public boolean isPredicate(String name, int arity) {
		for (Predicate pred : this.predicateList) {
			if (pred.getName().equals(name) && (pred.getArity() == arity)) {
				return true;
			}
		}
		return false;
	}
	
	public Predicate getPredicate(String name, int arity) {
		for (Predicate pred : this.predicateList) {
			if (pred.getName().equals(name) && (pred.getArity() == arity)) {
//				System.out.println("found predicate: "+ pred.getName() + "//" + String.valueOf(pred.getArity()));
				return pred;
			}
		}
		System.out.println("Issue finding: "+ name + "//" + String.valueOf(arity) + " in module " + this.name);
		return null;
	}
	
}
