package kbingest.translator;

import java.util.*;

public class Predicate {

	private String name;
	private int arity;

	private ArrayList<Fact> factList;


	public Predicate(String name, int arity) {
		this.name = name;
		this.arity = arity;
		this.factList = new ArrayList<>();
	}

	public void addFact(ArrayList<Functor> components) {
		this.factList.add(new Fact(components));
	}

	public List<String> getCurriedOperands(String operator) {
		List<String> operands = new ArrayList<>();
		for (Fact fact : this.factList) {
			if (fact.getComponents().get(0).getName().equals(operator)) {
				operands.add(fact.getComponents().get(1).getComponentsAsTuples());
			}
		}
		return operands;
	}
			
	public ArrayList<String> getCurriedFunc() {
		ArrayList<String> curriedFunc = new ArrayList<>();
		for (Fact fact : this.factList) {
			if (curriedFunc.contains(fact.getComponents().get(0).getName()) == false) {
				curriedFunc.add(fact.getComponents().get(0).getName());
			}
		}
		return curriedFunc;
	}

	public boolean isEqualToPredicate(Predicate p){
		if ((this.getArity() == p.getArity()) && (this.getName().equals(p.getName()))){
			return true;
		} else {
			return false;
		}
	}
	public int getArity() {
		return this.arity;
	}

	public String getName() {
		return this.name;
	}
	
	public boolean hasFacts() {
		return (this.factList.size() > 0);
	}
	
	public ArrayList<Fact> getFactList() {
		return this.factList;
	}
	

}
