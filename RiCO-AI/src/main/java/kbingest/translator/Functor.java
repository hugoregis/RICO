package kbingest.translator;

import java.util.ArrayList;

public class Functor {

	private String name;
	private int arity;
	private ArrayList<Functor> components;

	private int depth;
	

	public Functor(String name) {
		this.name = name;
		this.arity = 0;
		this.components = new ArrayList<>();
	}

	public void initDepth(){
		this.depth = 0;
	}

	public void addArg(Functor component) {
		this.components.add(component);
	}

	public void addListArg(ArrayList<Functor> newComponents) {
		for (Functor comp : newComponents) {
			this.components.add(comp);
		}
	}
	
	public String getComponentsAsTuples() {
		if (this.hasComponents() && this.name != "LIST") {
			String tupleFormat = "(";
			for (Functor comp : this.components) {
				tupleFormat = tupleFormat + comp.getComponentsAsTuples() + ", ";
			}
			tupleFormat = tupleFormat.substring(0, (tupleFormat.length() - 2)) + ")";
			return tupleFormat;
		} else if (this.name == "LIST") {
			String listFormat = "[";
			for (Functor comp : this.components) {
				listFormat = listFormat + comp.getComponentsAsTuples() + ", ";
			}
			listFormat = listFormat.substring(0, (listFormat.length() - 2)) + "]";
			return listFormat;
		} else {
			return this.name;
		}
	}
	
	public String getComponentsAsString() {
		if (this.hasComponents() && this.name != "LIST") {
			String s = this.name + "(";
			for (Functor comp : this.components) {
				s = s + comp.getComponentsAsString() + ", ";
			}
			s = s.substring(0, (s.length() - 2)) + ")";
			return s;
		} else if (this.name == "LIST") {
			String listFormat = "[";
			for (Functor comp : this.components) {
				listFormat = listFormat + comp.getComponentsAsString() + ", ";
			}
			listFormat = listFormat.substring(0, (listFormat.length() - 2)) + "]";
			return listFormat;
		} else {
			return this.name;
		}
	}
	
	public ArrayList<Functor> getComponents() {
		return this.components;
	}
	
	public boolean hasComponents() {
		return (this.components.size() != 0);
	}

	public int getArity() {
		return this.components.size();
	}

	public String getName() {
		return this.name;
	}

	
}
