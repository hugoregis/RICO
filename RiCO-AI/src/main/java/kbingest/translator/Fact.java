package kbingest.translator;

import java.util.*;

public class Fact {
	private ArrayList<Functor> components;
	
	public Fact(ArrayList<Functor> components) {
		this.components = components;
		for (Functor comp: this.components){
			comp.initDepth();
		}
	}
	
	public ArrayList<Functor> getComponents(){
		return this.components;
	}
	
}
// treat a functor as a tuple -- functor name is the "type" (as comment in Haskell)