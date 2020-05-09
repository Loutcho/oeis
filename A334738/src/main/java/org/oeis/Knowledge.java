package org.oeis;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;
import java.util.TreeSet;

public class Knowledge {

	public Map<Function, Set<Node>> functions = new TreeMap<>();
	public Map<Integer, List<Function>> functionsByDistance = new TreeMap<>();

	private Knowledge() {
	}
	
	public static Knowledge init() {
		Knowledge initialKnowledge = new Knowledge();
		Set<Node> representations = new TreeSet<>();
		representations.add(new X());
		initialKnowledge.functions.put(Function.x, representations);
		List<Function> ranked0 = new ArrayList<>();
		ranked0.add(Function.x);
		initialKnowledge.functionsByDistance.put(0, ranked0);
		return initialKnowledge;
	}
	
	@Override
	public String toString() {
		StringBuilder sb = new StringBuilder();
		int n = functionsByDistance.size();
		sb.append("{\n");
		for (int i = 0; i < n; i ++) {
			sb.append("  Distance " + i + ":\n");
			for(Function f : functionsByDistance.get(i)) {
				Set<Node> representationsOfF = functions.get(f);
				sb.append("    ");
				sb.append(f);
				sb.append(" = \n");
				for (Node representation : representationsOfF) {
					sb.append("      ");
					sb.append(representation);
					sb.append("\n");
				}
			}
		}
		sb.append("}\n");
		return sb.toString();
	}
}
