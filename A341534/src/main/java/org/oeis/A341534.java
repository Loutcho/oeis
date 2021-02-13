package org.oeis;

import java.util.Map;
import java.util.Set;
import java.util.TreeMap;

public class A341534 {

	private static final int N = 100;
	
	public static void main(String[] args) {
		new A341534().run();
	}
	
	private void run() {

		int n = 1;

		Configuration initialConfiguration = Configuration.generateInitialConfiguration();

		Map<Configuration, Integer> population = new TreeMap<>();
		population.put(initialConfiguration, 1);
		System.out.print(population.size() + ", ");
		
		while (n < N) {
			n ++;
			Map<Configuration, Integer> newPopulation = new TreeMap<>();
			for (Map.Entry<Configuration, Integer> entry : population.entrySet()) {
				Configuration configuration = entry.getKey();
				Integer parentNumber = entry.getValue();
				Set<Configuration> children = configuration.getChildren();
				for (Configuration child : children) {
					Integer childNumber = newPopulation.get(child);
					if (childNumber == null) {
						childNumber = 0;
					} else {
						throw new IllegalStateException("Distinct parents are not supposed to be able to generate the same child!");
					}
					childNumber += parentNumber;
					newPopulation.put(child, childNumber);
				}
			}
			population = newPopulation;
			System.out.print(population.size() + ", ");
		}
		System.out.println("\n");

		/*
		for (Map.Entry<Configuration, Integer> entry : population.entrySet()) {
			Configuration configuration = entry.getKey();
			System.out.println(configuration.prettyPrint());
		}
		*/
	}
}

