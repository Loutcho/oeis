package org.oeis;

import java.util.Map;
import java.util.Set;
import java.util.TreeMap;

public class A341534 {

	private static final int N = 10;
	private boolean svg = true;
	private boolean data = (! svg);
	
	public static void main(String[] args) {
		new A341534().run();
	}
	
	private void run() {

		if (svg) {
			prettyPrintSvgBefore();
		}
		
		int n = 1;

		Configuration initialConfiguration = Configuration.generateInitialConfiguration();

		Map<Configuration, Integer> population = new TreeMap<>();
		population.put(initialConfiguration, 1);
		if (data) {
			System.out.print(population.size() + ", ");
		}
		if (svg) {
			prettyPrint(population, n);
		}
		
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
			if (data) {
				System.out.print(population.size() + ", ");
			}
			if (svg) {
				prettyPrint(population, n);
			}
		}
		
		if (svg) {
			prettyPrintSvgAfter();
		}
	}
	
	public static final double DX = 100.0;
	public static final double DY = 70.0;
	
	private void prettyPrintSvgBefore() {
		double width = DX * (N + 2);
		double height = DY * ((81.0 / 63.0) * N + 2); // estimation;
		StringBuilder sb = new StringBuilder();
		sb.append("<?xml version='1.0' standalone='no'?>\n");
		sb.append("<!DOCTYPE svg PUBLIC '-//W3C//DTD SVG 1.1//EN' 'http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd'>\n");
		sb.append("<svg xmlns='http://www.w3.org/2000/svg' version='1.1' width='" + width + "' height='" + height + "' background-color='black'>\n");
		sb.append("\t<rect fill='#000000' width='" + width + "' height='" + height + "' />\n");
		System.out.print(sb.toString());
	}
	
	private void prettyPrintSvgAfter() {
		System.out.print("</svg>\n");
	}
	
	private void prettyPrint(Map<Configuration, Integer> population, int n) {
		int c = 0;
		for (Map.Entry<Configuration, Integer> entry : population.entrySet()) {
			c ++;
			Configuration configuration = entry.getKey();
			System.out.print(configuration.prettyPrint(n + 1, n / 2 + 1, n, c));
		}
	}
}

