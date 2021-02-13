package org.oeis;

import java.util.Map;
import java.util.Set;
import java.util.SortedMap;
import java.util.TreeMap;
import java.util.TreeSet;

public class Configuration implements Comparable<Configuration> {

	public static Configuration generateInitialConfiguration() {
		Configuration c = new Configuration(new Direction(-1, +1), new Direction(-1,  0));
		c.addPoint(new Point(0, 0));
		return c;
	}
	
	private Direction min;
	private Direction max;
	private Map<Point, Integer> mapPointMultiplicity;
	
	private Configuration(Direction min, Direction max) {
		this.min = min;
		this.max = max;
		mapPointMultiplicity = new TreeMap<>();
	}

	public void addPoint(Point p) {
		Integer n = mapPointMultiplicity.get(p);
		n = (n == null) ? 1 : (n + 1);
		mapPointMultiplicity.put(p, n);
	}

	public Map<Direction, Set<Pair>> computeMapDirectionPairs() {
		Set<Pair> pairs = new TreeSet<>();
		Map<Direction, Set<Pair>> mapDirectionsPairs = new TreeMap<>();
		Set<Point> points = mapPointMultiplicity.keySet();
		for (Point p1 : points) {
			for (Point p2 : points) {
				if (! p1.equals(p2)) {
					Pair pair = new Pair(p1, p2);
					if (! pairs.contains(pair)) {
						pairs.add(pair);
						Direction direction = pair.getDirection();
						assert(direction.compareTo(new Direction(-1, 0)) <= 0);
						Set<Pair> pairsOfThisDirection = mapDirectionsPairs.get(direction);
						if (pairsOfThisDirection == null) {
							pairsOfThisDirection = new TreeSet<>();
							mapDirectionsPairs.put(direction, pairsOfThisDirection);
						}
						pairsOfThisDirection.add(pair);
					}
				}
			}
		}
		return mapDirectionsPairs;
	}
	
	private SortedMap<Integer, Set<Point>> computeOrder() {
		Direction intermediary = Direction.intermediary(min, max);
		Direction normal = intermediary.rotateCounterclockwise();
		SortedMap<Integer, Set<Point>> mapScalarProductPoints = new TreeMap<>();
		for (Point point : mapPointMultiplicity.keySet()) {
			int scalarProduct = normal.x * point.x + normal.y * point.y;
			Set<Point> pointsWithThisScalarProduct = mapScalarProductPoints.get(scalarProduct);
			if (pointsWithThisScalarProduct == null) {
				pointsWithThisScalarProduct = new TreeSet<>();
				mapScalarProductPoints.put(scalarProduct, pointsWithThisScalarProduct);
			}
			pointsWithThisScalarProduct.add(point);
		}
		return mapScalarProductPoints;
	}
	
	private Point findBiggestShare() {
		SortedMap<Integer, Set<Point>> mapScalarProductPoints = computeOrder();
		Integer leastScalarProduct = mapScalarProductPoints.firstKey();
		Set<Point> pointsWithLeastScalarProduct = mapScalarProductPoints.get(leastScalarProduct);
		if (pointsWithLeastScalarProduct.size() != 1) {
			throw new IllegalStateException("Assumption pointsWithLeastScalarProduct.size() always equals 1 is false");
		}
		Point pointWithLeastScalarProduct = null; for (Point p : pointsWithLeastScalarProduct) { pointWithLeastScalarProduct = p; }
		return pointWithLeastScalarProduct;
	}
	
	@Override
	public Configuration clone() {
		Configuration newConfiguration = new Configuration(min, max);
		newConfiguration.mapPointMultiplicity.putAll(mapPointMultiplicity);
		return newConfiguration;
	}
	
	private Configuration split(Point biggestShare) {
		Configuration newConfiguration = this.clone(); // but:
		Integer o = newConfiguration.mapPointMultiplicity.get(biggestShare);
		if (o == null) {
			throw new IllegalStateException("nbBiggestShares == null is not supposed to happen!");
		}
		int nbBiggestShares = o;
		if (nbBiggestShares == 0) {
			throw new IllegalStateException("nbBiggestShares == 0 is not supposed to happen!");
		}
		Point right = new Point(biggestShare.x + 1, biggestShare.y);
		Point up = new Point(biggestShare.x, biggestShare.y + 1);
		Integer nbRight = newConfiguration.mapPointMultiplicity.get(right);
		Integer nbUp = newConfiguration.mapPointMultiplicity.get(up);
		nbRight = (nbRight == null) ? 1 : nbRight + 1;
		nbUp = (nbUp == null) ? 1 : nbUp + 1;
		newConfiguration.mapPointMultiplicity.put(right, nbRight);
		newConfiguration.mapPointMultiplicity.put(up, nbUp);
		nbBiggestShares -= 1;
		if (nbBiggestShares == 0) {
			newConfiguration.mapPointMultiplicity.remove(biggestShare);
		} else {
			newConfiguration.mapPointMultiplicity.put(biggestShare, nbBiggestShares);
		}
		return newConfiguration;
	}
	
	private Direction getPivotalDirection() { // only works on a configuration whose min / max pair is not yet narrowed
		Map<Direction, Set<Pair>> mapDirectionPairs = computeMapDirectionPairs();
		Set<Direction> directions = mapDirectionPairs.keySet();
		Set<Direction> directionsLessThanOrEqualToMin = new TreeSet<>();
		Set<Direction> directionsMoreThanOrEqualToMax = new TreeSet<>();
		Set<Direction> directionsBetweenMinAndMax = new TreeSet<>();
		for (Direction direction : directions) {
			int comparisonToMin = direction.compareTo(min);
			int comparisonToMax = direction.compareTo(max);
			if (comparisonToMin <= 0) {
				directionsLessThanOrEqualToMin.add(direction);
			}
			if (comparisonToMax >= 0) {
				directionsMoreThanOrEqualToMax.add(direction);
			}
			if ((comparisonToMin > 0) && (comparisonToMax < 0)) {
				directionsBetweenMinAndMax.add(direction);
			}
		}
		switch (directionsBetweenMinAndMax.size()) {
		case 0:
			return null;
		case 1:
			Direction directionBetweenMinAndMax = null;
			for (Direction d : directionsBetweenMinAndMax) {
				directionBetweenMinAndMax = d;
			}
			return directionBetweenMinAndMax;
		default:
			throw new IllegalStateException("The number of pivotal directions is expected to be 0 or 1.");
		}
	}
	
	public Set<Configuration> getChildren() {
		Point biggestShare = findBiggestShare();
		Configuration newConfiguration = split(biggestShare);
		Direction pivotalDirection = newConfiguration.getPivotalDirection();
		Set<Configuration> children = new TreeSet<>();
		if (pivotalDirection == null) {
			children.add(newConfiguration);
		} else {
			Configuration newConfiguration1 = newConfiguration.clone();
			Configuration newConfiguration2 = newConfiguration.clone();
			newConfiguration1.min = pivotalDirection;
			newConfiguration2.max = pivotalDirection;
			children.add(newConfiguration1);
			children.add(newConfiguration2);
		}
		return children;
	}

	@Override
	public String toString() {
		return "Configuration [directionMin=" + min + ", directionMax=" + max
				+ ", mapPointMultiplicity=" + mapPointMultiplicity + "]";
	}

	@Override
	public int compareTo(Configuration that) {
		int d;
		d = this.min.compareTo(that.min); if (d != 0) { return d; }
		d = this.max.compareTo(that.max); if (d != 0) { return d; }
		d = this.mapPointMultiplicity.toString().compareTo(that.mapPointMultiplicity.toString()); if (d != 0) { return d; }
		return 0;
	}
	
	public String prettyPrint() {
		SortedMap<Integer, Set<Point>> mapScalarProductPoints = computeOrder();
		StringBuilder sb = new StringBuilder();
		boolean first = true;
		for (Map.Entry<Integer, Set<Point>> entry : mapScalarProductPoints.entrySet()) {
			if (first) {
				first = false;
			} else {
				sb.append(" > ");
			}
			Set<Point> value = entry.getValue();
			Point point = null;
			for (Point p : value) {
				point = p;
			}
			// sb.append("t^" + point.x + "*" + "(1-t)^" + point.y);
			sb.append(point);
		}
		return sb.toString();
	}
}
