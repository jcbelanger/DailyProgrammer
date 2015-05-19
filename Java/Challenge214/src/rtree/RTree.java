package rtree;

import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.Map;
import java.util.stream.Collectors;

public class RTree<Container extends MBR<Container>, Value>  {

	private int min, max;
	private InnerNode root;

	public RTree(int min, int max) {
		this.min = min;
		this.max = max;
		root = new InnerNode(null);
	}

	public Collection<Value> search(Container query) {
		return root.search(query);
	}

	public void insert(Value value, Container location) {
		root.insert(value, location);
	}
	
	class InnerNode implements Node<Container, Value> {

		private InnerNode parent;
		private Map<Container, Node<Container, Value>> children;

		public InnerNode(InnerNode parent) {
			this.parent = parent;
			this.children = new HashMap<>();
		}

		@Override
		public InnerNode getParent() {
			return parent;
		}

		@Override
		public Collection<Value> search(Container query) {
			return children.keySet()
				.stream()
				.filter(region -> region.isIntersectedBy(query))
				.map(children::get)
				.flatMap(node -> node.search(query).stream())
				.collect(Collectors.toList());
		}

		@Override
		public void insert(Value value, Container location) {
			if(children.size() == 0) {
				children.put(location, new LeafNode(this));
			}
			
			Comparator<Container> comparingEnlargement = (a, b) -> 
				Integer.compare(a.enlargement(location), b.enlargement(location));
				
			Comparator<Container> comparingArea = Container::compareTo;

			// TODO create general comparator fallback combinator
			// ^needed in split where comparison order is:
			// under min by x and x remain -> enlargement -> area -> count
			Comparator<Container> comparingEnlrgThenArea = (a, b) -> {
				int firstCompare = comparingEnlargement.compare(a, b);
				if(firstCompare != 0) {
					return firstCompare;
				}
				return comparingArea.compare(a, b);
			};

			// never below min, so always have at least 1 element
			Container bestRegion = children.keySet()
					.stream()
					.min(comparingEnlrgThenArea)
					.get();

			bestRegion.enlarge(location);
			children.get(bestRegion).insert(value, location);
		}

		@Override
		public void split() {
			// TODO implement
		}
	}

	class LeafNode implements Node<Container, Value> {

		private Container region;
		private InnerNode parent;
		private Map<Container, Value> entries;

		public LeafNode(InnerNode parent) {
			this.parent = parent;
			this.region = null;
			this.entries = new HashMap<>();
		}

		@Override
		public InnerNode getParent() {
			return parent;
		}

		@Override
		public Collection<Value> search(Container query) {
			if(region == null) {
				return Collections.emptyList();
			} else {
				return entries.keySet()
					.stream()
					.filter(location -> query.isIntersectedBy(location))
					.map(entries::get)
					.collect(Collectors.toList());
			}
		}

		@Override
		public void insert(Value value, Container location) {
			// TODO decide if allow overwrites
			entries.put(location, value);
			if (entries.size() <= max) {
				enlarge(location);
			} else {
				split();	
			}
		}
		
		private void enlarge(Container toInclude) {
			if(region == null) {
				region = toInclude;
			} else {
				region.enlarge(toInclude);
			}
		}
		
		@Override 
		public void split() {
			//TODO use quadtree to avoid O(n^2) farthest points lookup
			Container farthest1 = null, farthest2 = null;
			double maxDist = -1;
			for(Container location1 : entries.keySet()) {
				for(Container location2 : entries.keySet()) {
					double dist = location1.distanceBetween(location2);
					if(location1 != location2 && dist > maxDist) {
						farthest1 = location1;
						farthest2 = location2;
						maxDist = dist;
					}
				}
			}
			
			Map<Container, Value> remaining = entries;			
			LeafNode leaf1 = new LeafNode(parent);
			LeafNode leaf2 = new LeafNode(parent);
			leaf1.insert(remaining.remove(farthest1), farthest1);
			leaf2.insert(remaining.remove(farthest2), farthest2);
			
			for(Container location : remaining.keySet()) {
				double remain = entries.size();
				
				Comparator<LeafNode> meetMin = (a,b) ->
					Boolean.compare(a.entries.size() + remain < min,
									b.entries.size() + remain < min);
				Comparator<LeafNode> comparingEnlargement = (a, b) -> 
					Integer.compare(a.region.enlargement(location),
									b.region.enlargement(location));
				Comparator<LeafNode> comparingCount = (a,b) -> 
					Integer.compare(a.entries.size(),
									b.entries.size());
				Comparator<LeafNode> comparingArea = (a,b) -> a.region.compareTo(b.region);
				
				//TODO use utility to dry out comparison fallbacks
				Comparator<LeafNode> comparingBest = (a, b) -> {
					int first = meetMin.compare(a, b);
					if(first != 0) {
						return first;
					}
					int second = comparingEnlargement.compare(a, b);
					if(second != 0) {
						return second;
					}
					int third = comparingArea.compare(a, b);
					if(third != 0) {
						return third;
					}
					return comparingCount.compare(a, b);
				};
				

				LeafNode best = Arrays.asList(leaf1, leaf2)
					.stream()
					.min(comparingBest)
					.get();
				best.insert(remaining.get(location), location);
			}

			getParent().children.remove(this.region);
			getParent().children.put(leaf1.region, leaf1);
			getParent().children.put(leaf2.region, leaf2);
			if(getParent().children.size() >= max) {
				getParent().split();	
			}
		}
		
	}

}


interface Node<Container extends MBR<Container>, Value> {
	
	RTree<Container, Value>.InnerNode getParent();

	Collection<Value> search(Container query);

	void insert(Value value, Container location);
	
	void split();
}