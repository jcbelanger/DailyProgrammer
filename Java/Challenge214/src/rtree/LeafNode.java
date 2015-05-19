package rtree;

import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.Map;
import java.util.stream.Collectors;


class LeafNode<Container extends MBR<Container>, Value>  implements Node<Container, Value> {

	private RTree<Container, Value> tree;
	private Container region;
	private InnerNode<Container, Value> parent;
	private Map<Container, Value> entries;

	public LeafNode(RTree<Container, Value> tree, InnerNode<Container, Value> parent) {
		this.tree = tree;
		this.parent = parent;
		this.region = null;
		this.entries = new HashMap<>();
	}

	@Override
	public InnerNode<Container, Value> getParent() {
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
		if (entries.size() <= tree.max) {
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
		LeafNode<Container, Value> leaf1 = new LeafNode<>(tree, parent);
		LeafNode<Container, Value> leaf2 = new LeafNode<>(tree, parent);
		leaf1.insert(remaining.remove(farthest1), farthest1);
		leaf2.insert(remaining.remove(farthest2), farthest2);
		
		for(Container location : remaining.keySet()) {
			double remain = entries.size();
			
			Comparator<LeafNode<Container, Value>> meetMin = (a,b) ->
				Boolean.compare(a.entries.size() + remain < tree.min,
								b.entries.size() + remain < tree.min);
			Comparator<LeafNode<Container, Value>> comparingEnlargement = (a, b) -> 
				Integer.compare(a.region.enlargement(location),
								b.region.enlargement(location));
			Comparator<LeafNode<Container, Value>> comparingCount = (a,b) -> 
				Integer.compare(a.entries.size(),
								b.entries.size());
			Comparator<LeafNode<Container, Value>> comparingArea = (a,b) -> 
				a.region.compareTo(b.region);
			
			//TODO use utility to dry out comparison fallbacks
			Comparator<LeafNode<Container, Value>> comparingBest = (a, b) -> {
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
			

			LeafNode<Container, Value> best = Arrays.asList(leaf1, leaf2)
				.stream()
				.min(comparingBest)
				.get();
			best.insert(remaining.get(location), location);
		}

		getParent().children.remove(this.region);
		getParent().children.put(leaf1.region, leaf1);
		getParent().children.put(leaf2.region, leaf2);
		if(getParent().children.size() >= tree.max) {
			getParent().split();	
		}
	}
}