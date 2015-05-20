package rtree;

import java.util.Arrays;
import java.util.Comparator;
import java.util.HashMap;
import java.util.Map;
import java.util.Optional;
import java.util.function.Function;
import java.util.stream.Stream;


class LeafNode<Bounds extends Boundable<Bounds>, Value>  implements Node<Bounds, Value> {

	private RTree<Bounds, Value> tree;
	private Bounds bounds;
	private Map<Bounds, Value> entries;

	public LeafNode(RTree<Bounds, Value> tree) {
		this.tree = tree;
		this.bounds = null;
		this.entries = new HashMap<>();
	}

	@Override
	public Stream<Value> search(Bounds query) {
		if(bounds == null || !bounds.isIntersectedBy(query)) {
			return Stream.empty();
		} else {
			return entries.keySet()
				.stream()
				.filter(location -> location.isIntersectedBy(query))
				.map(entries::get);
		}
	}

	@Override
	public Optional<Node<Bounds, Value>> insert(Value value, Bounds location) {
		// TODO decide if we support overwrites
		entries.put(location, value);
		if (entries.size() <= tree.max) {
			enlarge(location);
			return Optional.empty();
		} else {
			return bubble();
		}
	}
	
	private void enlarge(Bounds toInclude) {
		if(bounds == null) {
			bounds = toInclude;
		} else {
			bounds.enlarge(toInclude);
		}
	}
	
	public Optional<Node<Bounds, Value>> bubble() {
		Bounds farthest1 = null, farthest2 = null;
		double maxDist = -1;
		for(Bounds location1 : entries.keySet()) {
			for(Bounds location2 : entries.keySet()) {
				double dist = location1.distanceBetween(location2);
				if(location1 != location2 && dist > maxDist) {
					farthest1 = location1;
					farthest2 = location2;
					maxDist = dist;
				}
			}
		}
		
		//save entries before the node is reset
		Map<Bounds, Value> remaining = entries;

		//reset this node
		entries = new HashMap<>();
		bounds = null;
		LeafNode<Bounds, Value> bubbled = new LeafNode<>(tree);

		//We will now accumulate entries between this node and one to be bubbled up
		this.insert(remaining.remove(farthest1), farthest1);
		bubbled.insert(remaining.remove(farthest2), farthest2);
		
		for(Bounds location : remaining.keySet()) {
			Comparator<Bounds> onElargement = Comparator.comparingDouble(c -> c.enlargement(location));
			Comparator<Bounds> optimalRegion = onElargement.thenComparing(Boundable::getSize);

			Comparator<Boolean> falseFirst = (a, b) -> a ^ b ? (a ? 1 : -1) : 0;
			
			Function<LeafNode<Bounds, Value>, Boolean> isMinMeetable = leaf -> {
				return leaf.entries.size() + remaining.size() > tree.min;
			};
			
			Comparator<LeafNode<Bounds, Value>> optimalNode = Comparator.comparing(isMinMeetable, falseFirst)
					.thenComparing(Comparator.comparing(Node::getBounds, optimalRegion))
					.thenComparingInt(leaf -> leaf.entries.size());

			LeafNode<Bounds, Value> best = Arrays.asList(this, bubbled).stream().min(optimalNode).get();
			best.insert(remaining.remove(location), location);
		}
		
		return Optional.of(bubbled);
	}

	@Override
	public Bounds getBounds() {
		return bounds;
	}
}