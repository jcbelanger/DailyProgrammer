package rtree;

import java.util.Arrays;
import java.util.Comparator;
import java.util.HashMap;
import java.util.Iterator;
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
		reset();
	}

	@Override
	public Stream<Value> search(Bounds query) {
		if(bounds == null || !bounds.isIntersectedBy(query)) {
			return Stream.empty();
		} else {
			return entries.keySet()
				.stream()
				.filter(entryBounds -> entryBounds.isIntersectedBy(query))
				.map(entries::get);
		}
	}

	@Override
	public Optional<Node<Bounds, Value>> insert(Value value, Bounds location) {
		if (size() < tree.max) {
			return noBubbleInsert(value, location);
		} else {
			return bubbleInsert(value, location);
		}
	}
	
	protected Optional<Node<Bounds, Value>> noBubbleInsert(Value value, Bounds location) {
		// TODO decide if we support overwrites
		entries.put(location, value);
		if(bounds == null) {
			bounds = location.clone();
		} else {
			bounds.enlarge(location);
		}
		return Optional.empty();
	}
	
	protected Optional<Node<Bounds, Value>> bubbleInsert(Value value, Bounds location) {
		entries.put(location, value);
		
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
		reset();
		LeafNode<Bounds, Value> bubbled = new LeafNode<>(tree);

		//We will now accumulate entries between this node and one to be bubbled up
		this.insert(remaining.remove(farthest1), farthest1);
		bubbled.insert(remaining.remove(farthest2), farthest2);
		
		for(Iterator<Bounds> iter = remaining.keySet().iterator(); iter.hasNext();) {
			Bounds bounds = iter.next();
			Value entry = remaining.get(bounds);
			
			Comparator<Bounds> onElargement = Comparator.comparingDouble(b -> b.enlargement(bounds));
			Comparator<Bounds> optimalBounds = onElargement.thenComparing(Boundable::getSize);

			Comparator<Boolean> falseFirst = (a, b) -> a ^ b ? (a ? 1 : -1) : 0;
			
			Function<LeafNode<Bounds, Value>, Boolean> isMinMeetable = leaf -> {
				return leaf.size() + remaining.size() > tree.min;
			};
			
			Comparator<LeafNode<Bounds, Value>> optimalNode = Comparator.comparing(isMinMeetable, falseFirst)
					.thenComparing(Comparator.comparing(Node::getBounds, optimalBounds))
					.thenComparingInt(leaf -> leaf.entries.size());

			LeafNode<Bounds, Value> best = Arrays.asList(this, bubbled).stream().min(optimalNode).get();
			best.insert(entry, bounds);
			iter.remove();
		}
		
		return Optional.of(bubbled);
	}

	private void reset() {
		entries = new HashMap<>();
		bounds = null;
	}

	@Override
	public Bounds getBounds() {
		return bounds;
	}

	@Override
	public int size() {
		return entries.size();
	}
}