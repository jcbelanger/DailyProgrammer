package rtree;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Comparator;
import java.util.Iterator;
import java.util.Optional;
import java.util.function.Function;
import java.util.stream.Stream;


class InnerNode<Bounds extends Boundable<Bounds>, Value> implements Node<Bounds, Value> {

	protected RTree<Bounds, Value> tree;
	protected Collection<Node<Bounds, Value>> children;
	protected Bounds bounds;
	
	public InnerNode(RTree<Bounds, Value> tree) {
		this.tree = tree;
		reset();
	}

	private void reset() {
		children = new ArrayList<>();
		bounds = null;
	}

	@Override
	public int size() {
		return children.size();
	}

	@Override
	public Bounds getBounds() {
		return bounds;
	}

	@Override
	public Stream<Value> search(Bounds query) {
		if(bounds == null || !bounds.isIntersectedBy(query)) {
			return Stream.empty();
		} else {
			return children.stream().flatMap(child -> child.search(query));
		}
	}

	@Override
	public Optional<Node<Bounds, Value>> insert(Value value, Bounds location) {
		if(size() < tree.max) {
			return noBubbleInsert(value, location);
		} else {
			return bubbleInsert(value, location);	
		}
	}

	private Optional<Node<Bounds, Value>> bubbleInsert(Value value, Bounds location) {
		//TODO only enlarge if insert doesn't bubble
		bounds.enlarge(location);
		
		Comparator<Bounds> onElargement = Comparator.comparingDouble(c -> c.enlargement(location));
		Comparator<Bounds> onSize = Comparator.comparingDouble(Boundable::getSize);
		Comparator<Bounds> optimalBounds = onElargement.thenComparing(onSize);
		
		return children
			.stream()
			.min(Comparator.comparing(Node::getBounds, optimalBounds))
			.flatMap(child -> child.insert(value, location))
			.flatMap(this::fizz);
	}
	
	protected Optional<Node<Bounds, Value>> noBubbleInsert(Value value, Bounds location) {
		LeafNode<Bounds, Value> leaf = new LeafNode<>(tree);
		leaf.insert(value, location);
		return noBubbleInsert(leaf);
	}

	protected Optional<Node<Bounds, Value>> noBubbleInsert(Node<Bounds, Value> node) {
		children.add(node);
		if(bounds == null) {
			bounds = node.getBounds().clone();
		} else {
			bounds.enlarge(node.getBounds());
		}
		return Optional.empty();
	}
	
	protected Optional<Node<Bounds, Value>> fizz(Node<Bounds, Value> bubbled) {
		children.add(bubbled);
		if(children.size() <= tree.max) {
			return Optional.empty();
		} else {
			return bubble();
		}
	}
	
	protected Optional<Node<Bounds, Value>> bubble() {
		//TODO dry out farthest apart code -- I wish java had tuples!
		Node<Bounds, Value> farthest1 = null, farthest2 = null;
		double maxDist = -1;
		for(Node<Bounds, Value> child1 : children) {
			Bounds location1 = child1.getBounds();
			for(Node<Bounds, Value> child2 : children) {
				Bounds location2 = child2.getBounds();
				double dist = location1.distanceBetween(location2);
				if(location1 != location2 && dist > maxDist) {
					farthest1 = child1;
					farthest2 = child2;
					maxDist = dist;
				}
			}
		}
		
		//save entries before the node is reset
		Collection<Node<Bounds, Value>> remaining = children;
		reset();
		InnerNode<Bounds, Value> bubbled = new InnerNode<>(tree);

		//We will now accumulate entries between this node and one to be bubbled up
		remaining.remove(farthest1);
		remaining.remove(farthest2);
		
		this.noBubbleInsert(farthest1);
		bubbled.noBubbleInsert(farthest2);
		
		for(Iterator<Node<Bounds, Value>> iter = remaining.iterator(); iter.hasNext();) {
			Node<Bounds, Value> child = iter.next();
			
			Comparator<Bounds> onElargement = Comparator.comparingDouble(b -> b.enlargement(child.getBounds()));
			Comparator<Bounds> optimalBounds = onElargement.thenComparing(Boundable::getSize);

			//TODO implement min size checking
			Comparator<Boolean> falseFirst = (a, b) -> a ^ b ? (a ? 1 : -1) : 0;
			Function<InnerNode<Bounds, Value>, Boolean> isMinMeetable = node -> {
				return node.size() + remaining.size() > tree.min;
			};
			Comparator<InnerNode<Bounds, Value>> onMeetsMin = Comparator.comparing(isMinMeetable, falseFirst);

			Comparator<InnerNode<Bounds, Value>> optimalNode = onMeetsMin.thenComparing(Node::getBounds, optimalBounds);

			InnerNode<Bounds, Value> best = Arrays.asList(this, bubbled).stream().min(optimalNode).get();
			best.noBubbleInsert(child);
			iter.remove();
		}
		
		return Optional.of(bubbled);
	}

}