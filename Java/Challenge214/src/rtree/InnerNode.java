package rtree;

import java.util.Comparator;
import java.util.HashSet;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Stream;


class InnerNode<Bounds extends Boundable<Bounds>, Value> implements Node<Bounds, Value> {

	private RTree<Bounds, Value> tree;
	private Set<Node<Bounds, Value>> children;
	private Bounds bounds;

	public InnerNode(RTree<Bounds, Value> tree) {
		this.tree = tree;
		this.bounds = null;
		this.children = new HashSet<>();
	}

	@Override
	public Optional<Node<Bounds, Value>> insert(Value value, Bounds location) {
		enlarge(location);
		
		if(children.size() < tree.max) {
			LeafNode<Bounds, Value> leaf = new LeafNode<>(tree);
			leaf.insert(value, location);
			children.add(leaf);
			return Optional.empty();
		}
		
		Comparator<Bounds> onElargement = Comparator.comparingDouble(c -> c.enlargement(location));
		Comparator<Bounds> onSize = Comparator.comparingDouble(Boundable::getSize);
		Comparator<Bounds> optimalBounds = onElargement.thenComparing(onSize);
		
		return children
			.stream()
			.min(Comparator.comparing(Node::getBounds, optimalBounds))
			.flatMap(child -> child.insert(value, location))
			.flatMap(this::fizz);
	}

	protected void enlarge(Bounds toInclude) {
		if(bounds == null) {
			bounds = toInclude;
		} else {
			bounds.enlarge(toInclude);
		}
	}
	
	private Optional<Node<Bounds, Value>> fizz(Node<Bounds, Value> bubbled) {
		if(children.size() <= tree.max) {
			return Optional.empty();
		}
		//TODO implement split
		return Optional.empty();
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
}