package rtree;

import java.util.Collection;
import java.util.Comparator;
import java.util.HashMap;
import java.util.Map;
import java.util.stream.Collectors;


class InnerNode<Container extends MBR<Container>, Value> implements Node<Container, Value> {

	private RTree<Container, Value> tree;
	private InnerNode<Container, Value> parent;
	protected Map<Container, Node<Container, Value>> children;

	public InnerNode(RTree<Container, Value> tree, InnerNode<Container, Value> parent) {
		this.tree = tree;
		this.parent = parent;
		this.children = new HashMap<>();
	}

	@Override
	public InnerNode<Container, Value> getParent() {
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
			children.put(location, new LeafNode<>(tree, this));
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