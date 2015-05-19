package rtree;

import java.util.Collection;

public class RTree<Container extends MBR<Container>, Value>  {

	protected int min, max;
	protected InnerNode<Container, Value> root;

	public RTree(int min, int max) {
		this.min = min;
		this.max = max;
		root = new InnerNode<>(this, null);
	}

	public Collection<Value> search(Container query) {
		return root.search(query);
	}

	public void insert(Value value, Container location) {
		root.insert(value, location);
	}
	
}

