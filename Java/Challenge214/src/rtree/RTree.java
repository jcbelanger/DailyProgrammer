package rtree;

import java.util.stream.Stream;

public class RTree<Bounds extends Boundable<Bounds>, Value>  {

	protected int min, max;
	protected Node<Bounds, Value> root;

	public RTree(int min, int max) {
		this.min = min;
		this.max = max;
		root = new InnerNode<>(this);
	}
	
	public Stream<Value> search(Bounds query) {
		return root.search(query);
	}

	public void insert(Value value, Bounds location) {
		//TODO handle root bubbling
		root.insert(value, location);
	}
	
}

