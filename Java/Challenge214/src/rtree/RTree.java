package rtree;

import java.util.Optional;
import java.util.stream.Stream;

public class RTree<Bounds extends Boundable<Bounds>, Value> {

	protected int min, max;
	private RootNode root;
	
	public RTree(int min, int max) {
		this.min = min;
		this.max = max;
		root = new RootNode(this);
	}

	public void insert(Value value, Bounds location) {
		root.insert(value, location);
	}

	public Stream<Value> search(Bounds query) {
		return root.search(query);
	}

	final class RootNode extends InnerNode<Bounds, Value> {

		public RootNode(RTree<Bounds, Value> tree) {
			super(tree);
		}

		@Override
		protected Optional<Node<Bounds, Value>> fizz(Node<Bounds, Value> bubbled) {
			if(root != this) {
				return super.fizz(bubbled);
			}
			root = new RootNode(tree);
			root.noBubbleInsert(this);
			root.noBubbleInsert(bubbled);
			return Optional.empty();
		}
	}
}

