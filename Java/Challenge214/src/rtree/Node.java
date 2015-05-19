package rtree;

import java.util.Collection;

interface Node<Container extends MBR<Container>, Value> {
	
	InnerNode<Container, Value> getParent();

	Collection<Value> search(Container query);

	void insert(Value value, Container location);
	
	void split();
}