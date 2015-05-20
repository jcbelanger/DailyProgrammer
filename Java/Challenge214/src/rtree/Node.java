package rtree;

import java.util.Optional;
import java.util.stream.Stream;

interface Node<Bounds extends Boundable<Bounds>, Value> {
	
	Bounds getBounds();

	Stream<Value> search(Bounds query);

	Optional<Node<Bounds, Value>> insert(Value value, Bounds location);

}