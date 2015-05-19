package rtree;


/**
 * Interface to represent a Minimum Bounding Region.
 * 
 * @author Joshua Belanger
 *
 * @param <Container>
 *            region of the same type. This forces enlargement to only be done
 *            against the same type of regions. The Comparable<R> instance
 *            should return the area/volume of the container
 */
public interface MBR<Container extends MBR<Container>> extends Comparable<Container> {

	boolean isIntersectedBy(Container other);

	int enlargement(Container toInclude);

	void enlarge(Container toInclude);
	
	double distanceBetween(Container other);

}
