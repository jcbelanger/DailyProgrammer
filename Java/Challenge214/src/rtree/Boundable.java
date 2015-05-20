package rtree;


/**
 * Interface to represent a Minimum Bounding Region.
 * 
 * @author Joshua Belanger
 *
 * @param <Bounds>
 *            region of the same type. This forces enlargement to only be done
 *            against the same type of regions.
 */
public interface Boundable<Bounds extends Boundable<Bounds>> {

	boolean isIntersectedBy(Bounds other);

	double enlargement(Bounds toInclude);

	void enlarge(Bounds toInclude);
	
	double distanceBetween(Bounds other);
	
	double getSize();

}
