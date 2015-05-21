package rtree;


public class Rectangle implements Boundable<Rectangle> {

	protected double x1, y1, x2, y2;

	public Rectangle(double x1, double y1, double x2, double y2) {
		this.x1 = Math.min(x1, x2);
		this.y1 = Math.min(y1, y2);
		this.x2 = Math.max(x1, x2);
		this.y2 = Math.max(y1, y2);
	}
	
	@Override
	public double getSize() {
		return (x2 - x1) * (y2 - y1);
	}
	
	@Override
	public boolean isIntersectedBy(Rectangle other) {
		return
			this.x1 <= other.x2 &&
			this.x2 >= other.x1 && 
			this.y1 <= other.y2 &&
			this.y2 >= other.y1;
	}

	@Override
	public void enlarge(Rectangle toInclude) {
		x1 = Math.min(x1, toInclude.x1);
		y1 = Math.min(y1, toInclude.y1);
		x2 = Math.max(x2, toInclude.x2);
		y2 = Math.max(y2, toInclude.y2);
	}
	
	@Override
	public double distanceBetween(Rectangle other) {
		double dx = this.x1 - other.x1;
		double dy = this.y1 - other.y1;
		return dx*dx + dy*dy;
	}
	
	@Override
	public String toString() {
		return "Rect[" + x1 + "," + y1 + "," + x2 + "," + y2 +"]";
	}

	@Override
	public Rectangle clone() {
		return new Rectangle(x1, y1, x2, y2);
	}

}