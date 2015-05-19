package challenge214;

import rtree.RTree;
import rtree.Rectangle;

public class Main {

	public static void main(String[] args) {
		RTree<Rectangle, Rectangle> tree = new RTree<>(5, 10);
		for(int x = 0; x < 5; x++) {
			for(int y = 0; y < 5; y++) {
				Rectangle location = new Rectangle(x, y, x, y);
				Rectangle value = new Rectangle(x, y, x, y);
				tree.insert(value, location);	
			}	
		}
		System.out.println(tree);
	}

}
