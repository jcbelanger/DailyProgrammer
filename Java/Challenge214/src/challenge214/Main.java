package challenge214;

import rtree.RTree;
import rtree.Rectangle;

/**
 * Solution to Hard Challenge 214
 * http://www.reddit.com/r/dailyprogrammer/comments/3629st/20150515_challenge_214_hard_chester_the_greedy/
 * 
 * @author Joshua Belanger
 *
 */
public class Main {

	public static void main(String[] args) {
		//Test 5x5 grid
		RTree<Rectangle, String> tree = new RTree<>(20, 100);
		for(int x = 0; x < 1000; x++) {
			for(int y = 0; y < 1000; y++) {
				//Simulate (x,y) point with rectangle until I add another Boundable instance
				Rectangle location = new Rectangle(x, y, x, y);
				String value = "(" + x + ", " + y + ")";
				tree.insert(value, location);
			}
		}
		Rectangle query = new Rectangle(50,50,55,55);
		tree.search(query).forEach(System.out::println);
	}

}
