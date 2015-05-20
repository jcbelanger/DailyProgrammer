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
		RTree<Rectangle, String> tree = new RTree<>(5, 50);
		for(int x = 0; x < 5; x++) {
			for(int y = 0; y < 5; y++) {
				Rectangle location = new Rectangle(x, y, x, y);
				String value = "(" + x + ", " + y + ")";
				tree.insert(value, location);	
			}	
		}
		tree.search(new Rectangle(0, 0, 5, 5)).forEach(System.out::println);
	}

}
