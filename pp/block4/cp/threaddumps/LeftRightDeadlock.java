package  pp.block4.cp.threaddumps; 

import net.jcip.annotations.NotThreadSafe;

/**
 * Class which is deadlock prone!
 * WARNING! DEADLOCK PRONE!
 */
@NotThreadSafe
public class LeftRightDeadlock {

	private final Object left = new Object();
	private final Object right = new Object();

	/**
	 * Perform left right locking.
	 */
	public void leftRight() {
		synchronized (this.left) {
			synchronized (this.right) {
				System.out.println("Successfully obtained left-right lock.");
			}
		}
	}

	/**
	 * Perform right left locking.
	 */
	public void rightLeft() {
		synchronized (this.right) {
			synchronized (this.left) {
				System.out.println("Successfully obtained right-left lock.");
			}
		}
	}
}
