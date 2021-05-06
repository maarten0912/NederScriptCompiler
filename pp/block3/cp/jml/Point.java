package pp.block3.cp.jml; 

/**
 * The point class given with exercise 2 of block 3.
 */
public class Point {

	/*@ spec_public */private int x = 0;
	/*@ spec_public */private int y = 1;
	//@ public invariant x != y;

	private final Object lockX = new Object();
	private final Object lockY = new Object();

	//@ ensures \result >= 0;
	/*@ pure */
	public int getX() {
		synchronized (this.lockX) {
			return this.x;
		}
	}

	//@ ensures \result >= 0;
	/*@ pure */
	public int getY() {
		synchronized (this.lockY) {
			return this.y;
		}
	}

	/*@ requires n >= 0;
        ensures getX() == \old(getX()) || getX() == \old(getX()) + n; */
	public void moveX(int n) {
		boolean b;
		synchronized (this.lockX) {
			synchronized (this.lockY) {
				b = (x + n != y);
			}
		}
		synchronized (this.lockX) {
			if (b) {
				this.x = this.x + n;
			}
		}
	}
	/*@ requires n >= 0;
	    ensures getY() == \old(getY()) || getY() == \old(getY() + n); */
	public void moveY(int n) {
		boolean b;
		synchronized (this.lockX) {
			synchronized (this.lockY) {
				b = (this.x != this.y + n);
			}
		}
		synchronized (lockY) {
			if (b) {
				this.y = this.y + n;
			}
		}
	}

}
