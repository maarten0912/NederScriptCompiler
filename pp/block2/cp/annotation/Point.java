package pp.block2.cp.annotation; 

import net.jcip.annotations.NotThreadSafe;

/**
 * Simple point class as given for the exercise.
 */
@NotThreadSafe
public class Point {
    /*@ spec_public */
    private int x;

    /*@ spec_public */
    private int y;

    //@ ensures \result >= 0;
    /*@ pure */
    public int getX () {
        return this.x;
    }

    //@ ensures \result >= 0;
    /*@ pure */
    public int getY () {
        return this.y;
    }

    /*@
    requires n >= 0;
    ensures getX() == \old(getX()) + n;
    */
    public void moveX(int n) {
        this.x = this.x + n;
    }

    /*@
    requires n >= 0;
    ensures getY() == \old(getY()) + n;
    */
    public void moveY(int n) {
        this.y = this.y + n;
    }
}
