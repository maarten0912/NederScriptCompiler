package pp.block2.cp.annotation;

import net.jcip.annotations.NotThreadSafe;

/**
 * Class which implements the RandomDrift as given in the exercises, 
 * but uses a {@link Runnable} rather than a {@link Thread} implementation 
 * to make it testable using the JUnit concurrent test runner.
 */
@NotThreadSafe
public class RandomDrift implements Runnable {

    /**
     * The point used by this RandomDrift object.
     */
    private final Point point;

    /**
     * Amount of drifts to perform.
     */
    private final int amount;

    public RandomDrift(Point point, int amount) {
        this.point = point;
        this.amount = amount;
    }

    @Override
	public void run() {
        for (int i = 0; i < this.amount; i++) {
            int n = (int) (Math.random () * 10);
            this.point.moveX(n);
            int m = (int) (Math.random () * 10);
            this.point.moveY(m);
        }
    }
}
