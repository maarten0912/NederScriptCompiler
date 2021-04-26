package pp.block2.cp.annotation;

import nl.utwente.pp.cp.junit.ConcurrentRunner;
import nl.utwente.pp.cp.junit.ThreadNumber;
import nl.utwente.pp.cp.junit.Threaded;
import org.junit.Test;
import org.junit.runner.RunWith;

/**
 * Simple test that runs two instances of RandomDrift concurrently on the same point.
 */
@RunWith(ConcurrentRunner.class)
public class RandomPointTest {
	/** The number of threads to use for this test. */
	private static final int THREAD_COUNT = 2;

	/** The number of calls made per thread. */
	private static final int CHANGES_PER_THREAD = 10000;

	/** The point that will be used by the RandomDrift threads. */
	private final Point point = new Point();

	/** The first RandomDrift instance. */
	private final RandomDrift randomDrift1 = new RandomDrift(point, CHANGES_PER_THREAD);

	/** The second RandomDrift instance. */
	private final RandomDrift randomDrift2 = new RandomDrift(point, CHANGES_PER_THREAD);

	@Test
	@Threaded(count = THREAD_COUNT)
	public void randomDriftTest(@ThreadNumber int number) {
		if (number % 2 == 0) {
			this.randomDrift1.run();
		} else {
			this.randomDrift2.run();
		}
	}

}
