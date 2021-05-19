package pp.block4.cp.threaddumps;

import nl.utwente.pp.cp.junit.ConcurrentRunner;
import nl.utwente.pp.cp.junit.Threaded;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;

@RunWith(ConcurrentRunner.class)
public class LeftRightDeadlockTest {

	private LeftRightDeadlock lrd;

	private static final int AMOUNT_OF_THREADS = 10;

	@Before
	public void before() {
		lrd = new LeftRightDeadlock();
	}

	@Test
	@Threaded(count = AMOUNT_OF_THREADS)
	public void simpleMultiThreadedTest() throws InterruptedException {
		for (int i = 0; i < 100; i++) lrd.leftRight();
		for (int i = 0; i < 100; i++) lrd.rightLeft();
	}
}
