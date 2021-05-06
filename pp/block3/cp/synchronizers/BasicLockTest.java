package pp.block3.cp.synchronizers; 

import java.util.Collections;
import java.util.HashSet;
import java.util.Set;

import org.junit.After;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;

import net.jcip.annotations.ThreadSafe;
import nl.utwente.pp.cp.junit.ConcurrentRunner;
import nl.utwente.pp.cp.junit.ThreadNumber;
import nl.utwente.pp.cp.junit.Threaded;
import pp.block2.cp.unsafesequence.UnsafeSequence;

/**
 * Class for testing implementations of BasicLock
 */
@ThreadSafe
@RunWith(ConcurrentRunner.class)
public class BasicLockTest {

    /**
     * The amount of calls made per thread.
     */
    private static final int CALLS_PER_THREAD = 10000;

    /**
     * The sequence object to use for the tests. 
     * Should not be thread safe 
     * to test the {@link BasicLock} performance.
     */
    private UnsafeSequence sequence;

    /**
     * The set with consumed values from the sequence.
     */
    private final Set<Integer> numbers =
	Collections.synchronizedSet(new HashSet<>());

    /*
    TODO replace null by a constructor call to your
    BasicLock implementation
    */
    private final BasicLock lock = null;

    @Before
    public void before() {
        // TODO: Replace declaration and initialization 
        // with your (unsafe!) sequence from block 2. 
        this.sequence = new UnsafeSequence();
        this.numbers.clear();
    }

    private void testLock(int threadNumber, BasicLock lock) {
        Set<Integer> numbers = new HashSet<>();
        for (int i = 0; i < CALLS_PER_THREAD; i++) {
            lock.lock(threadNumber);
            numbers.add(this.sequence.getNext());
            lock.unlock(threadNumber);
        }
        this.numbers.addAll(numbers);
    }

    @Test
    @Threaded(count = 2)
    public void test(
    		@ThreadNumber int threadNumber)	throws Exception {
        this.testLock(threadNumber, this.lock);
    }

    @After
    public void after() {
        for (int i = 0; i < 2 * CALLS_PER_THREAD; i++) {
            Assert.assertTrue
		("Expects number " + i + " to be present among the results.",
		 this.numbers.contains(i));
        }
    }
}
