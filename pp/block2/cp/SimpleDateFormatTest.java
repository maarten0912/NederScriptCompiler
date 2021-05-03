package pp.block2.cp;

import nl.utwente.pp.cp.junit.ConcurrentRunner;
import nl.utwente.pp.cp.junit.ThreadNumber;
import nl.utwente.pp.cp.junit.Threaded;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import pp.block2.cp.queue.ConcurrentQueue;
import pp.block2.cp.queue.Queue;
import pp.block2.cp.queue.QueueEmptyException;

import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Random;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.fail;


@RunWith(ConcurrentRunner.class)
public class SimpleDateFormatTest {

    /**
     * The amount of threads used in each test.
     */
    private static final int AMOUNT_OF_THREADS = 10;

    /**
     * Simple random object to get random integers from.
     */
    private final Random random = new Random();

    private SimpleDateFormat sdf;

    @Before
    public void before() {
        //Setup an empty queue.
        this.sdf = new SimpleDateFormat("yyyy");
    }

    @Test
    @Threaded(count = AMOUNT_OF_THREADS)
    public void simpleMultiThreadedTest() {
        for (int i = 0; i < 100; i++) {
            int randomYear = random.ints(1000, 9999).findFirst().getAsInt();
            try {
                sdf.parse(String.valueOf(randomYear));
            } catch (Exception e) {
                fail("Should not throw exception: " + e.toString() + ", randomYear was " + randomYear);
            }
        }
    }
}
