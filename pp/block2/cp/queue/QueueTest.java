package pp.block2.cp.queue;

import org.junit.Test;
import pp.block2.cp.unsafesequence.VolatileSequence;

import static org.junit.Assert.assertEquals;

/**f
 * Interface to support multiple implementations of a Queue.
 */
public class QueueTest {

    Queue q;

    @Test
    public void CoarseGrainedLinkedListQueue() {
        q = new CoarseGrainedLinkedListQueue();
        checkConcurrency();
    }

    public void checkConcurrency() {
        QueueThread t1 = new QueueThread();
        QueueThread t2 = new QueueThread();
        t1.start();
        t2.start();

        try {
            t1.join();
            t2.join();
        } catch(InterruptedException e) {
            e.printStackTrace();
        }

        assertEquals(0, q.getLength());
    }

    private class QueueThread extends Thread {
        @Override
        public void run() {
            for (int i = 0; i < 100000; i++)
                q.push(new Object());
            try {
                for (int i = 0; i < 100000; i++)
                    q.pull();
            } catch (QueueEmptyException e) {
                e.printStackTrace();
            }
        }
    }

}
