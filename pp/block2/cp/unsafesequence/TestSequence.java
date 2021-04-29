package pp.block2.cp.unsafesequence;

import org.junit.Test;

import static org.junit.Assert.assertEquals;

/**
 * Interface to support multiple implementations of a Sequence.
 */
public class TestSequence {

    Sequence seq;

    @Test
    public void UnsafeSequence() {
        seq = new UnsafeSequence();
        checkConcurrency();
    }

    @Test
    public void SynchronizedSequence() {
        seq = new SynchronizedSequence();
        checkConcurrency();
    }

    @Test
    public void LockSequence() {
        seq = new LockSequence();
        checkConcurrency();
    }

    @Test
    public void VolatileSequence() {
        seq = new VolatileSequence();
        checkConcurrency();
    }


    public void checkConcurrency() {
        SequenceThread t1 = new SequenceThread();
        SequenceThread t2 = new SequenceThread();
        t1.start();
        t2.start();

        try {
            t1.join();
            t2.join();
        } catch(InterruptedException e) {
            e.printStackTrace();
        }

        assertEquals(200000, seq.getNext());
    }

    private class SequenceThread extends Thread {
        @Override
        public void run() {
            for (int i = 0; i < 100000; i++) seq.getNext();
        }
    }

}
