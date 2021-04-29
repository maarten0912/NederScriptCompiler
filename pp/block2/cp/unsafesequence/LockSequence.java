package pp.block2.cp.unsafesequence;

import net.jcip.annotations.NotThreadSafe;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReadWriteLock;
import java.util.concurrent.locks.ReentrantReadWriteLock;

@NotThreadSafe
public class LockSequence implements Sequence {
    private int value;
    ReadWriteLock lock = new ReentrantReadWriteLock();

    /**
     * Returns a unique value.
     */
    public int getNext() {
        lock.writeLock().lock();
        try {
            return value++;
        } finally {
            lock.writeLock().unlock();
        }
    }
}