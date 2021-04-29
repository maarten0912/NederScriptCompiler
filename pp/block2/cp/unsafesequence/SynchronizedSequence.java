package pp.block2.cp.unsafesequence;

import net.jcip.annotations.NotThreadSafe;

@NotThreadSafe
public class SynchronizedSequence implements Sequence {
    private int value;

    /**
     * Returns a unique value.
     */
    public int getNext() {
        synchronized(this) {
            return value++;
        }
    }
}