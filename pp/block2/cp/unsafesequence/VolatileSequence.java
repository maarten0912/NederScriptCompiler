package pp.block2.cp.unsafesequence;

import net.jcip.annotations.NotThreadSafe;

@NotThreadSafe
public class VolatileSequence implements Sequence {
    private volatile int value;
    
    public int getNext() {
        return value++;
    }
}