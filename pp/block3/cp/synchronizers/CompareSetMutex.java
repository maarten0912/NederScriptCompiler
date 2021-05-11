package pp.block3.cp.synchronizers;

import java.util.concurrent.atomic.AtomicInteger;

public class CompareSetMutex implements BasicLock{

    AtomicInteger holdCount;
    AtomicInteger ownerThread;

    public CompareSetMutex() {
        holdCount = new AtomicInteger();
        holdCount.set(0);
        ownerThread = new AtomicInteger();
        ownerThread.set(-1);
    }

    @Override
    public void lock(int threadNumber) {
        if (ownerThread.get() == threadNumber) {
            holdCount.incrementAndGet();
        } else {
            while (!ownerThread.compareAndSet(-1,threadNumber)) {
                try {
                    Thread.sleep(10);
                } catch (InterruptedException e) {
                    e.printStackTrace();
                }
            }
            holdCount.incrementAndGet();
            ownerThread.set(threadNumber);
        }
    }

    @Override
    public void unlock(int threadNumber) {
        if (ownerThread.get() == threadNumber && holdCount.decrementAndGet() == 0) {
            ownerThread.set(-1);
        }
    }
}
