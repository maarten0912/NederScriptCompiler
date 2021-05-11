package pp.block3.cp.synchronizers;

import java.util.concurrent.atomic.AtomicIntegerArray;

public class DekkersBasicLock implements BasicLock{

    AtomicIntegerArray wantsToEnter;
    Integer turn;

    public DekkersBasicLock() {
        wantsToEnter = new AtomicIntegerArray(2);
        wantsToEnter.set(0,0);
        wantsToEnter.set(1,0);
        turn = 0;
    }

    @Override
    public void lock(int threadNumber) {
        wantsToEnter.set(threadNumber, 1);
        while (wantsToEnter.get(1 - threadNumber) == 1) {
            if (turn != threadNumber) {
                wantsToEnter.set(threadNumber, 0);
                while (turn != threadNumber) {
                    try {
                        Thread.sleep(10);
                    } catch (InterruptedException e) {
                        e.printStackTrace();
                    }
                }
                wantsToEnter.set(threadNumber, 1);
            }
        }
    }

    @Override
    public void unlock(int threadNumber) {
        turn = 1 - threadNumber;
        wantsToEnter.set(threadNumber, 0);
    }
}
