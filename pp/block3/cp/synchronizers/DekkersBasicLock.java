package pp.block3.cp.synchronizers;

import java.util.concurrent.atomic.AtomicIntegerArray;

/**
 * BasicLock interface which can lock with 2 threads based on thread number.
 */
public class DekkersBasicLock implements BasicLock {

	AtomicIntegerArray wantsToEnter;
	volatile Integer turn;

	public DekkersBasicLock() {
		wantsToEnter = new AtomicIntegerArray(2);
		wantsToEnter.set(0,0);
		wantsToEnter.set(1,0);
		turn = 0;
	}

	/**
	 * Acquires the lock.
	 * @param threadNumber is the number of the requesting thread, 
	 * threadNumber == 0|1
	 */
	public void lock(int threadNumber) {
		wantsToEnter.set(threadNumber, 1);
		while (wantsToEnter.get(1 - threadNumber) == 1) {
			if (turn != threadNumber) {
				wantsToEnter.set(threadNumber,0);
				while (turn != threadNumber) {
					try {
						Thread.sleep(10);
					} catch (InterruptedException e) {
						e.printStackTrace();
					}
				}
				wantsToEnter.set(threadNumber,1);
			}
		}
	};

	/**
	 * Releases the lock.
	 * @param threadNumber is the number of the releasing thread, 
	 * threadNumber == 0|1
	 */
	public void unlock(int threadNumber) {
		turn = 1 - threadNumber;
		wantsToEnter.set(threadNumber,0);
	};
}
