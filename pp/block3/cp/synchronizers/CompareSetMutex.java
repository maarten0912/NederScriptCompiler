package pp.block3.cp.synchronizers;

import javax.swing.table.JTableHeader;
import java.util.concurrent.atomic.AtomicInteger;

/**
 * BasicLock interface which can lock with 2 threads based on thread number.
 */
public class CompareSetMutex implements BasicLock {

	AtomicInteger owner;
	AtomicInteger level;

	public CompareSetMutex() {
		owner = new AtomicInteger();
		level = new AtomicInteger();
		owner.set(-1);
		level.set(0);
	}

	/**
	 * Acquires the lock.
	 * @param threadNumber is the number of the requesting thread, 
	 * threadNumber == 0|1
	 */
	public void lock(int threadNumber) {
		if (owner.get() == threadNumber) {
			level.incrementAndGet();
		} else {
			while (!owner.compareAndSet(-1,threadNumber)) {
				try {
					Thread.sleep(10);
				} catch (InterruptedException e) {
					e.printStackTrace();
				}
			};
			level.incrementAndGet();
			owner.set(threadNumber);
		}
	};

	/**
	 * Releases the lock.
	 * @param threadNumber is the number of the releasing thread, 
	 * threadNumber == 0|1
	 */
	public void unlock(int threadNumber) {
		if (owner.get() == threadNumber && level.decrementAndGet() == 0) {
			owner.set(-1);
		}
	};
}
