package pp.block3.cp.synchronizers; 

/**
 * BasicLock interface which can lock with 2 threads based on thread number.
 */
public interface BasicLock {
	/**
	 * Acquires the lock.
	 * @param threadNumber is the number of the requesting thread, 
	 * threadNumber == 0|1
	 */
	void lock(int threadNumber);

	/**
	 * Releases the lock.
	 * @param threadNumber is the number of the releasing thread, 
	 * threadNumber == 0|1
	 */
	void unlock(int threadNumber);
}
