package pp.block3.cp.hotel;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;

/**
 * Simple Hotel class as given in exercise 1 of block 3.
 */
public class Hotel implements Runnable {

	private final static int NR_ROOMS = 10;
	private final Person[] rooms = new Person[NR_ROOMS];
	private final List<Person> queue = new ArrayList<>();
	private final Lock queueLock = new ReentrantLock();

	private boolean occupied(int i) {
		return (rooms[i] != null);
	}

	private int checkIn(Person p) {
		int i = 0;
		while (occupied(i)) {
			i = (i + 1) % NR_ROOMS;
		}
		this.rooms[i] = p;
		return i;
	}

	private void enter(Person p) {
		this.queueLock.lock();
		this.queue.add(p);
		this.queueLock.unlock();
	}

	// every desk employee should run as a separate thread
	@Override
	public void run() {
		while (true) {
			if (!this.queue.isEmpty()) {
				this.queueLock.lock();
				Person guest = this.queue.remove(0);
				this.queueLock.unlock();
				checkIn(guest);
			}
		}
	}

}
