package pp.block3.cp.lockcoupling;

import net.jcip.annotations.ThreadSafe;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;

/**
 * Synchronized list which just synchronizes on the whole list
 * Used for testing performance in exercise 3 of block 3.
 */
@ThreadSafe
public class FineGrainedList<T> implements List<T> {

	// The sentinel node points to the first element of the list.
	private Node<T> sentinel = new Node<>(null, null);

	private Map<Node<T>, Lock> locks = new HashMap<>();

	public FineGrainedList() {
		locks.put(sentinel, new ReentrantLock());
	}

	@Override
	public void insert(int position, T value) {
		Node<T> current = this.find(position);
		locks.get(current).lock();
		current.setNext(new Node<>(value, current.getNext()));
		locks.put(current.getNext(), new ReentrantLock());
		locks.get(current).unlock();
	}

	@Override
	public void add(T value) {
		Node<T> current = this.sentinel;
		locks.get(current).lock();
		while (current.getNext() != null) {
			locks.get(current.getNext()).lock();
			locks.get(current).unlock();
			current = current.getNext();
		}

		current.setNext(new Node<>(value, null));
		locks.put(current.getNext(), new ReentrantLock());
		locks.get(current).unlock();
	}

	@Override
	public int size() {
		int count = 0;
		Node<T> current = this.sentinel;
		locks.get(current).lock();
		while (current.getNext() != null) {
			count++;
			locks.get(current.getNext()).lock();
			locks.get(current).unlock();
			current = current.getNext();
		}
		locks.get(current).unlock();
		return count;
	}

	@Override
	public void remove(T item) {
		Node<T> current = this.sentinel;
		locks.get(current).lock();
		Node<T> tmp;
		while (current.getNext() != null) {
			tmp = current;
			locks.get(current.getNext()).lock();
			current = current.getNext();
			locks.get(tmp).unlock();
			if (current.getItem().equals(item)) {
				locks.get(current.getNext()).lock();
				tmp.setNext(current.getNext());
				locks.get(current.getNext()).unlock();
				return;
			}
		}
		locks.get(current).unlock();
	}

	@Override
	public void delete(int position) {
		Node<T> current = this.find(position);
		locks.get(current).lock();
		if (current.getNext() != null) {
			locks.get(current.getNext()).lock();
			locks.get(current.getNext().getNext()).lock();
			current.setNext(current.getNext().getNext());
			locks.get(current.getNext().getNext()).unlock();
			locks.get(current.getNext()).unlock();
		}
		locks.get(current).unlock();
	}

	/**
	 * Find the element at the specified position.
	 * @param position The position to get the element of.
	 */
	private Node<T> find(int position) {
		Node<T> current = this.sentinel;
		locks.get(current).lock();
		while (current.getNext() != null && position > 0) {
			locks.get(current.getNext()).lock();
			locks.get(current).unlock();
			current = current.getNext();
			position--;
		}
		locks.get(current).unlock();
		return current;
	}

	@Override
	public String toString() {
		String result = "";
		Node<T> current = this.sentinel;
		locks.get(current).lock();
		while (current.getNext() != null) {
			locks.get(current.getNext()).lock();
			locks.get(current).unlock();
			current = current.getNext();
			result += current.getItem().toString() + " ";
		}
		locks.get(current).unlock();
		return result;
	}

}
