package pp.block3.cp.lockcoupling; 

import net.jcip.annotations.ThreadSafe;

/**
 * Synchronized list which just synchronizes on the whole list 
 * Used for testing performance in exercise 3 of block 3.
 */
@ThreadSafe
public class SynchronizedList<T> implements List<T> {

	// The sentinel node points to the first element of the list.
	private Node<T> sentinel = new Node<>(null, null);

	@Override
	public synchronized void insert(int position, T value) {
		Node<T> current = this.find(position);
		current.setNext(new Node<>(value, current.getNext()));
	}

	@Override
	public synchronized void add(T value) {
		Node<T> current = this.sentinel;
		while (current.getNext() != null) {
			current = current.getNext();
		}
		current.setNext(new Node<>(value, null));
	}

	@Override
	public synchronized int size() {
		int count = 0;
		Node<T> current = this.sentinel;
		while (current.getNext() != null) {
			count++;
			current = current.getNext();
		}
		return count;
	}

	@Override
	public synchronized void remove(T item) {
		Node<T> current = this.sentinel;
		Node<T> tmp;
		while (current.getNext() != null) {
			tmp = current;
			current = current.getNext();
			if (current.getItem().equals(item)) {
				tmp.setNext(current.getNext());
				return;
			}
		}
	}

	@Override
	public synchronized void delete(int position) {
		Node<T> current = this.find(position);
		if (current.getNext() != null) {
			current.setNext(current.getNext().getNext());
		}
	}

	/**
	 * Find the element at the specified position.
	 * @param position The position to get the element of.
	 */
	private synchronized Node<T> find(int position) {
		Node<T> current = this.sentinel;
		while (current.getNext() != null && position > 0) {
			current = current.getNext();
			position--;
		}
		return current;
	}

	@Override
	public synchronized String toString() {
		String result = "";
		Node<T> current = this.sentinel;
		while (current.getNext() != null) {
			current = current.getNext();
			result += current.getItem().toString() + " ";
		}
		return result;
	}

}
