package pp.block3.cp.lockcoupling;

import net.jcip.annotations.NotThreadSafe;
/**
 * Simple node for synchronized list. 
 * Used for testing the performance in exercise 3 of block 3.
 */
@NotThreadSafe
public class Node<T> {
	/** The value of backed by this node. */
	private T item;

	/** The next node of the list. */
	private Node<T> next;

	public Node(T value, Node<T> next) {
		this.item = value;
		this.next = next;
	}

	public T getItem() {
		return this.item;
	}

	public Node<T> getNext() {
		return this.next;
	}

	public void setNext(Node<T> next) {
		this.next = next;
	}
}
