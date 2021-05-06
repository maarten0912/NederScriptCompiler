package pp.block3.cp.lockcoupling; 

/**
 * Interface as given for implementing an concurrent linked list 
 * in exercise 3 of block 3.
 */
public interface List<T> {
	/**
	 * Insert an element at the specified position in the list.
	 * @param position The position to insert the element at.
	 * @param value The value of the element to insert.
	 */
	void insert(int position, T value);

	/**
	 * Add an element to the end of the list.
	 * @param value The value of the element to add to the list.
	 */
	void add(T value);

	/**
	 * Remove the specified element from the list.
	 * @param item The element to remove from the list.
	 */
	void remove(T item);

	/**
	 * Delete the element at the specified position.
	 * @param position The position of the element that should be deleted.
	 */
	void delete(int position);

	/**
	 * Get the amount of elements currently in this list.
	 * @return The size of the list.
	 */
	int size();

	/**
	 * A toString function for the list.
	 * @return The string representing the list.
	 */
	String toString();
}
