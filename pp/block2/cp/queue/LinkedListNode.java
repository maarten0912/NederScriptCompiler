package pp.block2.cp.queue;

/**
 * Class which represents the Node in a LinkedList.
 * @param <T> The type of object saved in this LinkedListNode.
 */
public class LinkedListNode<T> {

    /**
     * The object that is backed by this node.
     */
    private T content;

    /**
     * The pointer to the next node in the list.
     */
    private LinkedListNode <T> next;

    /**
     * Constructs a {@link LinkedListNode} without a next node.
     * @param c The object to back by this node.
     */
    LinkedListNode(T c) {
        this.content = c;
    }

    /**
     * Constructs a {@link LinkedListNode} with a next node.
     * @param c The object to back by this node.
     */
    LinkedListNode(T c, LinkedListNode<T> n) {
        this(c);
        this.next = n;
    }

    /**
     * Check if the node has a next node pointer.
     * @return True if there is a pointer to a next node, false otherwise.
     */
    public boolean hasNext() {
        return this.next != null;
    }

    /**
     * Set the pointer to the next node.
     * @param n The next node to point to, or null if there is not any.
     */
    public void setNext(LinkedListNode<T> n) {
        this.next = n;
    }

    /**
     * Get the next node pointer of this node.
     * @return The pointer to the next node,
     * or null if there is none defined.
     */
    public LinkedListNode<T> getNext() {
        return this.next;
    }

    /**
     * Get the backed object of the node.
     * @return The backed object of the node.
     */
    public T getContent() {
        return this.content;
    }

    /**
     * Set the backed object of the node.
     * @param content The new object corresponding to the node.
     */
    public void setContent(T content) {
        this.content = content;
    }
}
