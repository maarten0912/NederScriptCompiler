package pp.block2.cp.queue; 

/**
 * Simple Queue interface
 * @param <T> The type of objects in the queue.
 */
public interface Queue<T> {
    /** Pushes an element at the head of the queue. */
    void push(T x);

    /** Obtains and removes the tail of the queue. */
    T pull() throws QueueEmptyException;

    /** Returns the number of elements in the queue. */
    int getLength();
}
