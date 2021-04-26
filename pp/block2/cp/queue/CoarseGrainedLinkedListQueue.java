package pp.block2.cp.queue; 

import java.util.concurrent.atomic.AtomicInteger;

import net.jcip.annotations.ThreadSafe;

/**
 * Implementation of the LinkedList{@link Queue} which uses coarse grained locking.
 */
@ThreadSafe
public class CoarseGrainedLinkedListQueue<T> implements Queue<T> {
    /**
     * The head of the linked list queue. 
     * Points to the first element of the list.
     */
    private volatile LinkedListNode<T> head = null;

    /**
     * The tail of the linked list queue. 
     * Points to the last element of the list.
     */
    private volatile LinkedListNode<T> tail = null;

    /**
     * Length of the list, cached for performance.
     */
    private AtomicInteger length = new AtomicInteger();

    @Override
    public void push(T x) {
        LinkedListNode<T> temp = new LinkedListNode<>(x);
        synchronized (this) {
            if (this.head == null) {
                this.head = temp;
            } else {
                this.tail.setNext(temp);
            }
            this.tail = temp;
        }
        this.length.incrementAndGet();
    }

    @Override
    public synchronized T pull() throws QueueEmptyException {
        if (this.head != null) {
            T x = this.head.getContent();
            this.head = this.head.hasNext()
            		? this.head.getNext() : null;
            this.length.decrementAndGet();
            return x;
        } else {
            throw new QueueEmptyException();
        }
    }

    @Override
    public int getLength() {
        return this.length.get();
    }
}
