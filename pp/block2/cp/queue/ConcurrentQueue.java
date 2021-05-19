package pp.block2.cp.queue;

import net.jcip.annotations.ThreadSafe;

import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;

/**
 * Implementation of the LinkedList{@link Queue} which uses fine grained locking.
 */
@ThreadSafe
public class ConcurrentQueue<T> implements Queue<T> {
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

    Lock l1 = new ReentrantLock();
    Lock l2 = new ReentrantLock();

    @Override
    public void push(T x) {
        LinkedListNode<T> temp = new LinkedListNode<>(x);
        l1.lock();
        try {
            if (this.head == null) {
                this.head = temp;
            } else {
                this.tail.setNext(temp);
            }
            this.tail = temp;
        } finally {
            l1.unlock();
        }
        this.length.incrementAndGet();
    }

    @Override
    public T pull() throws QueueEmptyException {
        l2.lock();
        try {
            if (this.head != null) {
                T x = this.head.getContent();
                this.head = this.head.hasNext()
                        ? this.head.getNext() : null;
                this.length.decrementAndGet();
                return x;
            } else {
                throw new QueueEmptyException();
            }
        } finally {
            l2.unlock();
        }
    }

    @Override
    public int getLength() {
        return this.length.get();
    }
}
