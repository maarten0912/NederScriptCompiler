package pp.block2.cp.queue;

import net.jcip.annotations.ThreadSafe;

import java.util.concurrent.ConcurrentLinkedQueue;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;

/**
 * Implementation of the LinkedList{@link Queue} which uses fine grained locking.
 */
@ThreadSafe
public class ConcurrentLinked<T> implements Queue<T> {

    ConcurrentLinkedQueue<T> queue = new ConcurrentLinkedQueue<>();

    @Override
    public void push(T x) {
        queue.add(x);
    }

    @Override
    public T pull() throws QueueEmptyException {
        T res = this.queue.poll();
        if (res == null) {
            throw new QueueEmptyException();
        }
        return res;
    }

    @Override
    public int getLength() {
        return this.queue.size();
    }
}
