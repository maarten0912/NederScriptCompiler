//package pp.block2.cp.queue;
//
//import net.jcip.annotations.ThreadSafe;
//
//import java.util.ArrayList;
//import java.util.LinkedList;
//import java.util.List;
//
///**
// * Implementation of the LinkedList{@link Queue} which uses fine grained locking.
// */
//@ThreadSafe
//public class MultiQueue<T> implements Queue<T> {
//
//    List<LinkedList<T>> queues = new ArrayList<>();
//
//    public MultiQueue(int num) {
//        for (int i = 0; i < num; i++) {
//            queues.add(new LinkedList<>());
//        }
//    }
//
//    @Override
//    public void push(T x) {
////        queues.get
//    }
//
//    @Override
//    public T pull() throws QueueEmptyException {
//        T res = this.queue.pull();
//        if (res == null) {
//            throw new QueueEmptyException();
//        }
//        return res;
//    }
//
//    @Override
//    public int getLength() {
//        return this.queue.size();
//    }
//}
