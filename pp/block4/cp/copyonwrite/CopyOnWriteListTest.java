package pp.block4.cp.copyonwrite;

import nl.utwente.pp.cp.junit.ConcurrentRunner;
import nl.utwente.pp.cp.junit.ThreadNumber;
import nl.utwente.pp.cp.junit.Threaded;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Random;
import java.util.concurrent.CopyOnWriteArrayList;

/**
 * Class which tests the performance of the CopyOnWriteList implementations, as well as an synchronized list for
 * comparison. Asked for in exercise 3 of block 4.
 */
@RunWith(ConcurrentRunner.class)
public class CopyOnWriteListTest {

	/**
	 * The number of elements in the list.
	 */
	private static final int NUMBER_OF_ELEMENTS = 1000;

	/**
	 * The amount of writers used for each test.
	 */
	private static final int NUMBER_OF_WRITERS = 10;

	/**
	 * The amount of readers used for each test.
	 */
	private static final int NUMBER_OF_READERS = 100;

	/**
	 * The amount of writes performed per writer in each test.
	 */
	private static final int NUMBER_OF_WRITES = 1000;

	/**
	 * The amount of reads performed per reader in each test.
	 */
	private static final int NUMBER_OF_READS = 100 * 1000;

	/**
	 * Random object to generate list values with.
	 */
	private final Random random = new Random();

	/**
	 * The start time of the test (nanoseconds).
	 */
	private long startTime;

	/**
	 * The finish time of the test (nanoseconds).
	 */
	private long finishTime;

	/**
	 * The synchronized list to use for testing.
	 */
	private final List<String> synchronizedList = Collections.synchronizedList(new ArrayList<>());

	/**
	 * The custom implemented copy on write list.
	 */
	private final CustomCopyOnWriteArrayList<String> customCopyOnWriteList = new CustomCopyOnWriteArrayList<>();

	/**
	 * The default Java copy on write list.
	 */
	private final List<String> copyOnWriteList = new CopyOnWriteArrayList<>();

	public CopyOnWriteListTest() {
		this.random.ints().mapToObj(i -> "" + i).limit(NUMBER_OF_ELEMENTS).forEach(this.synchronizedList::add);
		this.random.ints().mapToObj(i -> "" + i).limit(NUMBER_OF_ELEMENTS).forEach(this.copyOnWriteList::add);
		this.random.ints().mapToObj(i -> "" + i).limit(NUMBER_OF_ELEMENTS).forEach(this.customCopyOnWriteList::add);
	}

	@Before
	public void before() {
		this.finishTime = 0;
		this.startTime = System.nanoTime();
	}

	private interface ListSetter<T> {
		T set(int index, T element);
	}

	private interface ListGetter<T> {
		T get(int index);
	}

	private void listTest(int threadNumber, ListSetter<String> setter, ListGetter<String> getter) {
		if (threadNumber < NUMBER_OF_WRITERS) {
			for (int i = 0; i < NUMBER_OF_WRITES; i++) {
				setter.set(this.random.nextInt(NUMBER_OF_ELEMENTS),
						"" + this.random.nextInt(NUMBER_OF_ELEMENTS));
			}
		} else {
			for (int i = 0; i < NUMBER_OF_READS; i++) {
				getter.get(this.random.nextInt(NUMBER_OF_ELEMENTS));
			}
		}
	}

	@Test
	@Threaded(count = NUMBER_OF_WRITERS + NUMBER_OF_READERS)
	public void synchronizedListTest(@ThreadNumber int threadNumber) {
		this.listTest(threadNumber, this.synchronizedList::set, this.synchronizedList::get);
	}

	@Test
	@Threaded(count = NUMBER_OF_WRITERS + NUMBER_OF_READERS)
	public void copyOnWriteListTest(@ThreadNumber int threadNumber) {
		this.listTest(threadNumber, this.copyOnWriteList::set, this.copyOnWriteList::get);
	}

	@Test
	@Threaded(count = NUMBER_OF_WRITERS + NUMBER_OF_READERS)
	public void customCopyOnWriteListTest(@ThreadNumber int threadNumber) {
		this.listTest(threadNumber, this.customCopyOnWriteList::set, this.customCopyOnWriteList::get);
	}

	@After
	public void after() {
		this.finishTime = System.nanoTime();
		System.out.printf("Total execution time: %s ms.%n", (this.finishTime - this.startTime) / 1000000);
	}

}