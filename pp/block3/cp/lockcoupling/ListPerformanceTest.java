package pp.block3.cp.lockcoupling; 

import nl.utwente.pp.cp.junit.ConcurrentRunner;
import nl.utwente.pp.cp.junit.Threaded;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;

/**
 * Test for testing the performance of the implemented lists 
 * in exercise 3 of block 3.
 */
@RunWith(ConcurrentRunner.class)
public class ListPerformanceTest {

	/**
	 * The amount of threads to use for this test.
	 */
	private static final int THREAD_AMOUNT = 1000;

	/**
	 * The amount of calls made per thread.
	 */
	private static final int CALLS_PER_THREAD = 1000;

	/**
	 * The amount of elements per list.
	 */
	private static final int AMOUNT_OF_ELEMENTS = 1000;

	private long startTime;
	private long finishTime;

	private List<String> synchronizedList;
	private List<String> lockCoupledList;

	private void initLists() {
		this.synchronizedList = new SynchronizedList<>();

		/*
		 TODO Replace the constructor call below with your own
		 class's constructor to compare it with the SynchronizedList
		*/

		this.lockCoupledList = new FineGrainedList<>();
		
		for (int i = 1; i <= AMOUNT_OF_ELEMENTS / 5; i++){
			this.synchronizedList.add("aap");
			this.synchronizedList.add("noot");
			this.synchronizedList.add("mies");
			this.synchronizedList.add("wim");
			this.synchronizedList.add("zus");
			this.lockCoupledList.add("aap");
			this.lockCoupledList.add("noot");
			this.lockCoupledList.add("mies");
			this.lockCoupledList.add("wim");
			this.lockCoupledList.add("zus");
		}
	}

	@Before
	public void before() {
		this.initLists();
		this.startTime = System.nanoTime();
		this.finishTime = 0;
	}

	@Test
	@Threaded(count = THREAD_AMOUNT)
	public void synchronizedListTest() {
		for (int i = 0; i < CALLS_PER_THREAD; i++) {
			this.synchronizedList.remove("foo");
		}
	}

	@Test
	@Threaded(count = THREAD_AMOUNT)
	public void lockCoupledListTest() {
		for (int i = 0; i < CALLS_PER_THREAD; i++) {
			this.lockCoupledList.remove("foo");
		}
	}

	@After
	public void after() {
		this.finishTime = System.nanoTime();
		System.out.printf("Total execution time: %s ms.%n",
				  (this.finishTime - this.startTime) / 1000000);
	}

}
