package pp.block3.cp.barriers; 

import java.util.ArrayList;
/**
 * Abstract class implementing the
 * map-reduce framework. To be completed
 * by the student.
 */
public abstract class MapReduceBase<I, O, R> {
	/**
	 * List for storing the results of all the
	 * map tasks
	 */
	private ArrayList<O> mapOutput;
	protected abstract O map(I in);
	protected abstract R reduce(ArrayList<O> in);
	/**
	 * Runs the map-reduce operation
	 * @param values the values to operate on
	 * @return the result of the map-reduce operation
	 */
	protected R run(ArrayList <I> values) {
		return null; // TODO fill in
	}
}
