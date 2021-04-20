package pp.block1.cc.dfa;

/** Algorithm interface for checking whether a given DFA accepts a given word. */
public class EfficientChecker implements Checker {
	/**
	 * Returns <code>true</code> if the DFA with the given start state accepts
	 * the given word.
	 */
	public boolean accepts(State start, String word) {
		State state = start;
		for (char ch : word.toCharArray()) {
			if (state.hasNext(ch)) {
				state = state.getNext(ch);
			} else {
				return false;
			}
		}
		return state.isAccepting();
	}
}
