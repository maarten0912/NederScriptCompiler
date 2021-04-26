package pp.block1.cc.test;

import org.junit.Assert;
import org.junit.Test;
import pp.block1.cc.dfa.EfficientChecker;
import pp.block1.cc.dfa.State;

import static pp.block1.cc.dfa.State.*;

/** Test class for Checker implementation. */
public class CheckerTest {
	private EfficientChecker myChecker = new EfficientChecker(); // TODO instantiate your Checker implementation

	private State dfa;

	@Test
	public void testID6() {
		this.dfa = ID6_DFA;
		accepts("a12345");
		rejects("");
		rejects("a12 45");
		rejects("a12 456");
		rejects("a123456");
		rejects("123456");
	}

	@Test
	public void testLALA() {
		this.dfa = LALA_DFA;
		accepts("La ");
		rejects("LLa");
		rejects("L aaa ");
		accepts("Laaaaaaaaaaaaaaa");
		accepts("Laa Laaaa    ");
		rejects("LALa");
		accepts("LaLaLaLi");
		rejects("LaLaaaaLaLii");
		accepts("Laaaa    Laaa     Laa     Li    ");
	}


	private void accepts(String word) {
		if (!this.myChecker.accepts(this.dfa, word)) {
			Assert.fail(String.format(
					"Word '%s' is erroneously rejected by %s", word, this.dfa));
		}
	}

	private void rejects(String word) {
		if (this.myChecker.accepts(this.dfa, word)) {
			Assert.fail(String.format(
					"Word '%s' is erroneously accepted by %s", word, this.dfa));
		}
	}
}
