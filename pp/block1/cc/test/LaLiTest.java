package pp.block1.cc.test;

import org.junit.Test;
import pp.block1.cc.antlr.LaLi;

public class LaLiTest {
	private static LexerTester tester = new LexerTester(LaLi.class);

	@Test
	public void succeedingTest() {
		tester.correct("LaaaaLaLaa Laaaa    LaLiLaa");
		tester.yields("LaaaaLaLaa Laaaa    LaLiLaa", LaLi.LALA, LaLi.LALALALI, LaLi.LA);
		tester.correct("Laa");
		tester.wrong("LaaLi");

	}

	@Test
	public void noRepeatingITest() {
		// spaces in keywords are not in the rules
		tester.wrong("LaLaLaLii");
	}

	@Test
	public void noCapitalATest() {
		// spaces in keywords are not in the rules
		tester.wrong("LALa");
	}
}
