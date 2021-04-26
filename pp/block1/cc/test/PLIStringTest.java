package pp.block1.cc.test;

import org.junit.Test;
import pp.block1.cc.antlr.PLIString;

public class PLIStringTest {
	private static LexerTester tester = new LexerTester(PLIString.class);

	@Test
	public void succeedingTest() {
		tester.correct("\"Hello sir!\"");
		tester.correct("\"And like shakespeare said: \"\"The evil that men do lives after them; the good is oft " +
				"interred with their bones.\"\"\"");
		tester.correct("\"Yes look at this symbol: \"\" !!! \"");
		tester.yields("\"And like shakespeare said: \"\"The evil that men do lives after them; the good is oft " +
						"interred with their bones.\"\"\"", PLIString.STRING);
	}

	@Test
	public void noOutsideQuotesTest() {
		// spaces in keywords are not in the rules
		tester.wrong("Oops didn't start the string\"");
	}

	@Test
	public void quoteNotEscapedTest() {
		tester.wrong("\" Oops I did not escape this symbol: \" ...\"");
	}

}
