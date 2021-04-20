package pp.block1.cc.test;

import org.junit.Test;


@SuppressWarnings("javadoc")
public class ID6Test {
	private static LexerTester tester = null; // TODO fill in once you have the ID6 grammar

	@Test
	public void tooShortTest() {
		tester.wrong("1");
		tester.wrong("a");
	}
	
	@Test
	public void rightLengthTest() {
		tester.correct("a12345");
		tester.correct("AaBbCc");
		tester.correct("a12a45");
		tester.wrong("1a2345");
		tester.wrong("a12.45");
	}
	
	@Test
	public void tooLongTest() {
		tester.wrong("a123456");
		tester.wrong("0123456");
	}
	
	@Test
	public void successiveTest() {
		// 0 or >1 ids in succession
		tester.yields("");
		// tester.yields("a12345AaBbCc", ID6.ID, ID6.ID); // TODO uncomment once you have the ID6 grammar
		tester.wrong("a12345 AaBbCc");
		tester.wrong("a123451a2345");
	}
}
