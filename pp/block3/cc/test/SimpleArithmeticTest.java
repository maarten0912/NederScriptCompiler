package pp.block3.cc.test;

import org.antlr.v4.runtime.*;
import org.antlr.v4.runtime.tree.ParseTree;
import org.antlr.v4.runtime.tree.ParseTreeWalker;
import org.junit.Test;
import pp.block3.cc.antlr.*;

import static org.junit.Assert.assertEquals;

public class SimpleArithmeticTest {
	private final ParseTreeWalker walker = new ParseTreeWalker();
	private final ArithmeticListener al = new ArithmeticListener();

	@Test
	public void test() {
		test(Type.BOOL, "5 = 3");
		test(Type.BOOL, "true = false");
		test(Type.BOOL, "yeet = yoot");
		test(Type.BOOL, "true + false");
		test(Type.NUM, "3 ^ 4");
		test(Type.NUM, "5 + 6");
		test(Type.STR, "yeet ^ 3");
		test(Type.ERR, "3 ^ yeet");
		test(Type.STR, "yeet + yoot");
		test(Type.ERR, "true ^ 4");
		test(Type.ERR, "4 ^ yeet");
		test(Type.ERR, "true + 3");
		test(Type.ERR, "true = 3");
	}

	private void test(Type expected, String t) {
		assertEquals(expected, parseSimpleArithmeticAttr(t).type);
		ParseTree tree = parseSimpleArithmetic(t);
		this.al.init();
		this.walker.walk(this.al, tree);
		assertEquals(expected, this.al.type(tree));
	}

	private ParseTree parseSimpleArithmetic(String text) {
		CharStream chars = CharStreams.fromString(text);
		Lexer lexer = new SimpleArithmeticAttrLexer(chars);
		TokenStream tokens = new CommonTokenStream(lexer);
		SimpleArithmeticParser parser = new SimpleArithmeticParser(tokens);
		return parser.t();
	}

	private SimpleArithmeticAttrParser.TContext parseSimpleArithmeticAttr(String text) {
		CharStream chars = CharStreams.fromString(text);
		Lexer lexer = new SimpleArithmeticAttrLexer(chars);
		TokenStream tokens = new CommonTokenStream(lexer);
		SimpleArithmeticAttrParser parser = new SimpleArithmeticAttrParser(tokens);
		return parser.t();
	}
}
