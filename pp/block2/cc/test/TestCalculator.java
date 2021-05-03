package pp.block2.cc.test;

import org.antlr.v4.runtime.*;
import org.antlr.v4.runtime.tree.ParseTree;
import org.antlr.v4.runtime.tree.ParseTreeWalker;
import org.junit.Test;
import pp.block2.cc.antlr.ArithmeticLexer;
import pp.block2.cc.antlr.ArithmeticParser;
import pp.block2.cc.antlr.Calculator.*;

import java.math.BigInteger;
import java.util.Scanner;

import static org.junit.Assert.assertEquals;

public class TestCalculator {

    @Test
    public void testPlus() {
        assertEquals(count("1+2"),new BigInteger("3"));
        assertEquals(count("2+5"),new BigInteger("7"));
        assertEquals(count("1+70"),new BigInteger("71"));
        assertEquals(count("0 + 2"),new BigInteger("2"));
    }

    @Test
    public void testMinus() {
        assertEquals(count("2-1"),new BigInteger("1"));
        assertEquals(count("5-2"),new BigInteger("3"));
        assertEquals(count("70-1"),new BigInteger("69"));
        assertEquals(count("0 - 2"),new BigInteger("-2"));
    }
    @Test
    public void testDoubleNegation() {
        assertEquals(count("2--1"),new BigInteger("3"));
    }

    @Test
    public void testMult() {
        assertEquals(count("2*1"),new BigInteger("2"));
        assertEquals(count("5*2"),new BigInteger("10"));
        assertEquals(count("70*-1"),new BigInteger("-70"));
        assertEquals(count("1 + 2 * 3"),new BigInteger("7"));
    }

    @Test
    public void testPow() {
        assertEquals(count("2^3"),new BigInteger("8"));
        assertEquals(count("5^2"),new BigInteger("25"));
        assertEquals(count("5^1+1"),new BigInteger("6"));
        assertEquals(count("5^(1+1)"),new BigInteger("25"));
    }

    public BigInteger count(String input) {
        CharStream stream = CharStreams.fromString(input);
        Lexer lexer = new ArithmeticLexer(stream);
        TokenStream tokens = new CommonTokenStream(lexer);
        ArithmeticParser parser = new ArithmeticParser(tokens);
        ParseTree tree = parser.expression();

        ParseTreeWalker walker = new ParseTreeWalker();
        MyArithmeticListener listener = new MyArithmeticListener();
        walker.walk(listener, tree);
        return listener.getResult();
    }

}
