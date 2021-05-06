package pp.block2.cc.antlr;

import org.antlr.v4.runtime.tree.ParseTree;
import org.antlr.v4.runtime.*;
import org.antlr.v4.runtime.tree.ParseTreeWalker;
import org.antlr.v4.runtime.tree.pattern.ParseTreePattern;

import java.math.BigInteger;
import java.util.Scanner;
import java.util.Stack;

public class Calculator {
    public static void main(String[] args) {
        while (true) {
            Scanner in = new Scanner(System.in);
            CharStream stream = CharStreams.fromString(in.nextLine());
            Lexer lexer = new ArithmeticLexer(stream);
            TokenStream tokens = new CommonTokenStream(lexer);
            ArithmeticParser parser = new ArithmeticParser(tokens);
            ParseTree tree = parser.expression();
            ParseTreeWalker walker = new ParseTreeWalker();
            MyArithmeticListener listener = new MyArithmeticListener();
            walker.walk(listener, tree);
            System.out.println(listener.getResult());
        }
    }

    public static class MyArithmeticListener extends ArithmeticBaseListener {
        private Stack<BigInteger> stack = new Stack<>();

        public BigInteger getResult() {
            if (stack.size() == 1) {
                return stack.peek();
            } else {
                return null;
            }
        }

        @Override
        public void exitExpression(ArithmeticParser.ExpressionContext ctx) {
            super.exitExpression(ctx);
            if (ctx.NUMBER() != null) {
                if (ctx.MINUS() != null) {
                    stack.push(BigInteger.valueOf(Long.parseLong(ctx.MINUS().getText() + ctx.NUMBER().getText())));
                } else {
                    stack.push(BigInteger.valueOf(Long.parseLong(ctx.NUMBER().getText())));
                }
            } else if (ctx.PLUS() != null) {
                BigInteger b = stack.pop();
                BigInteger a = stack.pop();
                stack.push(a.add(b));
            } else if (ctx.MINUS() != null) {
                BigInteger b = stack.pop();
                BigInteger a = stack.pop();
                stack.push(a.subtract(b));
            } else if (ctx.MULT() != null) {
                BigInteger b = stack.pop();
                BigInteger a = stack.pop();
                stack.push(a.multiply(b));
            } else if (ctx.POW() != null) {
                BigInteger b = stack.pop();
                BigInteger a = stack.pop();
                stack.push(a.pow(b.intValue()));
            }
        }
    }
}
