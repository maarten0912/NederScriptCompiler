package pp.block2.cc.antlr;

import org.antlr.v4.runtime.tree.ErrorNode;
import org.antlr.v4.runtime.tree.ParseTree;
import org.antlr.v4.runtime.*;
import org.antlr.v4.runtime.tree.ParseTreeWalker;
import org.antlr.v4.runtime.tree.TerminalNode;
import pp.block2.cc.ParseException;
import pp.block2.cc.ll.Sentence;
import pp.block2.cc.ll.SentenceParser;

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

            System.out.println(tree.toStringTree(parser));
            ParseTreeWalker walker = new ParseTreeWalker();
            MyArithmeticListener listener = new MyArithmeticListener();
            walker.walk(listener, tree);
            System.out.println(listener.getResult());
        }
    }

    private static class MyArithmeticListener extends ArithmeticBaseListener {
        private BigInteger result;

        private Stack<BigInteger> stack = new Stack<>();

        public BigInteger getResult() {
            return stack.pop();
        }

        @Override
        public void exitExpression(ArithmeticParser.ExpressionContext ctx) {
            super.exitExpression(ctx);
            if (ctx.NUMBER() != null) {
                stack.push(BigInteger.valueOf(Long.parseLong(ctx.NUMBER().getText())));
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

        @Override
        public void visitTerminal(TerminalNode node) {
            super.visitTerminal(node);
        }

        @Override
        public void visitErrorNode(ErrorNode node) {
            super.visitErrorNode(node);
        }
    }
}
