package pp.block2.cc.antlr;

import org.antlr.runtime.tree.ParseTree;
import org.antlr.v4.runtime.*;
import pp.block2.cc.ParseException;
import pp.block2.cc.ll.Sentence;
import pp.block2.cc.ll.SentenceParser;

public class Calculator {

    public static void main(String[] args) {
        if (args.length == 0) {
            System.err.println("Usage: [text]+");
        } else {
            CharStream stream = CharStreams.fromString(args[0]);
            Lexer lexer = new ArithmeticLexer(stream);
            TokenStream tokens = new CommonTokenStream(lexer);
            ArithmeticParser parser = new ArithmeticParser(tokens);
            ParseTree tree = parser;

        }
    }
}
