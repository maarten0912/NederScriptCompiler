package pp.block2.cc.antlr;

import org.antlr.v4.runtime.CharStream;
import org.antlr.v4.runtime.CharStreams;
import org.antlr.v4.runtime.Lexer;
import pp.block2.cc.ParseException;
import pp.block2.cc.ll.Sentence;
import pp.block2.cc.ll.SentenceParser;

public class Caculator {

    public static void main(String[] args) {
        if (args.length == 0) {
            System.err.println("Usage: [text]+");
        } else {
            for (String text : args) {
                CharStream stream = CharStreams.fromString(text);
                Lexer lexer = new ArithmeticLexer(stream);
                try {
                    System.out.printf("Parse tree: %n%s%n",
                            new ArithmeticParser(lexer.getTokenStream()));
                } catch (ParseException e) {
                    e.printStackTrace();
                }
            }
        }
    }
}
