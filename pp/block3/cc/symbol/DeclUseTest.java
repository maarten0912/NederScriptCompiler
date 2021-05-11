package pp.block3.cc.symbol;

import org.antlr.v4.runtime.*;
import org.antlr.v4.runtime.tree.ParseTree;
import org.antlr.v4.runtime.tree.ParseTreeWalker;
import org.junit.Test;
import pp.block3.cc.antlr.*;

import java.io.IOException;
import java.util.Stack;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.fail;

public class DeclUseTest {
    private final ParseTreeWalker walker = new ParseTreeWalker();
    private final MyDeclUseListener al = new MyDeclUseListener();

    @Test
    public void test() {
        test(new Stack<>(), "pp/block3/cc/symbol/program1.txt");
        test(new Stack<>(), "pp/block3/cc/symbol/program2.txt");
        test(new Stack<>(), "pp/block3/cc/symbol/program3.txt");
    }

    private void test(Stack<String> expected, String t) {
        ParseTree tree = null;
        try {
            tree = parseDeclUse(t);
        } catch (IOException e) {
            e.printStackTrace();
            fail("Should be able to read file");
        }
        this.walker.walk(this.al, tree);
//        assertEquals(expected, this.al.getErrorstack());
    }

    private ParseTree parseDeclUse(String file) throws IOException {
        CharStream chars = CharStreams.fromFileName(file);
        Lexer lexer = new SimpleArithmeticAttrLexer(chars);
        TokenStream tokens = new CommonTokenStream(lexer);
        System.out.println(tokens.getText());
        DeclUseParser parser = new DeclUseParser(tokens);
//        System.out.println(parser.program().toStringTree());
        return parser.program();
    }

}
