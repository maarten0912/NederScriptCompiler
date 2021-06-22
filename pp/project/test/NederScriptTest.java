package pp.project.test;

import org.antlr.v4.runtime.*;
import org.antlr.v4.runtime.tree.ParseTree;
import org.junit.Test;
import pp.project.NederScriptCompiler;
import pp.project.NederScriptLexer;
import pp.project.NederScriptParser;

public class NederScriptTest {

    private final NederScriptCompiler compiler = new NederScriptCompiler();

    @Test
    public void testSyntax() {
        test("functie toevoeging (Getal a, Getal b) : Getal {\n" +
                "    geefterug a + b;\n" +
                "}");
    }

    @Test
    public void testContext() {

    }

    @Test
    public void testSemantics() {

    }

    private void test(String text) {
        ParseTree tree = parseNS(text);
        this.walker.walk(this.listener, tree);
    }

    private ParseTree parseNS(String text) {

    }

}
