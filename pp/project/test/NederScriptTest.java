package pp.project.test;

import org.antlr.v4.runtime.*;
import org.antlr.v4.runtime.tree.ParseTree;
import pp.project.*;

import java.io.File;
import java.io.IOException;

public class NederScriptTest {

    private final NederScriptCompiler compiler = NederScriptCompiler.instance();

    public static final String ANSI_BLACK_BACKGROUND = "\u001B[40m";
    public static final String ANSI_RED_BACKGROUND = "\u001B[41m";
    public static final String ANSI_GREEN_BACKGROUND = "\u001B[42m";
    public static final String ANSI_YELLOW_BACKGROUND = "\u001B[43m";
    public static final String ANSI_BLUE_BACKGROUND = "\u001B[44m";
    public static final String ANSI_PURPLE_BACKGROUND = "\u001B[45m";
    public static final String ANSI_CYAN_BACKGROUND = "\u001B[46m";
    public static final String ANSI_WHITE_BACKGROUND = "\u001B[47m";

    public static final String ANSI_RESET = "\u001B[0m";
    public static final String ANSI_BLACK = "\u001B[30m";
    public static final String ANSI_RED = "\u001B[31m";
    public static final String ANSI_GREEN = "\u001B[32m";
    public static final String ANSI_YELLOW = "\u001B[33m";
    public static final String ANSI_BLUE = "\u001B[34m";
    public static final String ANSI_PURPLE = "\u001B[35m";
    public static final String ANSI_CYAN = "\u001B[36m";
    public static final String ANSI_WHITE = "\u001B[37m";

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

    private void checkSucces(String filename) throws IOException {
        try {
            System.out.println("\n" + ANSI_GREEN + "currently running file: " + filename + ANSI_RESET + "\n");
            check(parse(filename));
        } catch (ParseException exc) {
            fail (filename + " should pass the check but didn't");
        }
    }

    private void checkFail(String filename) throws IOException {
        try {
            System.out.println("\n" + ANSI_GREEN + "currently running file: " + filename);
            System.out.println("the errors below are the errors that should occur;" + ANSI_RESET);
            check(parse(filename));
            fail (filename + " should fail the check but didn't");
        } catch (ParseException exc) {
            //good
        }
    }

    private ParseTree parse(String filename) throws IOException {
        return this.compiler.parse(new File("pp/project/test/" + filename));
    }

    private ParseTree parseNS(String text) {

    }

}
