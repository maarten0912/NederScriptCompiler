package pp.project.test;

import org.junit.Test;

import org.antlr.v4.runtime.*;
import org.antlr.v4.runtime.tree.ParseTree;
import pp.project.*;

import java.io.File;
import java.io.IOException;

import static org.junit.Assert.*;

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
        try {

            checkSucces("test1.ns");
            checkFail("test1wrong.ns");

//            checkSucces("test2.ns");
            // test2 is for when function calls are implemented

            checkSucces("testfor.ns");
            checkSucces("testwhile.ns");
            checkSucces("testifelse.ns");

        } catch (IOException e) {
            fail(e.getMessage());
        }
    }

    @Test
    public void testContext() {
        try {
            checkSucces("testfor.ns");
            checkSucces("testwhile.ns");
            checkSucces("testifelse.ns");

        } catch (IOException e) {
            fail(e.getMessage());
        }
    }

    @Test
    public void testSemantics() {

    }


    private void checkSucces(String filename) throws IOException {
        try {
            System.out.println("\n" + ANSI_GREEN + "currently running file: " + filename + ANSI_RESET + "\n");
            check(parse(filename));
        } catch (ParseException exc) {
            fail (filename + " should pass the check but didn't\nException was: " + exc.getMessage());
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

    private ParseTree parse (File file) throws IOException {
        return this.compiler.parse(file);
    }

    private NederScriptResult check(ParseTree tree) throws ParseException {
        return this.compiler.check(tree);
    }

}
