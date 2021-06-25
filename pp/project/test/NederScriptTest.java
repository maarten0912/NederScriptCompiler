package pp.project.test;

import org.junit.Test;

import org.antlr.v4.runtime.tree.ParseTree;
import pp.project.build.NederScriptCompiler;
import pp.project.elaboration.NederScriptResult;
import pp.project.exception.ParseException;

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

            File dir = new File("pp/project/test/syntax/correct");
            File[] testFiles = dir.listFiles();

            for (File testFile : testFiles) {
                syntaxSucces("/syntax/correct/" + testFile.getName());
            }

            dir = new File("pp/project/test/syntax/wrong");
            testFiles = dir.listFiles();

            for (File testFile : testFiles) {
                syntaxFail("/syntax/wrong/" + testFile.getName());
            }

        } catch (IOException e) {
            fail(e.getMessage());
        }
    }

    @Test
    public void testSyntaxTree() {
        try {
            ParseTree tree = parse("/syntax/testtree.ns");
            tree = tree.getChild(0);
            ParseTree line2 = tree.getChild(6);
            assertEquals(line2.getChild(0).getChild(0).getChild(0).getChild(0).getText(),"Getal");
        } catch (IOException | ParseException e) {
            e.printStackTrace();
        }
    }

    @Test
    public void testContext() {
        try {
            File dir = new File("pp/project/test/context/correct");
            File[] testFiles = dir.listFiles();

            for (File testFile : testFiles) {
                checkSucces("/context/correct/" + testFile.getName());
            }

            dir = new File("pp/project/test/context/wrong");
            testFiles = dir.listFiles();

            for (File testFile : testFiles) {
                checkFail("/context/wrong/" + testFile.getName());
            }

        } catch (IOException e) {
            fail(e.getMessage());
        }
    }

    @Test
    public void testSemantics() {

    }

    private void syntaxSucces(String filename) throws IOException {
        try {
            System.out.println("\n" + ANSI_GREEN + "Testing file '" + filename + "' for syntax errors." + ANSI_RESET + "\n");
            parse(filename);
        } catch (ParseException exc) {
            exc.print();
            fail (filename + " should pass the check but didn't");
        }
    }

    private void syntaxFail(String filename) throws IOException {
        try {
            System.out.println("\n" + ANSI_GREEN + "Testing file '" + filename + "' for syntax errors." + ANSI_RESET);
            System.out.println(ANSI_RED + "This test should fail" + ANSI_RESET);
            parse(filename);
            fail (filename + " should fail the check but didn't");
        } catch (ParseException exc) {
            exc.print();
        }
    }

    private void checkSucces(String filename) throws IOException {
        try {
            System.out.println("\n" + ANSI_GREEN + "Currently checking file: " + filename + ANSI_RESET + "\n");
            check(parse(filename));
        } catch (ParseException exc) {
            exc.print();
            fail (filename + " should pass the check but didn't");
        }
    }

    private void checkFail(String filename) throws IOException {
        try {
            System.out.println("\n" + ANSI_GREEN + "Currently checking file: " + filename);
            System.out.println(ANSI_RED + "This test should fail" + ANSI_RESET);
            check(parse(filename));
            fail (filename + " should fail the check but didn't");
        } catch (ParseException exc) {
            exc.print();
        }
    }

    private ParseTree parse(String filename) throws IOException, ParseException {
        return this.compiler.parse(new File("pp/project/test/" + filename));
    }

    private ParseTree parse (File file) throws IOException, ParseException {
        return this.compiler.parse(file);
    }

    private NederScriptResult check(ParseTree tree) throws ParseException {
        return this.compiler.check(tree);
    }

}
