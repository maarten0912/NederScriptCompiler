package pp.project.build;

import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.TimeUnit;

import org.antlr.v4.runtime.CharStream;
import org.antlr.v4.runtime.CharStreams;
import org.antlr.v4.runtime.*;
import org.antlr.v4.runtime.tree.ParseTree;
import pp.project.elaboration.NederScriptChecker;
import pp.project.elaboration.NederScriptLexer;
import pp.project.elaboration.NederScriptParser;
import pp.project.elaboration.NederScriptResult;
import pp.project.exception.ErrorListener;
import pp.project.exception.ParseException;
import pp.project.generation.NederScriptGenerator;
import pp.project.generation.NederScriptProgram;
import pp.project.generation.SprockellBuilder;

public class NederScriptCompiler {

    private final static NederScriptCompiler instance = new NederScriptCompiler();

    public static NederScriptCompiler instance() {
        return instance;
    }

    private final NederScriptChecker checker;

    private final NederScriptGenerator generator;

    private NederScriptCompiler() {
        this.checker = new NederScriptChecker();
        this.generator = new NederScriptGenerator();
    }

    /**
     * Function to run the compiler
     *
     * @param args
     */
    public static void main(String[] args) {
        String filename;
        if (args.length != 1) {
            filename = "program.ns";
        } else {
            filename = args[0];
        }

        try {
            new NederScriptCompiler().run(filename, "pp/project/", true, true);
        } catch (ParseException e) {
            e.printStackTrace();
        }

    }

    public List<String> run(String filename, String dir, Boolean show, Boolean ghc) throws ParseException {
        List<String> lines = new ArrayList<>();
        try {
            System.out.println("--- Compiling " + filename);

            NederScriptProgram prog = instance().compile(new File(dir + filename));
            SprockellBuilder sprockell = prog.toHaskell();

            System.out.println("\n--- Finished compiling " + filename);

            filename = filename.split(".ns")[0];
            String filenamehs = filename + ".hs";

            new HaskellRunner().run(dir + filenamehs, sprockell);

            System.out.println("\n--- Created haskell file " + filenamehs);


            if (ghc) {
                Process p = Runtime.getRuntime().exec("ghc " + filenamehs + " -outputdir tmp -o out/" + filename, null, new File("pp/project/"));

                System.out.println("\n--- Created executable out/" + filename + ".exe");

                BufferedReader errors = new BufferedReader(new InputStreamReader(p.getErrorStream()));

                String line;

                if ((line = errors.readLine()) != null) {
                    // TODO error?
                    System.out.println("\nHaskell error:");
                    System.out.println(line);
                    while ((line = errors.readLine()) != null) {
                        System.out.println(line);
                    }
                }

                p.waitFor();

                System.out.println("\n--- Running executable '" + filename + ".exe' \n");

                Process execute = Runtime.getRuntime().exec("pp/project/out/" + filename + ".exe");

                BufferedReader output = new BufferedReader(new InputStreamReader(execute.getInputStream()));

                while ((line = output.readLine()) != null) {
                    System.out.println(line);
                }

            } else {
                if (show)
                    System.out.println("\n--- Running executable '" + filename + "'\n");

                Process p = Runtime.getRuntime().exec("runhaskell " + filenamehs, null, new File(dir));

                BufferedReader errors = new BufferedReader(new InputStreamReader(p.getErrorStream()));

                String line;

                p.waitFor();

                if (errors.readLine() != null) {
                    if ((line = errors.readLine()) != null && show) {
                        System.out.println("\nHaskell error:");
                        System.out.println(line);
                        while ((line = errors.readLine()) != null) {
                            System.out.println(line);
                        }
                    }
                }
                BufferedReader output = new BufferedReader(new InputStreamReader(p.getInputStream()));

                while ((line = output.readLine()) != null) {
                    if (show)
                        System.out.println(line);
                    lines.add(line);

                }
            }

        } catch(IOException | InterruptedException exc){
            exc.printStackTrace();
        }

        return lines;
    }

    public NederScriptProgram compile(String text) throws ParseException {
        return compile(parse(text));
    }

    public NederScriptProgram compile(File file) throws ParseException, IOException {
        return compile(parse(file));
    }

    public NederScriptProgram compile(ParseTree tree) throws ParseException {
        NederScriptResult checkResult = this.checker.check(tree);
        return this.generator.generate(tree, checkResult);
    }


    public NederScriptResult check(String text) throws ParseException {
        return check(parse(text));
    }

    public NederScriptResult check(File file) throws IOException, ParseException {
        return check(parse(file));
    }

    public NederScriptResult check(ParseTree tree) throws ParseException {
        return this.checker.check(tree);
    }


    public ParseTree parse(String text) throws ParseException {
        return parse(CharStreams.fromString(text));
    }

    public ParseTree parse(File file) throws IOException, ParseException {
        return parse(CharStreams.fromPath(file.toPath()));
    }

    private ParseTree parse(CharStream chars) throws ParseException {
        ErrorListener listener = new ErrorListener();
        Lexer lexer = new NederScriptLexer(chars);
        lexer.removeErrorListeners();
        lexer.addErrorListener(listener);
        TokenStream tokens = new CommonTokenStream(lexer);
        NederScriptParser parser = new NederScriptParser(tokens);
        parser.removeErrorListeners();
        parser.addErrorListener(listener);
        ParseTree result = parser.program();
        listener.throwException();
        return result;
    }


}
