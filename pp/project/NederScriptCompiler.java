package pp.project;

import java.io.File;
import java.io.IOException;

import org.antlr.v4.runtime.CharStream;
import org.antlr.v4.runtime.CharStreams;
import org.antlr.v4.runtime.*;
import org.antlr.v4.runtime.tree.ParseTree;
import pp.iloc.Simulator;
import pp.iloc.model.Program;
import pp.project.elaboration.NederScriptChecker;
import pp.project.elaboration.NederScriptLexer;
import pp.project.elaboration.NederScriptParser;
import pp.project.elaboration.NederScriptResult;
import pp.project.exception.ErrorListener;
import pp.project.exception.ParseException;
import pp.project.generation.NederScriptGenerator;
import pp.project.generation.NederScriptProgram;
import pp.project.sprockell.HaskellRunner;
import pp.project.sprockell.SprockellBuilder;

public class NederScriptCompiler {

    private final static NederScriptCompiler instance = new NederScriptCompiler();

    public static NederScriptCompiler instance() {
        return instance;
    }

    private final NederScriptChecker checker;

    private NederScriptCompiler() {
        this.checker = new NederScriptChecker();
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


    public ParseTree parse (String text) throws ParseException {
        return parse(CharStreams.fromString(text));
    }

    public ParseTree parse (File file) throws IOException, ParseException {
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
