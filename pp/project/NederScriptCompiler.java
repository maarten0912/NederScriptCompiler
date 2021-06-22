package pp.project;

import java.io.File;
import java.io.IOException;

import org.antlr.v4.runtime.CharStream;
import org.antlr.v4.runtime.CharStreams;
import org.antlr.v4.runtime.*;
import org.antlr.v4.runtime.tree.ParseTree;
import org.antlr.v4.runtime.tree.ParseTreeWalker;

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


    public ParseTree parse (String text) {
        return parse(CharStreams.fromString(text));
    }

    public ParseTree parse (File file) throws IOException {
        return parse(CharStreams.fromPath(file.toPath()));
    }

    private ParseTree parse(CharStream chars) {
        Lexer lexer = new NederScriptLexer(chars);
        TokenStream tokens = new CommonTokenStream(lexer);
        NederScriptParser parser = new NederScriptParser(tokens);
        return parser.program();
    }


}
