package pp.project;

import java.io.File;
import java.io.IOException;

import org.antlr.v4.runtime.CharStream;
import org.antlr.v4.runtime.CharStreams;
import org.antlr.v4.runtime.*;
import org.antlr.v4.runtime.tree.ParseTree;
import org.antlr.v4.runtime.tree.ParseTreeWalker;

public class NederScriptCompiler {



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
