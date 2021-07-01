package pp.project.elaboration;
import org.antlr.v4.runtime.tree.ParseTree;
import org.antlr.v4.runtime.tree.ParseTreeWalker;
import pp.project.elaboration.NederScriptBaseListener;
import pp.project.elaboration.NederScriptParser;

public class NederScriptFunctionListener extends NederScriptBaseListener {

    private NederScriptChecker checker;
    private ScopeTable st;

    public void check(NederScriptChecker checker, ParseTree tree, ScopeTable st) {
        this.st = st;
        this.checker = checker;
        this.st.add("afdrukken", NederScriptType.LEEGTE);
        this.st.add("lengte", NederScriptType.GETAL);
        this.st.add("aansluiten",NederScriptType.LEEGTE);
        new ParseTreeWalker().walk(this, tree);
    }

    @Override
    public void enterFunction(NederScriptParser.FunctionContext ctx) {
        if (ctx.COLON() != null) {
            this.st.add(ctx.VAR(0).getText(), checker.typeContextToNederScriptType(ctx.type(ctx.type().size() - 1)));
        } else {
           this.st.add(ctx.VAR(0).getText(), NederScriptType.LEEGTE);
        }
    }
}
