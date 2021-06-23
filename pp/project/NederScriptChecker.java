package pp.project;

import org.antlr.v4.runtime.ParserRuleContext;
import org.antlr.v4.runtime.Token;
import org.antlr.v4.runtime.tree.ParseTree;
import org.antlr.v4.runtime.tree.ParseTreeWalker;
import org.antlr.v4.runtime.tree.*;
import pp.block5.cc.simple.Type;

import java.util.ArrayList;
import java.util.List;

public class NederScriptChecker extends NederScriptBaseListener {

    private List<String> errors;
    private NederScriptResult result;
    private SymbolTable st;

    public NederScriptResult check(ParseTree tree) throws ParseException {
        this.result = new NederScriptResult();
        this.errors = new ArrayList<>();
        this.st = new ScopeTable();
        new ParseTreeWalker().walk(this, tree);
        if (hasErrors()) {
            throw new ParseException(getErrors());
        }
        return this.result;
    }

    /** Indicates if any errors were encountered in this tree listener. */
    public boolean hasErrors() {
        return !getErrors().isEmpty();
    }

    /** Returns the list of errors collected in this tree listener. */
    public List<String> getErrors() {
        return this.errors;
    }

    @Override
    public void enterFunction(NederScriptParser.FunctionContext ctx) { st.openScope(); }

    @Override
    public void exitFunction(NederScriptParser.FunctionContext ctx) {
        st.closeScope();
    }

    @Override
    public void enterNewScopeInst(NederScriptParser.NewScopeInstContext ctx) {
        st.openScope();
    }

    @Override
    public void exitNewScopeInst(NederScriptParser.NewScopeInstContext ctx) {
        st.closeScope();
    }


    @Override
    public void exitPrefixExpr(NederScriptParser.PrefixExprContext ctx) {
        super.exitPrefixExpr(ctx);
    }

    @Override
    public void exitParExpr(NederScriptParser.ParExprContext ctx) {
        super.exitParExpr(ctx);
    }

    @Override
    public void exitFunCallExpr(NederScriptParser.FunCallExprContext ctx) {
        super.exitFunCallExpr(ctx);
    }

    @Override
    public void exitCompExpr(NederScriptParser.CompExprContext ctx) {
        super.exitCompExpr(ctx);
    }

    @Override
    public void exitPrimitiveExpr(NederScriptParser.PrimitiveExprContext ctx) {
        setType(ctx, getType(ctx.primitive()));
    }

    @Override
    public void exitBoolExpr(NederScriptParser.BoolExprContext ctx) {
        checkType(ctx.expr(0), NederScriptType.BOOLEAANS);
        checkType(ctx.expr(1), NederScriptType.BOOLEAANS);
        setType(ctx, NederScriptType.BOOLEAANS);
    }

    @Override
    public void exitMultExpr(NederScriptParser.MultExprContext ctx) {
        checkType(ctx.expr(0), NederScriptType.GETAL);
        checkType(ctx.expr(1), NederScriptType.GETAL);
        setType(ctx, NederScriptType.GETAL);
    }

    @Override
    public void exitPlusExpr(NederScriptParser.PlusExprContext ctx) {
        super.exitPlusExpr(ctx);
    }

    @Override
    public void exitNonTypedDecl(NederScriptParser.NonTypedDeclContext ctx) {
        String var = ctx.VAR().getText();
        NederScriptType type = typeContextToNederScriptType(ctx.type());
        this.st.add(var, type);
        setOffset(ctx, this.st.getOffset(var));
        System.out.println("Put non typed in table: " + var);
    }

    @Override
    public void exitTypedDecl(NederScriptParser.TypedDeclContext ctx) {
        String var = ctx.VAR().getText();
        NederScriptType type = typeContextToNederScriptType(ctx.type());
        this.st.add(var, type);
        setOffset(ctx, this.st.getOffset(var));
        System.out.println("Put typed in table: " + var);
    }

    @Override
    public void exitForIn(NederScriptParser.ForInContext ctx) {
        String var = ctx.VAR().getText();
        NederScriptType type = typeContextToNederScriptType(ctx.type());
        this.st.add(var, type);
        //TODO wtf waarom werkt het niet
        setOffset(ctx, this.st.getOffset(var));
        System.out.println("Put typed in table: " + var);
    }

    @Override
    public void exitVarExpr(NederScriptParser.VarExprContext ctx) {
        if (ctx.VAR().size() > 1) {
            // this var is a function call
            //TODO
            return;
        }
        String var = ctx.VAR(0).getText();
        if (!this.st.contains(var)) {
            addError(ctx, "Variable '%s' not declared", var);
            return;
        }
        NederScriptType type = this.st.getType(var);
        if (type == null) {
            addError(ctx, "Variable '%s' not initialized", var);
        }
        setType(ctx, type);
    }

    @Override
    public void exitStringPrimitive(NederScriptParser.StringPrimitiveContext ctx) {
        int length = ctx.STR().getText().length() - 2;
        setType(ctx, new NederScriptType.Touw(length));
    }

    @Override
    public void exitCharacterPrimitive(NederScriptParser.CharacterPrimitiveContext ctx) {
        setType(ctx, NederScriptType.KARAKTER);
    }

    @Override
    public void exitArrayPrimitive(NederScriptParser.ArrayPrimitiveContext ctx) {
        int arrayLength = ctx.primitive().size();
        NederScriptType type = getType(ctx.primitive(0));
        for (int i = 0; i < ctx.primitive().size(); i++) {
            NederScriptType curType = getType(ctx.primitive(i));
            if (!type.equals(curType)) {
                addError(ctx,"Not all elements in Reeks have the same type");
            }
        }
        setType(ctx, new NederScriptType.Reeks(arrayLength, type));
    }

    @Override
    public void exitIntegerPrimitive(NederScriptParser.IntegerPrimitiveContext ctx) {
        setType(ctx, NederScriptType.GETAL);
    }

    @Override
    public void exitBooleanPrimitive(NederScriptParser.BooleanPrimitiveContext ctx) {
        setType(ctx, NederScriptType.BOOLEAANS);
    }

    @Override
    public void visitErrorNode(ErrorNode node) {
        addError(node.getSymbol(), "Parser exception");
    }

    /** Records an error at a given parse tree node.
     * @param node the parse tree node at which the error occurred
     * @param message the error message
     * @param args arguments for the message, see {@link String#format}
     */
    private void addError(ParserRuleContext node, String message,
                          Object... args) {
        addError(node.getStart(), message, args);
    }

    /** Records an error at a given token.
     * @param token the token at which the error occurred
     * @param message the error message
     * @param args arguments for the message, see {@link String#format}
     */
    private void addError(Token token, String message, Object... args) {
        int line = token.getLine();
        int column = token.getCharPositionInLine();
        message = String.format(message, args);
        message = String.format("Line %d:%d - %s", line, column, message);
        this.errors.add(message);
    }

    /** Convenience method to add a type to the result. */
    private void setType(ParseTree node, NederScriptType type) {
        this.result.setType(node, type);
    }

    /** Returns the type of a given expression or type node. */
    private NederScriptType getType(ParseTree node) {
        return this.result.getType(node);
    }

    /** Checks the inferred type of a given parse tree,
     * and adds an error if it does not correspond to the expected type.
     */
    private void checkType(ParserRuleContext node, NederScriptType expected) {
        NederScriptType actual = getType(node);
        if (actual == null) {
            addError(node, "Missing inferred type");
            return;
        }
        if (!actual.equals(expected)) {
            addError(node, "Expected type '%s' but found '%s'", expected,
                    actual);
        }
    }

    private NederScriptType typeContextToNederScriptType(NederScriptParser.TypeContext s) {
        if (s.BOOLEAN() != null) {
            return NederScriptType.BOOLEAANS;
        } else if (s.STRING() != null) {
            return NederScriptType.TOUW;
        } else if (s.INTEGER() != null) {
            return NederScriptType.GETAL;
        } else if (s.THREAD() != null) {
            return NederScriptType.DRAAD;
        } else if (s.CHARACTER() != null) {
            return NederScriptType.KARAKTER;
        } else if (s.ARRAY() != null) {
            NederScriptType elemType = typeContextToNederScriptType(s.type());
            return new NederScriptType.Reeks(0, elemType);
        }
        return null;
    }

    /** Convenience method to add a flow graph entry to the result. */
    private void setEntry(ParseTree node, ParserRuleContext entry) {
        if (entry == null) {
            throw new IllegalArgumentException("Null flow graph entry");
        }
        this.result.setEntry(node, entry);
    }

    /** Returns the flow graph entry of a given expression or statement. */
    private ParserRuleContext getEntry(ParseTree node) {
        return this.result.getEntry(node);
    }

    /** Convenience method to add an offset to the result. */
    private void setOffset(ParseTree node, Integer offset) {
        if (offset == null) {
            throw new IllegalArgumentException("null offset");
        }
        this.result.setOffset(node, offset);
    }

    /** Returns the offset of a given expression or statement. */
    private Integer getOffset(ParseTree node) {
        return this.result.getOffset(node);
    }
}
