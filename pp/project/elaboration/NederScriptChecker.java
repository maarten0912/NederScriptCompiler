package pp.project.elaboration;

import org.antlr.v4.runtime.ParserRuleContext;
import org.antlr.v4.runtime.Token;
import org.antlr.v4.runtime.tree.ParseTree;
import org.antlr.v4.runtime.tree.ParseTreeWalker;
import org.antlr.v4.runtime.tree.*;
import pp.project.elaboration.NederScriptBaseListener;
import pp.project.elaboration.NederScriptParser;
import pp.project.exception.ParseException;

import java.util.ArrayList;
import java.util.List;

//TODO: check length of string and arrays

public class NederScriptChecker extends NederScriptBaseListener {

    private List<String> errors;
    private NederScriptResult result;
    private ScopeTable st;
    private ScopeTable globalSt;

    public NederScriptResult check(ParseTree tree) throws ParseException {
        this.result = new NederScriptResult();
        this.errors = new ArrayList<>();
        this.st = new ScopeTable();
        this.globalSt = new ScopeTable();

        new NederScriptFunctionListener().check(this,tree, st);
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
    public void enterFunction(NederScriptParser.FunctionContext ctx) {
        st.openScope();
        globalSt.openScope();
        if (ctx.COLON() != null) {
            // A return type is supplied
            for (int i = 0; i < ctx.type().size() - 1; i++) {
                String varname = ctx.VAR(i + 1).getText();
                this.st.add(varname, typeContextToNederScriptType(ctx.type(i)));
            }
        } else {
            // No return type
            for (int i = 0; i < ctx.type().size(); i++) {
                String varname = ctx.VAR(i + 1).getText();
                this.st.add(varname, typeContextToNederScriptType(ctx.type(i)));
            }
        }
    }

    @Override
    public void exitFunction(NederScriptParser.FunctionContext ctx) {
        st.closeScope();
        globalSt.closeScope();
    }

    @Override
    public void enterNewScopeInst(NederScriptParser.NewScopeInstContext ctx) {
        st.openScope();
        globalSt.openScope();
    }

    @Override
    public void exitNewScopeInst(NederScriptParser.NewScopeInstContext ctx) {
        st.closeScope();
        globalSt.closeScope();
    }

    @Override
    public void exitPrefixExpr(NederScriptParser.PrefixExprContext ctx) {
        setType(ctx, getType(ctx.expr()));
    }

    @Override
    public void exitParExpr(NederScriptParser.ParExprContext ctx) {
        setType(ctx, getType(ctx.expr()));
    }

    @Override
    public void exitFunCallExpr(NederScriptParser.FunCallExprContext ctx) {
        setType(ctx, getType(ctx.funCall()));
    }

    @Override
    public void exitFunCall(NederScriptParser.FunCallContext ctx) {
        //TODO check if number and types of args are correct
        if (!this.st.contains(ctx.VAR().getText())) {
            addError(ctx, "Function '" + ctx.VAR().getText() + "' does not exist.");
        } else {
            setType(ctx,this.st.getType(ctx.VAR().getText()));
        }
    }

    @Override
    public void exitCompExpr(NederScriptParser.CompExprContext ctx) {
        if (ctx.compOp().EQ() != null) {
            checkType(ctx.expr(1), getType(ctx.expr(0)));
        }
        else {
            checkType(ctx.expr(0), NederScriptType.GETAL);
            checkType(ctx.expr(1), NederScriptType.GETAL);
        }
        setType(ctx, NederScriptType.BOOLEAANS);
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
        NederScriptType type = getType(ctx.expr(0));

        if (type instanceof NederScriptType.Touw) {
            if (!NederScriptType.GETAL.equals(getType(ctx.expr(1)))) {
                checkType(ctx.expr(1), new NederScriptType.Touw(0));
            }
            setType(ctx, getType(ctx.expr(0)));
        } else if (type instanceof NederScriptType.Getal) {
            if (!new NederScriptType.Touw(0).equals(getType(ctx.expr(1)))) {
                checkType(ctx.expr(1), NederScriptType.GETAL);
            }
            setType(ctx, getType(ctx.expr(1)));
        } else {
            addError(ctx, "Found incompatible types for %s operation: %s and %s",ctx.plusOp().getText(), getType(ctx.expr(0)), getType(ctx.expr(1)));
        }
    }

    @Override
    public void exitNonTypedDecl(NederScriptParser.NonTypedDeclContext ctx) {
        String var = ctx.VAR().getText();
        NederScriptType type;
        if (ctx.type().STRING() != null) {
            int len = Integer.parseInt(ctx.type().NUM().getText());
            type = new NederScriptType.Touw(len);
        } else if (ctx.type().ARRAY() != null) {
            NederScriptType elemType = typeContextToNederScriptType(ctx.type().type());
            int len = Integer.parseInt(ctx.type().NUM().getText());
            type = new NederScriptType.Reeks(elemType, len);
        } else {
            type = typeContextToNederScriptType(ctx.type());
        }

        if (ctx.PUBLIC() != null) {
            this.globalSt.add(var, type);
            setOffset(ctx, this.globalSt.getOffset(var));
            setPublic(ctx, true);
        } else {
            this.st.add(var, type);
            setOffset(ctx, this.st.getOffset(var));
            setPublic(ctx, false);
        }
    }

    @Override
    public void exitTypedDecl(NederScriptParser.TypedDeclContext ctx) {
        String var = ctx.VAR().getText();
        NederScriptType type;
        if (ctx.type().STRING() != null) {
            int strLen = getType(ctx.expr()).size() - 1;
            int expectedLen = Integer.parseInt(ctx.type().NUM().getText());
            if (strLen != expectedLen) {
                addError(ctx, "Lengths of %s do not match: expected %s but found %s", var, expectedLen, strLen);
            }
            type = new NederScriptType.Touw(strLen);
        } else if (ctx.type().ARRAY() != null) {
            int arrLen = getType(ctx.expr()).size() - 1;
            int expectedLen = Integer.parseInt(ctx.type().NUM().getText());
            if (arrLen != expectedLen) {
                addError(ctx, "Lengths of %s do not match: expected %s but found %s", var, expectedLen, arrLen);
            }
            NederScriptType elemType = ((NederScriptType.Reeks) getType(ctx.expr())).getElemType();
            if (ctx.type().type().ARRAY() != null || ctx.type().type().STRING() != null) {
                addError(ctx, "Nested array are currently not supported");
                return;
            }
            NederScriptType expectedElemType = typeContextToNederScriptType(ctx.type().type());
            if (elemType != expectedElemType) {
                addError(ctx, "Reeks element types of %s do not match: expected %s but found %s", var, expectedElemType, elemType);
            }
            type = new NederScriptType.Reeks(elemType, arrLen);
        } else {
            type = typeContextToNederScriptType(ctx.type());
        }
        if (ctx.PUBLIC() != null) {
            this.globalSt.add(var, type);
            setOffset(ctx, this.globalSt.getOffset(var));
            setPublic(ctx, true);
        } else {
            this.st.add(var, type);
            setOffset(ctx, this.st.getOffset(var));
            setPublic(ctx, false);
        }
        checkType(ctx.expr(), type);
        setType(ctx, type);

    }

    @Override
    public void exitIfelse(NederScriptParser.IfelseContext ctx) {
        if (getType(ctx.expr()) != NederScriptType.BOOLEAANS) {
            addError(ctx, "If statement requires Booleaans type: '%s', found type '%s'", ctx.expr().getText(), getType(ctx.expr()));
        }
    }

    @Override
    public void enterIfelseInst(NederScriptParser.IfelseInstContext ctx) {
        st.openScope();
        globalSt.openScope();
    }

    @Override
    public void exitIfelseInst(NederScriptParser.IfelseInstContext ctx) {
        st.closeScope();
        globalSt.closeScope();
    }

    @Override
    public void enterWhileInst(NederScriptParser.WhileInstContext ctx) {
        st.openScope();
        globalSt.openScope();
    }

    @Override
    public void exitWhileInst(NederScriptParser.WhileInstContext ctx) {
        st.closeScope();
        globalSt.closeScope();
    }

    @Override
    public void enterForInst(NederScriptParser.ForInstContext ctx) {
        st.openScope();
        globalSt.openScope();
    }

    @Override
    public void exitForInst(NederScriptParser.ForInstContext ctx) {
        st.closeScope();
        globalSt.closeScope();
    }



    @Override
    public void exitVarExpr(NederScriptParser.VarExprContext ctx) {

        String var = ctx.VAR().getText();
        if (!this.st.contains(var)) {
            addError(ctx, "Variable '%s' not declared", var);
            return;
        }

        if (ctx.expr().size() > 0) {
            // this var is a list and an index is supplied

            NederScriptType resType = this.st.getType(ctx.VAR().getText());
            setType(ctx.VAR(), resType);
            setType(ctx, resType);

            for (int i = 0; i < ctx.expr().size(); i++) {
                NederScriptType type = getType(ctx.expr(i));
                setOffset(ctx.expr(i),getOffset(ctx.expr(i)));
                if (!type.equals(NederScriptType.GETAL)) {
                    addError(ctx.expr(i), "Expected type 'Getal' but was '%s'",type);
                    return;
                }
                if (resType instanceof NederScriptType.Reeks) {
                    resType = ((NederScriptType.Reeks) resType).getElemType();
                } else if (resType instanceof NederScriptType.Touw) {
                    resType = NederScriptType.KARAKTER;
                } else {
                    addError(ctx, "Expected type 'Reeks' but was '%s'",resType);
                }
            }

            setOffset(ctx, this.st.getOffset(ctx.VAR().getText()));
            setOffset(ctx.VAR(), this.st.getOffset(ctx.VAR().getText()));

        } else {
            NederScriptType type = this.st.getType(ctx.VAR().getText());
            if (type == null) {
                addError(ctx, "Variable '%s' not initialized", ctx.VAR().getText());
            }
            setType(ctx, type);
            setType(ctx.VAR(), type);
            int off = this.st.getOffset(ctx.VAR().getText());
            setOffset(ctx, off);
        }

        System.out.println(String.format("[exitVarExpr] Found variable %s of type %s with offset %s", ctx.VAR().getText(),this.result.getType(ctx.VAR()),this.result.getOffset(ctx)));


    }

    @Override
    public void exitAssign(NederScriptParser.AssignContext ctx) {
        if (ctx.expr().size() > 1) {
            //this var is an array with index

            NederScriptType resType = this.st.getType(ctx.VAR().getText());
            setType(ctx, resType);
            setType(ctx.VAR(), resType);
            for (int i = 0; i < ctx.expr().size() - 1; i++) {
                NederScriptType type = getType(ctx.expr(i));
                if (!type.equals(NederScriptType.GETAL)) {
                    addError(ctx, "Expected type 'Getal' but was '%s'",type);
                    return;
                }
                if (resType instanceof NederScriptType.Reeks) {
                    resType = ((NederScriptType.Reeks) resType).getElemType();
                } else if (resType instanceof NederScriptType.Touw) {
                    resType = NederScriptType.KARAKTER;
                } else {
                    addError(ctx, "Expected type 'Reeks' but was '%s'",resType);
                }
            }

            checkType(ctx.expr(ctx.expr().size() - 1),resType);
            setOffset(ctx.VAR(), this.st.getOffset(ctx.VAR().getText()));
        } else {
            String varName = ctx.VAR().getText();
            if (this.st.contains(varName)) {
                NederScriptType varType = this.st.getType(ctx.VAR().getText());
                checkType(ctx.expr(ctx.expr().size() - 1),varType);
            } else {
                addError(ctx, "Variable '%s' not declared", varName);
            }
            setOffset(ctx, this.st.getOffset(ctx.VAR().getText()));
            setType(ctx, getType(ctx.expr(ctx.expr().size() - 1)));
        }

        System.out.println(String.format("[exitAssign] Found variable %s of type %s", ctx.VAR().getText(),this.result.getType(ctx)));


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
        int elemNumber = 0;
        NederScriptType type = null;
        if (ctx.primitive().size() > 0) {
            type = getType(ctx.primitive(0));
        } else if (ctx.VAR().size() > 0) {
            String var = ctx.VAR(0).getText();
            if (this.st.contains(var)) {
                type = this.st.getType(var);
            } else {
                addError(ctx, "Variable '%s' not declared", var);
            }
        }

        for (int i = 0; i < ctx.primitive().size(); i++) {
            NederScriptType curType = getType(ctx.primitive(i));
            elemNumber++;
            assert type != null;
            if (!type.equals(curType)) {
                addError(ctx,"Not all elements in Reeks have the same type. Expected type '%s' while type was '%s'", type, curType);
            }
        }
        for (int i = 0; i < ctx.VAR().size(); i++) {
            elemNumber++;
            String var = ctx.VAR(i).getText();
            if (this.st.contains(var)) {
                NederScriptType curType = this.st.getType(var);
                assert type != null;
                if (!type.equals(curType)) {
                    addError(ctx,"Not all elements in Reeks have the same type. Expected type '%s' while type was '%s'", type, curType);
                }
            } else {
                addError(ctx, "Variable '%s' not declared", var);
            }
        }
        setType(ctx, new NederScriptType.Reeks(type, elemNumber));

        for (int i = 0; i < ctx.VAR().size(); i++) {
            int off = this.st.getOffset(ctx.VAR(i).getText());
            setOffset(ctx.VAR(i), off);
        }


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
    public void enterThreadInst(NederScriptParser.ThreadInstContext ctx) {
        st.openScope();
        globalSt.openScope();
        this.result.addThread();

        if (this.result.getNumThreads() > 4) {
            addError(ctx,"A maximum of 4 threads can be created.");
        }
    }

    @Override
    public void exitThreadInst(NederScriptParser.ThreadInstContext ctx) {
        st.closeScope();
        globalSt.closeScope();
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
    public void setType(ParseTree node, NederScriptType type) {
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
            System.out.println(node.getText());
            addError(node, "Expected type '%s' but found '%s'", expected,
                    actual);
        }
    }

    public NederScriptType typeContextToNederScriptType(NederScriptParser.TypeContext s) {

        //These two types require more info to create a type
        assert s.STRING() == null;
        assert s.ARRAY() == null;

        if (s.BOOLEAN() != null) {
            return NederScriptType.BOOLEAANS;
        } else if (s.INTEGER() != null) {
            return NederScriptType.GETAL;
        } else if (s.CHARACTER() != null) {
            return NederScriptType.KARAKTER;
        }
        return null;
    }

    /** Convenience method to add an offset to the result. */
    private void setOffset(ParseTree node, Integer offset) {
        if (offset == null) {
            throw new IllegalArgumentException("null offset");
        }
        this.result.setOffset(node, offset);
    }

    private void setPublic(ParseTree node, Boolean isPublic) {
        if (isPublic == null) {
            throw new IllegalArgumentException("null boolean");
        }
        this.result.setPublic(node, isPublic);
    }

    /** Returns the offset of a given expression or statement. */
    private Integer getOffset(ParseTree node) {
        return this.result.getOffset(node);
    }
}
