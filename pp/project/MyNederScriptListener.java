package pp.project;

import org.antlr.v4.runtime.ParserRuleContext;
import org.antlr.v4.runtime.tree.ErrorNode;
import org.antlr.v4.runtime.tree.ParseTree;
import org.antlr.v4.runtime.tree.ParseTreeProperty;
import org.antlr.v4.runtime.tree.TerminalNode;
import pp.block3.cc.antlr.Type;
import pp.block4.cc.cfg.Node;
import pp.project.exceptions.NoTypeException;
import pp.project.exceptions.TypeMisMatchException;
import pp.project.exceptions.VariableNotExistsException;

import java.util.Stack;

public class MyNederScriptListener extends NederScriptBaseListener {

    SymbolTable st = new MySymbolTable();
    private ParseTreeProperty<NSType> types = new ParseTreeProperty<>();
    private Stack<Exception> errorStack = new Stack<>();


    @Override
    public void enterFunction(NederScriptParser.FunctionContext ctx) {
        st.openScope();
    }

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
    public void enterAssign(NederScriptParser.AssignContext ctx) {
        if (!st.contains(ctx.VAR().toString())) {
            errorStack.add(new VariableNotExistsException("Variable '" + ctx.VAR().toString() + "' does not exists in the current scope"));
        }

    }

    @Override
    public void enterNonTypedDecl(NederScriptParser.NonTypedDeclContext ctx) {
        NSType type = null;
        if (ctx.type().INTEGER() != null) {
            type = NSType.GETAL;
        } else if (ctx.type().BOOLEAN() != null) {
            type = NSType.BOOLEAANS;
        } else if (ctx.type().STRING() != null) {
            type = NSType.TOUW;
        } else if (ctx.type().ARRAY() != null) {
            type = NSType.REEKS;
        } else if (ctx.type().THREAD() != null) {
            type = NSType.DRAAD;
        }
        if (type == null) {
             errorStack.add(new NoTypeException(ctx.type().toString() + " type was not recognized"));
        }
        st.add(ctx.VAR().toString());
    }

    @Override
    public void enterTypedDecl(NederScriptParser.TypedDeclContext ctx) {
        NSType expectedType = null;
        if (ctx.type().INTEGER() != null) {
            expectedType = NSType.GETAL;
        } else if (ctx.type().BOOLEAN() != null) {
            expectedType = NSType.BOOLEAANS;
        } else if (ctx.type().STRING() != null) {
            expectedType = NSType.TOUW;
        } else if (ctx.type().ARRAY() != null) {
            expectedType = NSType.REEKS;
        } else if (ctx.type().THREAD() != null) {
            expectedType = NSType.DRAAD;
        }
        if (expectedType == null) {
            errorStack.add(new NoTypeException(ctx.type().toString() + " type was not recognized"));
        }
        NSType actualType = type(ctx.primitive());
        if (expectedType.equals(actualType)) {
            set(ctx, actualType);
        } else {
            errorStack.add(new TypeMisMatchException("Expected type '" + expectedType + "' but got '" + actualType + "'"));
        }
    }

    @Override
    public void enterStringPrimitive(NederScriptParser.StringPrimitiveContext ctx) {
        set(ctx, NSType.TOUW);
    }

    @Override
    public void enterArrayPrimitive(NederScriptParser.ArrayPrimitiveContext ctx) {
        set(ctx, NSType.REEKS);
    }

    @Override
    public void enterIntegerPrimitive(NederScriptParser.IntegerPrimitiveContext ctx) {
        set(ctx, NSType.GETAL);
    }

    @Override
    public void enterBooleanPrimitive(NederScriptParser.BooleanPrimitiveContext ctx) {
        set(ctx, NSType.BOOLEAANS);
    }

    @Override
    public void visitErrorNode(ErrorNode node) {
        super.visitErrorNode(node);
    }

    /** Sets the type attribute of a given node. */
    private void set(ParseTree node, NSType type) {
        this.types.put(node, type);
    }

    /** Retrieves the type of a given node. */
    public NSType type(ParseTree node) {
        return this.types.get(node);
    }
}
