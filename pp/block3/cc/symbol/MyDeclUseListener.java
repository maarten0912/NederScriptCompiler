package pp.block3.cc.symbol;

import org.antlr.v4.runtime.tree.ErrorNode;
import org.antlr.v4.runtime.tree.ParseTreeProperty;
import pp.block3.cc.antlr.Type;

import java.util.Stack;

public class MyDeclUseListener extends DeclUseBaseListener {

    private Stack<String> errorstack = new Stack<>();
    private SymbolTable st = new MySymbolTable();

    public Stack<String> getErrorstack() {
        return errorstack;
    }

    @Override
    public void exitProgram(DeclUseParser.ProgramContext ctx) {
        if (errorstack.size() > 0) {
            System.err.println("Could not parse the program:");
            for (String s : errorstack) {
                System.err.println(s);
            }
            System.exit(2);
        }
    }

    @Override
    public void enterUnit(DeclUseParser.UnitContext ctx) {
        if (ctx.series() != null) {
            st.openScope();
        }
    }

    @Override
    public void exitUnit(DeclUseParser.UnitContext ctx) {
        if (ctx.series() != null) {
            st.closeScope();
        }
    }

    @Override
    public void exitDecl(DeclUseParser.DeclContext ctx) {
        if (!st.add(ctx.ID().toString())) {
            String errmsg = "Identifier has been redeclared in the same scope, at " + ctx.ID().getSymbol().getLine() + ":" + ctx.ID().getSymbol().getCharPositionInLine();
            errorstack.add(errmsg);
        }
    }

    @Override
    public void enterUse(DeclUseParser.UseContext ctx) {
        System.out.println("Enter: " + ctx.ID());

    }

    @Override
    public void exitUse(DeclUseParser.UseContext ctx) {
        System.out.println("Exit: " + ctx.ID());
        if (!st.contains(ctx.ID().toString())) {
            String errmsg = "Identifier has not been declared before, at" + ctx.ID().getSymbol().getLine() + ":" + ctx.ID().getSymbol().getCharPositionInLine();
            errorstack.add(errmsg);
        }
    }

    @Override
    public void visitErrorNode(ErrorNode node) {
        String errmsg = "Lexer error in line " + node.getSymbol().getLine() + ":" + node.getSymbol().getCharPositionInLine();
        errorstack.add(errmsg);
    }
}
