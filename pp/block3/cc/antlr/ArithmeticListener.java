package pp.block3.cc.antlr;

import org.antlr.v4.runtime.ParserRuleContext;

import pp.block2.cc.antlr.ArithmeticBaseListener;
import pp.block2.cc.antlr.ArithmeticParser;
import org.antlr.v4.runtime.tree.ParseTree;
import org.antlr.v4.runtime.tree.ParseTreeProperty;


public class ArithmeticListener extends SimpleArithmeticBaseListener {

    private ParseTreeProperty<Type> types;

    /** Initialises the calculator before using it to walk a tree. */
    public void init() {
        this.types = new ParseTreeProperty<Type>();
    }

    @Override
    public void exitNumber(SimpleArithmeticParser.NumberContext ctx) {
        super.exitNumber(ctx);
    }

    @Override
    public void exitBool(SimpleArithmeticParser.BoolContext ctx) {
        super.exitBool(ctx);
    }

    @Override
    public void exitString(SimpleArithmeticParser.StringContext ctx) {
        super.exitString(ctx);
    }


    @Override
    public void exitEquals(SimpleArithmeticParser.EqualsContext ctx) {
        super.exitEquals(ctx);
    }


    @Override
    public void exitBracket(SimpleArithmeticParser.BracketContext ctx) {
        super.exitBracket(ctx);
    }


    @Override
    public void exitHat(SimpleArithmeticParser.HatContext ctx) {
//        set( ctx, Math.pow(val(ctx.t(0)), val(ctx.t(1))));
    }


    @Override
    public void exitPlus(SimpleArithmeticParser.PlusContext ctx) {
        if (type(ctx.t(0)) == type(ctx.t(1)) {
            set( ctx, val(ctx.t(0)) + val(ctx.t(1)));
        } else {

        }
    }

    /** Sets the type attribute of a given node. */
    private void set(ParseTree node, Type type) {
        this.types.put(node, type);
    }

    /** Retrieves the type of a given node. */
    public Type type(ParseTree node) {
        return this.types.get(node);
    }
}
