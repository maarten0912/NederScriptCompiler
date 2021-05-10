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
        set(ctx, Type.NUM);
    }

    @Override
    public void exitBool(SimpleArithmeticParser.BoolContext ctx) {
        set(ctx, Type.BOOL);
    }

    @Override
    public void exitString(SimpleArithmeticParser.StringContext ctx) {
        set(ctx, Type.STR);
    }


    @Override
    public void exitEquals(SimpleArithmeticParser.EqualsContext ctx) {
        if (type(ctx.t(0)) == type(ctx.t(1))) {
            set( ctx, Type.BOOL);
        } else {
            set (ctx, Type.ERR);
        }
    }


    @Override
    public void exitBracket(SimpleArithmeticParser.BracketContext ctx) {
        set(ctx, type(ctx.t()));
    }


    @Override
    public void exitHat(SimpleArithmeticParser.HatContext ctx) {
        if (type(ctx.t(1)) == Type.NUM && type(ctx.t(0)) != Type.BOOL) {
            set( ctx, type(ctx.t(0)));
        } else {
            set (ctx, Type.ERR);
        }
    }


    @Override
    public void exitPlus(SimpleArithmeticParser.PlusContext ctx) {
        if (type(ctx.t(0)) == type(ctx.t(1))) {
            set( ctx, type(ctx.t(0)));
        } else {
            set (ctx, Type.ERR);
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
