package pp.block3.cc.symbol;

import org.antlr.v4.runtime.ParserRuleContext;

public class MyDeclUseListener extends DeclUseBaseListener{



    @Override
    public void enterProgram(DeclUseParser.ProgramContext ctx) {
        super.enterProgram(ctx);
    }

    @Override
    public void enterSeries(DeclUseParser.SeriesContext ctx) {
        super.enterSeries(ctx);
    }

    @Override
    public void enterUnit(DeclUseParser.UnitContext ctx) {
        super.enterUnit(ctx);
    }

    @Override
    public void enterDecl(DeclUseParser.DeclContext ctx) {
        super.enterDecl(ctx);
    }

    @Override
    public void enterUse(DeclUseParser.UseContext ctx) {
        super.enterUse(ctx);
    }

    @Override
    public void enterEveryRule(ParserRuleContext ctx) {
        super.enterEveryRule(ctx);
    }

    @Override
    public void exitEveryRule(ParserRuleContext ctx) {
        super.exitEveryRule(ctx);
    }

    @Override
    public void exitProgram(DeclUseParser.ProgramContext ctx) {
        super.exitProgram(ctx);
    }

    @Override
    public void exitSeries(DeclUseParser.SeriesContext ctx) {
        super.exitSeries(ctx);
    }

    @Override
    public void exitUnit(DeclUseParser.UnitContext ctx) {
        super.exitUnit(ctx);
    }

    @Override
    public void exitDecl(DeclUseParser.DeclContext ctx) {
        super.exitDecl(ctx);
    }

    @Override
    public void exitUse(DeclUseParser.UseContext ctx) {
        super.exitUse(ctx);
    }
}
