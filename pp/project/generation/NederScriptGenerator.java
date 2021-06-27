package pp.project.generation;

import org.antlr.v4.runtime.*;
import org.antlr.v4.runtime.tree.ParseTree;
import org.antlr.v4.runtime.tree.ParseTreeProperty;
import pp.block5.cc.ErrorListener;
import pp.block5.cc.ParseException;
import pp.block5.cc.antlr.BuildingLexer;
import pp.block5.cc.antlr.BuildingParser;
import pp.iloc.model.Label;
import pp.iloc.model.Reg;
import pp.project.elaboration.NederScriptBaseVisitor;
import pp.project.elaboration.NederScriptParser;
import pp.project.elaboration.NederScriptResult;

public class NederScriptGenerator extends NederScriptBaseVisitor<NederScriptInstruction> {

    private NederScriptResult result;
    private NederScriptProgram prog;
    /** Register count, used to generate fresh registers. */
    private int regCount;
    /** Association of expression and target nodes to registers. */
    private ParseTreeProperty<Reg> regs;
    /** Association of statement nodes to labels. */
    private ParseTreeProperty<Label> labels;

    public NederScriptProgram generate(ParseTree tree, NederScriptResult result) {
        this.result = result;
        this.prog = new NederScriptProgram();
        this.regs = new ParseTreeProperty<>();
        this.labels = new ParseTreeProperty<>();
        this.regCount = 0;
        tree.accept(this);
//        this.makeFibonacci();
        return this.prog;
    }

    @Override
    public NederScriptInstruction visitProgram(NederScriptParser.ProgramContext ctx) {
        System.out.println("Visited program");
        return super.visitProgram(ctx);
    }

    @Override
    public NederScriptInstruction visitFunction(NederScriptParser.FunctionContext ctx) {
        System.out.println("Visited program");
        return new NederScriptInstruction.Branch(1,new NederScriptTarget.Rel(2));

    }

    @Override
    public NederScriptInstruction visitNormalInst(NederScriptParser.NormalInstContext ctx) {
        return super.visitNormalInst(ctx);
    }

    @Override
    public NederScriptInstruction visitIfelseInst(NederScriptParser.IfelseInstContext ctx) {
        return super.visitIfelseInst(ctx);
    }

    @Override
    public NederScriptInstruction visitWhileInst(NederScriptParser.WhileInstContext ctx) {
        return super.visitWhileInst(ctx);
    }

    @Override
    public NederScriptInstruction visitForInst(NederScriptParser.ForInstContext ctx) {
        return super.visitForInst(ctx);
    }

    @Override
    public NederScriptInstruction visitNewScopeInst(NederScriptParser.NewScopeInstContext ctx) {
        return super.visitNewScopeInst(ctx);
    }

    @Override
    public NederScriptInstruction visitAssignStat(NederScriptParser.AssignStatContext ctx) {
        return super.visitAssignStat(ctx);
    }

    @Override
    public NederScriptInstruction visitDeclStat(NederScriptParser.DeclStatContext ctx) {
        return super.visitDeclStat(ctx);
    }

    @Override
    public NederScriptInstruction visitReturnStat(NederScriptParser.ReturnStatContext ctx) {
        return super.visitReturnStat(ctx);
    }

    @Override
    public NederScriptInstruction visitPrintStat(NederScriptParser.PrintStatContext ctx) {
        return super.visitPrintStat(ctx);
    }

    @Override
    public NederScriptInstruction visitFunctionCallStat(NederScriptParser.FunctionCallStatContext ctx) {
        return super.visitFunctionCallStat(ctx);
    }

    @Override
    public NederScriptInstruction visitIfelse(NederScriptParser.IfelseContext ctx) {
        return super.visitIfelse(ctx);
    }

    @Override
    public NederScriptInstruction visitWhileS(NederScriptParser.WhileSContext ctx) {
        return super.visitWhileS(ctx);
    }

    @Override
    public NederScriptInstruction visitForNormal(NederScriptParser.ForNormalContext ctx) {
        return super.visitForNormal(ctx);
    }

    @Override
    public NederScriptInstruction visitForIn(NederScriptParser.ForInContext ctx) {
        return super.visitForIn(ctx);
    }

    @Override
    public NederScriptInstruction visitAssign(NederScriptParser.AssignContext ctx) {
        return super.visitAssign(ctx);
    }

    @Override
    public NederScriptInstruction visitNonTypedDecl(NederScriptParser.NonTypedDeclContext ctx) {
        return super.visitNonTypedDecl(ctx);
    }

    @Override
    public NederScriptInstruction visitTypedDecl(NederScriptParser.TypedDeclContext ctx) {
        return super.visitTypedDecl(ctx);
    }

    @Override
    public NederScriptInstruction visitReturnS(NederScriptParser.ReturnSContext ctx) {
        return super.visitReturnS(ctx);
    }

    @Override
    public NederScriptInstruction visitPrint(NederScriptParser.PrintContext ctx) {
        return super.visitPrint(ctx);
    }

    @Override
    public NederScriptInstruction visitFunCall(NederScriptParser.FunCallContext ctx) {
        return super.visitFunCall(ctx);
    }

    @Override
    public NederScriptInstruction visitStringPrimitive(NederScriptParser.StringPrimitiveContext ctx) {
        return super.visitStringPrimitive(ctx);
    }

    @Override
    public NederScriptInstruction visitCharacterPrimitive(NederScriptParser.CharacterPrimitiveContext ctx) {
        return super.visitCharacterPrimitive(ctx);
    }

    @Override
    public NederScriptInstruction visitArrayPrimitive(NederScriptParser.ArrayPrimitiveContext ctx) {
        return super.visitArrayPrimitive(ctx);
    }

    @Override
    public NederScriptInstruction visitIntegerPrimitive(NederScriptParser.IntegerPrimitiveContext ctx) {
        return super.visitIntegerPrimitive(ctx);
    }

    @Override
    public NederScriptInstruction visitBooleanPrimitive(NederScriptParser.BooleanPrimitiveContext ctx) {
        return super.visitBooleanPrimitive(ctx);
    }

    @Override
    public NederScriptInstruction visitType(NederScriptParser.TypeContext ctx) {
        return super.visitType(ctx);
    }

    @Override
    public NederScriptInstruction visitPrefixExpr(NederScriptParser.PrefixExprContext ctx) {
        return super.visitPrefixExpr(ctx);
    }

    @Override
    public NederScriptInstruction visitParExpr(NederScriptParser.ParExprContext ctx) {
        return super.visitParExpr(ctx);
    }

    @Override
    public NederScriptInstruction visitVarExpr(NederScriptParser.VarExprContext ctx) {
        return super.visitVarExpr(ctx);
    }

    @Override
    public NederScriptInstruction visitFunCallExpr(NederScriptParser.FunCallExprContext ctx) {
        return super.visitFunCallExpr(ctx);
    }

    @Override
    public NederScriptInstruction visitCompExpr(NederScriptParser.CompExprContext ctx) {
        return super.visitCompExpr(ctx);
    }

    @Override
    public NederScriptInstruction visitPrimitiveExpr(NederScriptParser.PrimitiveExprContext ctx) {
        return super.visitPrimitiveExpr(ctx);
    }

    @Override
    public NederScriptInstruction visitBoolExpr(NederScriptParser.BoolExprContext ctx) {
        return super.visitBoolExpr(ctx);
    }

    @Override
    public NederScriptInstruction visitMultExpr(NederScriptParser.MultExprContext ctx) {
        return super.visitMultExpr(ctx);
    }

    @Override
    public NederScriptInstruction visitPlusExpr(NederScriptParser.PlusExprContext ctx) {
        return super.visitPlusExpr(ctx);
    }

    @Override
    public NederScriptInstruction visitPrefixOp(NederScriptParser.PrefixOpContext ctx) {
        return super.visitPrefixOp(ctx);
    }

    @Override
    public NederScriptInstruction visitMultOp(NederScriptParser.MultOpContext ctx) {
        return super.visitMultOp(ctx);
    }

    @Override
    public NederScriptInstruction visitPlusOp(NederScriptParser.PlusOpContext ctx) {
        return super.visitPlusOp(ctx);
    }

    @Override
    public NederScriptInstruction visitBoolOp(NederScriptParser.BoolOpContext ctx) {
        return super.visitBoolOp(ctx);
    }

    @Override
    public NederScriptInstruction visitCompOp(NederScriptParser.CompOpContext ctx) {
        return super.visitCompOp(ctx);
    }

    public void makeFibonacci() {
        prog.addInstruction(new NederScriptInstruction.Debug("Beginning program!!!"));
        prog.addInstruction(new NederScriptInstruction.ReadInstr(new NederScriptAddrImmDI.NederScriptDirAddr(65536)));
        prog.addInstruction(new NederScriptInstruction.Receive(6));

        prog.addInstruction(new NederScriptInstruction.Load(new NederScriptAddrImmDI.NederScriptImmValue(0),2));
        prog.addInstruction(new NederScriptInstruction.Load(new NederScriptAddrImmDI.NederScriptImmValue(1),3));

        prog.addInstruction(new NederScriptInstruction.Compute(NederScriptOperator.Gt,2,6,4));
        prog.addInstruction(new NederScriptInstruction.Branch(4, new NederScriptTarget.Abs(13)));
        prog.addInstruction(new NederScriptInstruction.WriteInstr(2, new NederScriptAddrImmDI.NederScriptDirAddr(65536)));
        prog.addInstruction(new NederScriptInstruction.Compute(NederScriptOperator.Add,2,3,2));
        prog.addInstruction(new NederScriptInstruction.Compute(NederScriptOperator.Gt,3,6,4));
        prog.addInstruction(new NederScriptInstruction.Branch(4, new NederScriptTarget.Abs(13)));
        prog.addInstruction(new NederScriptInstruction.WriteInstr(3, new NederScriptAddrImmDI.NederScriptDirAddr(65536)));
        prog.addInstruction(new NederScriptInstruction.Compute(NederScriptOperator.Add,2,3,3));
        prog.addInstruction(new NederScriptInstruction.Jump(new NederScriptTarget.Rel(-8)));

        prog.addInstruction(new NederScriptInstruction.EndProg());

        prog.incrementThreadNumber();
        // These two lines are already default, but it is just for demonstration purposes
        prog.setDebugMode(false);
        prog.setDebugFunction("debuggerSimplePrintAndWait");
    }


}
