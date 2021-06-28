package pp.project.generation;

import org.antlr.v4.runtime.*;
import org.antlr.v4.runtime.tree.ParseTree;
import org.antlr.v4.runtime.tree.ParseTreeProperty;
import pp.iloc.model.Label;
import pp.iloc.model.Reg;
import pp.project.elaboration.NederScriptBaseVisitor;
import pp.project.elaboration.NederScriptParser;
import pp.project.elaboration.NederScriptResult;
import pp.project.elaboration.NederScriptType;
import pp.project.exception.ParseException;

import java.net.NetworkInterface;
import java.util.ArrayList;
import java.util.List;

public class NederScriptGenerator extends NederScriptBaseVisitor<List<NederScriptInstruction>> {

    private List<String> errs = new ArrayList<>();
    private NederScriptResult result;
    private NederScriptProgram prog;
    /** Register count, used to generate fresh registers. */
    private int regCount;
    /** Association of expression and target nodes to registers. */
    private ParseTreeProperty<Reg> regs;
    /** Association of statement nodes to labels. */
    private ParseTreeProperty<Label> labels;

    public NederScriptProgram generate(ParseTree tree, NederScriptResult result) throws ParseException {
        this.result = result;
        this.prog = new NederScriptProgram();
        this.regs = new ParseTreeProperty<>();
        this.labels = new ParseTreeProperty<>();
        this.regCount = 0;
        tree.accept(this);
//        this.makeFibonacci();
        if (errs.size() > 0) {
            throw new ParseException(errs);
        }
        return this.prog;
    }

    @Override
    public List<NederScriptInstruction> visitProgram(NederScriptParser.ProgramContext ctx) {
        System.out.println("Visit program");

        List<NederScriptInstruction> instList = new ArrayList<>();

        for (NederScriptParser.FunctionContext f : ctx.function()) {
            if (f.VAR(0).getText().equals("hoofd")) {
                List<NederScriptInstruction> newList = visit(f);
                if (newList != null) {
                    instList.addAll(newList);
                }
            }
        }
        instList.add(new NederScriptInstruction.EndProg());
        return instList;
    }

    @Override
    public List<NederScriptInstruction> visitFunction(NederScriptParser.FunctionContext ctx) {
        System.out.println("Visit function");
        //TODO put function args on stack
        //TODO create new AR
        List<NederScriptInstruction> instList = new ArrayList<>();
        for (NederScriptParser.InstructionContext i : ctx.instruction()) {
            List<NederScriptInstruction> newList = visit(i);
            if (newList != null) {
                instList.addAll(newList);
            }
        }
        return instList;
    }

    @Override
    public List<NederScriptInstruction> visitNormalInst(NederScriptParser.NormalInstContext ctx) {
        System.out.println("Visit normal instruction");
        //TODO put function args on stack

        return visit(ctx.statement());
    }

    @Override
    public List<NederScriptInstruction> visitIfelseInst(NederScriptParser.IfelseInstContext ctx) {
        //TODO
        return null;
    }

    @Override
    public List<NederScriptInstruction> visitWhileInst(NederScriptParser.WhileInstContext ctx) {
        //TODO
        return null;
    }

    @Override
    public List<NederScriptInstruction> visitForInst(NederScriptParser.ForInstContext ctx) {
        //TODO
        return null;
    }

    @Override
    public List<NederScriptInstruction> visitNewScopeInst(NederScriptParser.NewScopeInstContext ctx) {
        //TODO
        return null;
    }

    @Override
    public List<NederScriptInstruction> visitAssignStat(NederScriptParser.AssignStatContext ctx) {
        //TODO
        return null;
    }

    @Override
    public List<NederScriptInstruction> visitDeclStat(NederScriptParser.DeclStatContext ctx) {
        //TODO
        return null;
    }

    @Override
    public List<NederScriptInstruction> visitReturnStat(NederScriptParser.ReturnStatContext ctx) {
        //TODO
        return null;
    }


    @Override
    public List<NederScriptInstruction> visitFunctionCallStat(NederScriptParser.FunctionCallStatContext ctx) {
        System.out.println("Visit function call statement");
        //TODO put function args on stack

        return visit(ctx.funCall());
    }

    @Override
    public List<NederScriptInstruction> visitPlusExpr(NederScriptParser.PlusExprContext ctx) {
        System.out.println("Visit plus expr");
        List<NederScriptInstruction> newInst = new ArrayList<>();
        newInst.addAll(visit(ctx.expr(0)));
        newInst.addAll(visit(ctx.expr(1)));
        newInst.addAll(visit(ctx.plusOp()));

        return newInst;
    }

    @Override
    public List<NederScriptInstruction> visitPrefixExpr(NederScriptParser.PrefixExprContext ctx) {
        List<NederScriptInstruction> newInst = new ArrayList<>();
        newInst.addAll(visit(ctx.expr()));
        newInst.addAll(visit(ctx.prefixOp()));
        return newInst;
    }

    @Override
    public List<NederScriptInstruction> visitParExpr(NederScriptParser.ParExprContext ctx) {
        return visit(ctx.expr());
    }

    @Override
    public List<NederScriptInstruction> visitVarExpr(NederScriptParser.VarExprContext ctx) {
        //TODO
        return null;
    }

    @Override
    public List<NederScriptInstruction> visitFunCallExpr(NederScriptParser.FunCallExprContext ctx) {
        //TODO
        return null;
    }

    @Override
    public List<NederScriptInstruction> visitCompExpr(NederScriptParser.CompExprContext ctx) {
        List<NederScriptInstruction> newInst = new ArrayList<>();
        newInst.addAll(visit(ctx.expr(0)));
        newInst.addAll(visit(ctx.expr(1)));
        newInst.addAll(visit(ctx.compOp()));
        return newInst;
    }

    @Override
    public List<NederScriptInstruction> visitPrimitiveExpr(NederScriptParser.PrimitiveExprContext ctx) {
        return visit(ctx.primitive());
    }

    @Override
    public List<NederScriptInstruction> visitBoolExpr(NederScriptParser.BoolExprContext ctx) {
        List<NederScriptInstruction> newInst = new ArrayList<>();
        newInst.addAll(visit(ctx.expr(0)));
        newInst.addAll(visit(ctx.expr(1)));
        newInst.addAll(visit(ctx.boolOp()));
        return newInst;
    }

    @Override
    public List<NederScriptInstruction> visitMultExpr(NederScriptParser.MultExprContext ctx) {
        System.out.println("Visit mult expr");
        List<NederScriptInstruction> newInst = new ArrayList<>();
        newInst.addAll(visit(ctx.expr(0)));
        newInst.addAll(visit(ctx.expr(1)));
        newInst.addAll(visit(ctx.multOp()));
        return newInst;
    }

    @Override
    public List<NederScriptInstruction> visitPrefixOp(NederScriptParser.PrefixOpContext ctx) {
;       List<NederScriptInstruction> newInst = new ArrayList<>();
        if (ctx.NOT() != null) {
            newInst.add(new NederScriptInstruction.Pop(2));
            newInst.add(new NederScriptInstruction.Load(new NederScriptAddrImmDI.NederScriptImmValue(1),3));
            newInst.add(new NederScriptInstruction.Compute(NederScriptOperator.Sub, 3,2,4));
            newInst.add(new NederScriptInstruction.Push(4));
        } else {
            newInst.add(new NederScriptInstruction.Pop(2));
            newInst.add(new NederScriptInstruction.Load(new NederScriptAddrImmDI.NederScriptImmValue(-1),3));
            newInst.add(new NederScriptInstruction.Compute(NederScriptOperator.Mul, 2,3,4));
            newInst.add(new NederScriptInstruction.Push(4));
        }
        return newInst;
    }

    @Override
    public List<NederScriptInstruction> visitMultOp(NederScriptParser.MultOpContext ctx) {
        List<NederScriptInstruction> newInst = new ArrayList<>();
        if (ctx.STAR() != null) {
            newInst.add(new NederScriptInstruction.Pop(2));
            newInst.add(new NederScriptInstruction.Pop(3));
            newInst.add(new NederScriptInstruction.Compute(NederScriptOperator.Mul, 2,3,4));
            newInst.add(new NederScriptInstruction.Push(4));

        } else {
            errs.add("Division is currently not working. We are sorry.");
        }
        return newInst;
    }

    @Override
    public List<NederScriptInstruction> visitPlusOp(NederScriptParser.PlusOpContext ctx) {
        List<NederScriptInstruction> newInst = new ArrayList<>();

        newInst.add(new NederScriptInstruction.Pop(2));
        newInst.add(new NederScriptInstruction.Pop(3));

        if (ctx.PLUS() != null) {
            newInst.add(new NederScriptInstruction.Compute(NederScriptOperator.Add, 2,3,4));
        } else {
            newInst.add(new NederScriptInstruction.Compute(NederScriptOperator.Sub, 3,2,4));
        }

        newInst.add(new NederScriptInstruction.Push(4));

        return newInst;
    }

    @Override
    public List<NederScriptInstruction> visitBoolOp(NederScriptParser.BoolOpContext ctx) {
        List<NederScriptInstruction> newInst = new ArrayList<>();
        newInst.add(new NederScriptInstruction.Pop(2));
        newInst.add(new NederScriptInstruction.Pop(3));
        if (ctx.AND() != null) {
            newInst.add(new NederScriptInstruction.Compute(NederScriptOperator.And, 2,3,4));
        } else {
            newInst.add(new NederScriptInstruction.Compute(NederScriptOperator.Or, 2,3,4));
        }
        newInst.add(new NederScriptInstruction.Push(4));
        return newInst;
    }

    @Override
    public List<NederScriptInstruction> visitCompOp(NederScriptParser.CompOpContext ctx) {
        List<NederScriptInstruction> newInst = new ArrayList<>();

        newInst.add(new NederScriptInstruction.Pop(3));
        newInst.add(new NederScriptInstruction.Pop(2));

        if (ctx.LE() != null) {
            newInst.add(new NederScriptInstruction.Compute(NederScriptOperator.LtE, 2,3,4));
        } else if (ctx.LT() != null){
            newInst.add(new NederScriptInstruction.Compute(NederScriptOperator.Lt, 2,3,4));
        } else if (ctx.GE() != null){
            newInst.add(new NederScriptInstruction.Compute(NederScriptOperator.GtE, 2,3,4));
        } else if (ctx.GT() != null){
            newInst.add(new NederScriptInstruction.Compute(NederScriptOperator.Gt, 2,3,4));
        } else if (ctx.EQ() != null){
            newInst.add(new NederScriptInstruction.Compute(NederScriptOperator.Equal, 2,3,4));
        } else if (ctx.NE() != null){
            newInst.add(new NederScriptInstruction.Compute(NederScriptOperator.NEq, 2,3,4));
        }

        newInst.add(new NederScriptInstruction.Push(4));

        return newInst;
    }

    @Override
    public List<NederScriptInstruction> visitStringPrimitive(NederScriptParser.StringPrimitiveContext ctx) {
        //TODO put string on heap
        return null;
    }

    @Override
    public List<NederScriptInstruction> visitCharacterPrimitive(NederScriptParser.CharacterPrimitiveContext ctx) {
        List<NederScriptInstruction> instList = new ArrayList<>();
        Integer c = (int) ctx.CHR().getText().charAt(0);
        instList.add(new NederScriptInstruction.Load(new NederScriptAddrImmDI.NederScriptImmValue(c),2));
        instList.add(new NederScriptInstruction.Push(2));
        return instList;
    }

    @Override
    public List<NederScriptInstruction> visitArrayPrimitive(NederScriptParser.ArrayPrimitiveContext ctx) {
        //TODO put array on heap
        return null;
    }

    @Override
    public List<NederScriptInstruction> visitIntegerPrimitive(NederScriptParser.IntegerPrimitiveContext ctx) {
        List<NederScriptInstruction> instList = new ArrayList<>();
        instList.add(new NederScriptInstruction.Load(new NederScriptAddrImmDI.NederScriptImmValue(Integer.parseInt(ctx.NUM().getText())),2));
        instList.add(new NederScriptInstruction.Push(2));
        return instList;
    }

    @Override
    public List<NederScriptInstruction> visitBooleanPrimitive(NederScriptParser.BooleanPrimitiveContext ctx) {
        List<NederScriptInstruction> instList = new ArrayList<>();
        int res;
        if (ctx.TRUE() != null) {
            res = 1;
        } else {
            res = 0;
        }
        instList.add(new NederScriptInstruction.Load(new NederScriptAddrImmDI.NederScriptImmValue(res),2));
        instList.add(new NederScriptInstruction.Push(2));
        return instList;
    }

    @Override
    public List<NederScriptInstruction> visitFunCall(NederScriptParser.FunCallContext ctx) {
        System.out.println("Visit func call");
        List<NederScriptInstruction> instList = new ArrayList<>();
        String functionName = ctx.VAR().getText();
        switch (functionName) {
            case "afdrukken":
                NederScriptType type = result.getType(ctx.expr(0));
                if (type.equals(NederScriptType.TOUW)) {
                    //TODO
                } else if (type.equals(NederScriptType.GETAL)) {

                    List<NederScriptInstruction> exprIns = visit(ctx.expr(0));
                    if (exprIns != null) {
                        instList.addAll(exprIns);
                    }

                    instList.add(new NederScriptInstruction.Pop(2));
                    instList.add(new NederScriptInstruction.WriteInstr(2, new NederScriptAddrImmDI.NederScriptDirAddr(65536)));

                } else if (type.equals(NederScriptType.BOOLEAANS)) {
                    List<NederScriptInstruction> exprIns = visit(ctx.expr(0));
                    if (exprIns != null) {
                        instList.addAll(exprIns);
                    }
                    instList.add(new NederScriptInstruction.Pop(2));
                    instList.add(new NederScriptInstruction.Branch(2, new NederScriptTarget.Rel(2 + 7*2)));
                    instList.addAll(createPrintString("onwaar\n"));
                    instList.add(new NederScriptInstruction.Jump(new NederScriptTarget.Rel(1 + 5*2)));
                    instList.addAll(createPrintString("waar\n"));
                } else {
                    errs.add("Incorrect type for function 'afdrukken'. Expected TOUW, GETAL or BOOLEAANS but found " + type);
                }
                break;
            case "lengte":
                //TODO
                break;
            default:
                //TODO
                break;

        }

        return instList;
    }

    public List<NederScriptInstruction> createPrintString(String s) {
        List<NederScriptInstruction> instList = new ArrayList<>();

        for (int i = 0; i < s.length(); i++) {
            Integer c = (int) s.charAt(i);
            instList.add(new NederScriptInstruction.Load(new NederScriptAddrImmDI.NederScriptImmValue(c),2));
            instList.add(new NederScriptInstruction.WriteInstr(2, new NederScriptAddrImmDI.NederScriptDirAddr(65537)));
        }
        return instList;
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
