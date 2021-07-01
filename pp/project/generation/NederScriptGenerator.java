package pp.project.generation;

import org.antlr.v4.runtime.ParserRuleContext;
import org.antlr.v4.runtime.tree.ParseTree;
import pp.project.elaboration.*;
import pp.project.exception.ParseException;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class NederScriptGenerator extends NederScriptBaseVisitor<List<NederScriptInstruction>> {

    private List<String> errs = new ArrayList<>();
    private NederScriptResult result;
    private NederScriptProgram prog;
    private ScopeTable st;
    private Map<Integer, List<NederScriptInstruction>> threads = new HashMap<>();

    public NederScriptProgram generate(ParseTree tree, NederScriptResult result) throws ParseException {
        this.result = result;
        this.prog = new NederScriptProgram();
        this.st = new ScopeTable();
        this.prog.setThreadNumber(result.getNumThreads());
        this.prog.addInstructions(tree.accept(this));
//        this.makeFibonacci();
        if (errs.size() > 0) {
            throw new ParseException(errs);
        }
        this.prog.setDebugMode(false);
        return this.prog;
    }




    @Override
    public List<NederScriptInstruction> visitProgram(NederScriptParser.ProgramContext ctx) {
        List<NederScriptInstruction> instList = new ArrayList<>();

        for (NederScriptParser.FunctionContext f : ctx.function()) {
            if (f.VAR(0).getText().equals("hoofd")) {
                List<NederScriptInstruction> newList = visit(f);

                if (newList != null) {

                    //Setup branch for subthreads
                    instList.add(new NederScriptInstruction.Branch(1, new NederScriptTarget.Rel(4)));

                    //TODO: not best solution, fix another way
                    //Put something in first mem address because the length in-built function is supposed to be there
                    instList.add(new NederScriptInstruction.Load(new NederScriptAddrImmDI.NederScriptImmValue(1),2));
                    instList.add(new NederScriptInstruction.Store(2, new NederScriptAddrImmDI.NederScriptDirAddr(0)));

                    //Jump for main thread
                    instList.add(new NederScriptInstruction.Jump(new NederScriptTarget.Rel(getThreadInst().size() + 6)));

                    //Loop threads until called
                    instList.add(new NederScriptInstruction.ReadInstr(new NederScriptAddrImmDI.NederScriptIndAddr(1)));
                    instList.add(new NederScriptInstruction.Receive(2));
                    instList.add(new NederScriptInstruction.Compute(NederScriptOperator.Equal, 2, 0, 3));
//                    instList.add(new NederScriptInstruction.WriteInstr(2, ));
                    instList.add(new NederScriptInstruction.Branch(3, new NederScriptTarget.Rel(-3)));

                    //Jump to called location in register A
                    instList.add(new NederScriptInstruction.Jump(new NederScriptTarget.Ind(2)));

                    //Thread jump blocks
                    instList.addAll(getThreadInst());

                    instList.addAll(newList);
                }
            }
        }
        instList.add(new NederScriptInstruction.EndProg());
        return instList;
    }

    @Override
    public List<NederScriptInstruction> visitFunction(NederScriptParser.FunctionContext ctx) {
        //TODO put function args on stack
        //TODO create new AR
        this.st.openScope();
        List<NederScriptInstruction> instList = new ArrayList<>();
        for (NederScriptParser.InstructionContext i : ctx.instruction()) {
            List<NederScriptInstruction> newList = visit(i);
            if (newList != null) {
                instList.addAll(newList);
            }
        }
        this.st.closeScope();
        return instList;
    }

    @Override
    public List<NederScriptInstruction> visitNormalInst(NederScriptParser.NormalInstContext ctx) {
        //TODO put function args on stack

        return visit(ctx.statement());
    }

    @Override
    public List<NederScriptInstruction> visitIfelseInst(NederScriptParser.IfelseInstContext ctx) {
        this.st.openScope();
        List<NederScriptInstruction> instList = visit(ctx.ifelse());
        this.st.closeScope();
        return instList;
    }

    @Override
    public List<NederScriptInstruction> visitWhileInst(NederScriptParser.WhileInstContext ctx) {
        this.st.openScope();
        List<NederScriptInstruction> instList = visit(ctx.whileS());
        this.st.closeScope();
        return instList;
    }

    @Override
    public List<NederScriptInstruction> visitForInst(NederScriptParser.ForInstContext ctx) {
        this.st.openScope();
        List<NederScriptInstruction> instList = visit(ctx.forS());
        this.st.closeScope();
        return instList;
    }

    @Override
    public List<NederScriptInstruction> visitNewScopeInst(NederScriptParser.NewScopeInstContext ctx) {
        this.st.openScope();
        List<NederScriptInstruction> instList = new ArrayList<>();
        for (NederScriptParser.StatementContext s : ctx.statement()) {
            List<NederScriptInstruction> newList = visit(s);
            if (newList != null) {
                instList.addAll(newList);
            }
        }
        this.st.closeScope();
        return instList;
    }

    @Override
    public List<NederScriptInstruction> visitAssignStat(NederScriptParser.AssignStatContext ctx) {
        return visit(ctx.assign());
    }

    @Override
    public List<NederScriptInstruction> visitDeclStat(NederScriptParser.DeclStatContext ctx) {
        return visit(ctx.decl());
    }

    @Override
    public List<NederScriptInstruction> visitReturnStat(NederScriptParser.ReturnStatContext ctx) {
        return visit(ctx.returnS());
    }


    @Override
    public List<NederScriptInstruction> visitFunctionCallStat(NederScriptParser.FunctionCallStatContext ctx) {
        return visit(ctx.funCall());
    }

    @Override
    public List<NederScriptInstruction> visitIfelse(NederScriptParser.IfelseContext ctx) {
        List<NederScriptInstruction> instList = new ArrayList<>();

        instList.addAll(visit(ctx.expr())); // condition

        List<NederScriptInstruction> ifI = new ArrayList<>();
        List<NederScriptInstruction> elseI = new ArrayList<>();

        Boolean toElse = false;
        for (int i = 0; i < ctx.getChildCount(); i++) {
            if (ctx.getChild(i) == ctx.ELSE()) {
                toElse = true;
            }
            if (ctx.instruction().contains(ctx.getChild(i))) {
                if (toElse) {
                    ifI.addAll(visit(ctx.getChild(i)));
                } else {
                    elseI.addAll(visit(ctx.getChild(i)));
                }
            }
        }

        instList.add(new NederScriptInstruction.Pop(2));
        instList.add(new NederScriptInstruction.Branch(2,new NederScriptTarget.Rel(ifI.size() + 2)));

        instList.addAll(ifI);

        instList.add(new NederScriptInstruction.Jump(new NederScriptTarget.Rel(elseI.size() + 1)));

        instList.addAll(elseI);

        return instList;
    }

    @Override
    public List<NederScriptInstruction> visitWhileS(NederScriptParser.WhileSContext ctx) {
        List<NederScriptInstruction> instList = new ArrayList<>();

        List<NederScriptInstruction> exprI = visit(ctx.expr());

        instList.addAll(exprI); // condition

        List<NederScriptInstruction> loopI = new ArrayList<>();

        for (NederScriptParser.InstructionContext ic : ctx.instruction()) {
            loopI.addAll(visit(ic));
        }

        instList.add(new NederScriptInstruction.Pop(2));
        instList.add(new NederScriptInstruction.Load(new NederScriptAddrImmDI.NederScriptImmValue(1), 3));
        instList.add(new NederScriptInstruction.Compute(NederScriptOperator.Sub, 3, 2, 2));
        instList.add(new NederScriptInstruction.Branch(2,new NederScriptTarget.Rel(loopI.size() + 2)));

        instList.addAll(loopI);

        System.out.println(exprI.size());

        instList.add(new NederScriptInstruction.Jump(new NederScriptTarget.Rel(-loopI.size() - exprI.size() - 4)));

        return instList;
    }

    @Override
    public List<NederScriptInstruction> visitForS(NederScriptParser.ForSContext ctx) {
        List<NederScriptInstruction> instList = new ArrayList<>();

        if (ctx.decl() != null) {
            instList.addAll(visit(ctx.decl()));
        } else {
            instList.addAll(visit(ctx.assign()));
        }

        List<NederScriptInstruction> exprI = visit(ctx.expr());
        instList.addAll(exprI); // condition

        List<NederScriptInstruction> loopI = new ArrayList<>();

        for (NederScriptParser.InstructionContext ic : ctx.instruction()) {
            loopI.addAll(visit(ic));
        }

        List<NederScriptInstruction> incrI = visit(ctx.statement());

        instList.add(new NederScriptInstruction.Pop(2));
        instList.add(new NederScriptInstruction.Load(new NederScriptAddrImmDI.NederScriptImmValue(1), 3));
        instList.add(new NederScriptInstruction.Compute(NederScriptOperator.Sub, 3, 2, 2));
        instList.add(new NederScriptInstruction.Branch(2,new NederScriptTarget.Rel(loopI.size() + incrI.size() + 2)));

        instList.addAll(loopI);

        instList.addAll(incrI);

        instList.add(new NederScriptInstruction.Jump(new NederScriptTarget.Rel(-loopI.size() - exprI.size() - incrI.size() - 4)));

        return instList;
    }

    @Override
    public List<NederScriptInstruction> visitAssign(NederScriptParser.AssignContext ctx) {
        List<NederScriptInstruction> instList = new ArrayList<>();

        instList.addAll(visit(ctx.expr()));
        NederScriptType type = this.result.getType(ctx.expr());
        if (type instanceof NederScriptType.Touw) {

        } else if (type instanceof NederScriptType.Reeks) {
            //TODO
        } else {
            instList.add(new NederScriptInstruction.Pop(2));
            instList.add(new NederScriptInstruction.Store(2,new NederScriptAddrImmDI.NederScriptDirAddr(this.result.getOffset(ctx))));
        }
        return instList;
    }

    @Override
    public List<NederScriptInstruction> visitNonTypedDecl(NederScriptParser.NonTypedDeclContext ctx) {
        //TODO
        return null;
    }

    @Override
    public List<NederScriptInstruction> visitTypedDecl(NederScriptParser.TypedDeclContext ctx) {
        System.out.println(String.format("Variable %s has offset %s", ctx.VAR().getText(), this.result.getOffset(ctx)));
        List<NederScriptInstruction> instList = new ArrayList<>();
        instList.addAll(visit(ctx.expr()));
        NederScriptType type = this.result.getType(ctx.expr());
        if (type instanceof NederScriptType.Touw) {
            // regA: stringlength
            instList.add(new NederScriptInstruction.Pop(2));
            // regB: offset
            instList.add(new NederScriptInstruction.Load(new NederScriptAddrImmDI.NederScriptImmValue(this.result.getOffset(ctx)), 3));
            // regC: string length + 1
            instList.add(new NederScriptInstruction.Compute(NederScriptOperator.Incr, 2, 0, 4));
            // string length + 1 -> mem(offset)
            instList.add(new NederScriptInstruction.Store(4, new NederScriptAddrImmDI.NederScriptIndAddr(3)));
            // offset++
            instList.add(new NederScriptInstruction.Compute(NederScriptOperator.Incr, 3, 0, 3));

            instList.add(new NederScriptInstruction.Jump(new NederScriptTarget.Rel(5)));
            // get next character
            instList.add(new NederScriptInstruction.Pop(4));
            instList.add(new NederScriptInstruction.Store(4, new NederScriptAddrImmDI.NederScriptIndAddr(3)));

            // offset := offset + 1
            instList.add(new NederScriptInstruction.Compute(NederScriptOperator.Incr, 3, 0, 3));
            // stringlength := stringlength - 1
            instList.add(new NederScriptInstruction.Compute(NederScriptOperator.Decr, 2, 0, 2));

            // if stringlength != 0, jump back
            instList.add(new NederScriptInstruction.Branch(2, new NederScriptTarget.Rel(-4)));
        } else if (type instanceof NederScriptType.Reeks) {
            //TODO nested arrays

            // regA: arraylength
            instList.add(new NederScriptInstruction.Pop(2));
            // regB: offset
            instList.add(new NederScriptInstruction.Load(new NederScriptAddrImmDI.NederScriptImmValue(this.result.getOffset(ctx)), 3));
            // regC: arraylength + 1
            instList.add(new NederScriptInstruction.Compute(NederScriptOperator.Incr, 2, 0, 4));
            // arraylength + 1 -> mem(offset)
            instList.add(new NederScriptInstruction.Store(4, new NederScriptAddrImmDI.NederScriptIndAddr(3)));
            // offset++
            instList.add(new NederScriptInstruction.Compute(NederScriptOperator.Incr, 3, 0, 3));

            instList.add(new NederScriptInstruction.Jump(new NederScriptTarget.Rel(5)));
            // get next element
            instList.add(new NederScriptInstruction.Pop(4));
            instList.add(new NederScriptInstruction.Store(4, new NederScriptAddrImmDI.NederScriptIndAddr(3)));

            // offset := offset + 1
            instList.add(new NederScriptInstruction.Compute(NederScriptOperator.Incr, 3, 0, 3));
            // stringlength := stringlength - 1
            instList.add(new NederScriptInstruction.Compute(NederScriptOperator.Decr, 2, 0, 2));

            // if arraylength != 0, jump back
            instList.add(new NederScriptInstruction.Branch(2, new NederScriptTarget.Rel(-4)));
        } else {
            instList.add(new NederScriptInstruction.Pop(2));
            instList.add(new NederScriptInstruction.Store(2,new NederScriptAddrImmDI.NederScriptDirAddr(this.result.getOffset(ctx))));
        }
        return instList;
    }

    @Override
    public List<NederScriptInstruction> visitThreadInst(NederScriptParser.ThreadInstContext ctx) {
        List<NederScriptInstruction> instList = new ArrayList<>();

        addThreadInst(visit(ctx.thread()));

        Integer threadID = getThreads().keySet().size();
        Integer offset = getThreadOffset(threadID) + 1;

        instList.add(new NederScriptInstruction.Load(new NederScriptAddrImmDI.NederScriptImmValue(offset),2));
        instList.add(new NederScriptInstruction.WriteInstr(2,new NederScriptAddrImmDI.NederScriptDirAddr(threadID)));

        return instList;
    }

    @Override
    public List<NederScriptInstruction> visitThread(NederScriptParser.ThreadContext ctx) {
        List<NederScriptInstruction> instList = new ArrayList<>();

        List<NederScriptInstruction> bodyI = new ArrayList<>();

        for (NederScriptParser.InstructionContext ic : ctx.instruction()) {
            bodyI.addAll(visit(ic));
        }

        instList.addAll(bodyI);

        instList.add(new NederScriptInstruction.EndProg());

        return instList;
    }

    @Override
    public List<NederScriptInstruction> visitReturnS(NederScriptParser.ReturnSContext ctx) {
        //TODO
        return null;
    }

    @Override
    public List<NederScriptInstruction> visitFunCall(NederScriptParser.FunCallContext ctx) {
        List<NederScriptInstruction> instList = new ArrayList<>();
        String functionName = ctx.VAR().getText();
        switch (functionName) {
            case "afdrukken":
                NederScriptType type = result.getType(ctx.expr(0));
                if (type instanceof NederScriptType.Touw) {
                    List<NederScriptInstruction> exprIns = visit(ctx.expr(0));
                    if (exprIns != null) {
                        instList.addAll(exprIns);
                    }
                    instList.addAll(createPrintString("Sprockell "));
                    instList.add(new NederScriptInstruction.Load(new NederScriptAddrImmDI.NederScriptImmValue(48),7));
                    instList.add(new NederScriptInstruction.Compute(NederScriptOperator.Add, 7, 1, 7));
                    instList.add(new NederScriptInstruction.WriteInstr(7, new NederScriptAddrImmDI.NederScriptDirAddr(65537)));
                    instList.addAll(createPrintString(" says "));

                    instList.add(new NederScriptInstruction.Pop(2));
                    instList.add(new NederScriptInstruction.Load(new NederScriptAddrImmDI.NederScriptImmValue(1),3));

                    instList.add(new NederScriptInstruction.Jump(new NederScriptTarget.Rel(4)));

                    instList.add(new NederScriptInstruction.Pop(4));

                    instList.add(new NederScriptInstruction.WriteInstr(4, new NederScriptAddrImmDI.NederScriptDirAddr(65537)));

                    instList.add(new NederScriptInstruction.Compute(NederScriptOperator.Sub, 2, 3, 2));

                    // if sLen != 0, jump
                    instList.add(new NederScriptInstruction.Branch(2, new NederScriptTarget.Rel(-3)));

                    instList.addAll(createPrintString("\n"));

                } else if (type instanceof NederScriptType.Reeks) {
                    List<NederScriptInstruction> exprIns = visit(ctx.expr(0));
                    if (exprIns != null) {
                        instList.addAll(exprIns);
                    }
                    instList.addAll(createPrintString("Sprockell "));
                    instList.add(new NederScriptInstruction.Load(new NederScriptAddrImmDI.NederScriptImmValue(48),7));
                    instList.add(new NederScriptInstruction.Compute(NederScriptOperator.Add, 7, 1, 7));
                    instList.add(new NederScriptInstruction.WriteInstr(7, new NederScriptAddrImmDI.NederScriptDirAddr(65537)));
                    instList.addAll(createPrintString(" says "));

                    instList.add(new NederScriptInstruction.Load(new NederScriptAddrImmDI.NederScriptImmValue(48),7));
                    //Print [
                    instList.add(new NederScriptInstruction.Load(new NederScriptAddrImmDI.NederScriptImmValue(91),6));
                    instList.add(new NederScriptInstruction.WriteInstr(6, new NederScriptAddrImmDI.NederScriptDirAddr(65537)));

                    instList.add(new NederScriptInstruction.Pop(2));
                    instList.add(new NederScriptInstruction.Load(new NederScriptAddrImmDI.NederScriptImmValue(1),3));

                    instList.add(new NederScriptInstruction.Jump(new NederScriptTarget.Rel(6)));

                    instList.add(new NederScriptInstruction.Pop(4));

                    //Convert number to ascii character
                    //TODO: this is way too hard for multiple digit, numbers
//                    instList.add(new NederScriptInstruction.Compute(NederScriptOperator.Add,4, 7,4));
                    instList.add(new NederScriptInstruction.WriteInstr(4, new NederScriptAddrImmDI.NederScriptDirAddr(65536)));
                    //Print comma
                    instList.add(new NederScriptInstruction.Load(new NederScriptAddrImmDI.NederScriptImmValue(44),6));
                    instList.add(new NederScriptInstruction.WriteInstr(6, new NederScriptAddrImmDI.NederScriptDirAddr(65537)));


                    instList.add(new NederScriptInstruction.Compute(NederScriptOperator.Sub, 2, 3, 2));

                    //If we are at the last element in the array, jump
                    instList.add(new NederScriptInstruction.Compute(NederScriptOperator.Equal, 2, 3, 5));
                    instList.add(new NederScriptInstruction.Branch(5, new NederScriptTarget.Rel(3)));
                    // if arrLen != 0, jump
                    instList.add(new NederScriptInstruction.Branch(2, new NederScriptTarget.Rel(-7)));
                    instList.add(new NederScriptInstruction.Jump(new NederScriptTarget.Rel(3)));

                    //Print only the element without comma
                    instList.add(new NederScriptInstruction.Pop(4));
                    //Convert number to ascii character
//                    instList.add(new NederScriptInstruction.Compute(NederScriptOperator.Add,4, 7,4));
                    instList.add(new NederScriptInstruction.WriteInstr(4, new NederScriptAddrImmDI.NederScriptDirAddr(65536)));

                    //Print ]
                    instList.add(new NederScriptInstruction.Load(new NederScriptAddrImmDI.NederScriptImmValue(93),6));
                    instList.add(new NederScriptInstruction.WriteInstr(6, new NederScriptAddrImmDI.NederScriptDirAddr(65537)));

                    instList.addAll(createPrintString("\n"));

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
                    instList.addAll(createPrintString("Sprockell "));
                    instList.add(new NederScriptInstruction.Load(new NederScriptAddrImmDI.NederScriptImmValue(48),7));
                    instList.add(new NederScriptInstruction.Compute(NederScriptOperator.Add, 7, 1, 7));
                    instList.add(new NederScriptInstruction.WriteInstr(7, new NederScriptAddrImmDI.NederScriptDirAddr(65537)));
                    instList.addAll(createPrintString(" says "));

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
//                int size = this.result.getType(ctx.expr(0)).size() - 1;
//                instList.add(new NederScriptInstruction.Load(new NederScriptAddrImmDI.NederScriptImmValue(size),2));
//                instList.add(new NederScriptInstruction.Push(2));
                break;
            default:
                //TODO
                break;

        }

        return instList;
    }

    @Override
    public List<NederScriptInstruction> visitType(NederScriptParser.TypeContext ctx) {
        //TODO
        return null;
    }

    @Override
    public List<NederScriptInstruction> visitPlusExpr(NederScriptParser.PlusExprContext ctx) {
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
        List<NederScriptInstruction> instList = new ArrayList<>();
        NederScriptType type = this.result.getType(ctx);
        System.out.println(String.format("Found variable %s of type %s with offset %s", ctx.VAR(0).getText(),type,this.result.getOffset(ctx)));
        if (type instanceof NederScriptType.Touw) {
            // offset -> regA
            instList.add(new NederScriptInstruction.Load(new NederScriptAddrImmDI.NederScriptImmValue(this.result.getOffset(ctx)),2));
            // strLen + 1 -> regB
            instList.add(new NederScriptInstruction.Load(new NederScriptAddrImmDI.NederScriptIndAddr(2),3));
            // offset + strLen -> regB
            instList.add(new NederScriptInstruction.Compute(NederScriptOperator.Add,2,3,3));
            instList.add(new NederScriptInstruction.Compute(NederScriptOperator.Decr,3,0,3));

            instList.add(new NederScriptInstruction.Compute(NederScriptOperator.GtE, 2, 3, 4));
            instList.add(new NederScriptInstruction.Branch(4, new NederScriptTarget.Rel(5)));

            instList.add(new NederScriptInstruction.Load(new NederScriptAddrImmDI.NederScriptIndAddr(3),5));
            instList.add(new NederScriptInstruction.Push(5));
            instList.add(new NederScriptInstruction.Compute(NederScriptOperator.Decr,3,0,3));

            instList.add(new NederScriptInstruction.Jump(new NederScriptTarget.Rel(-5)));

//            instList.add(new NederScriptInstruction.Compute(NederScriptOperator.Incr,3,0,3));
            instList.add(new NederScriptInstruction.Load(new NederScriptAddrImmDI.NederScriptIndAddr(3),5));
            instList.add(new NederScriptInstruction.Compute(NederScriptOperator.Decr,5,0,5));
            instList.add(new NederScriptInstruction.Push(5));

        } else if (type instanceof NederScriptType.Reeks) {
            if (ctx.VAR().size() > 1 || ctx.NUM().size() > 0) {
                //TODO nested arrays
                for (int i = 0; i < ctx.getChildCount(); i++) {
                    if (ctx.VAR().contains(ctx.getChild(i))) {

                        break;
                    } else if (ctx.NUM().contains(ctx.getChild(i))) {

                        break;
                    }
                }
            } else {
                // offset -> regA
                instList.add(new NederScriptInstruction.Load(new NederScriptAddrImmDI.NederScriptImmValue(this.result.getOffset(ctx)),2));
                // strLen + 1 -> regB
                instList.add(new NederScriptInstruction.Load(new NederScriptAddrImmDI.NederScriptIndAddr(2),3));
                // offset + strLen -> regB
                instList.add(new NederScriptInstruction.Compute(NederScriptOperator.Add,2,3,3));
                instList.add(new NederScriptInstruction.Compute(NederScriptOperator.Decr,3,0,3));

                instList.add(new NederScriptInstruction.Compute(NederScriptOperator.GtE, 2, 3, 4));
                instList.add(new NederScriptInstruction.Branch(4, new NederScriptTarget.Rel(5)));

                instList.add(new NederScriptInstruction.Load(new NederScriptAddrImmDI.NederScriptIndAddr(3),5));
                instList.add(new NederScriptInstruction.Push(5));
                instList.add(new NederScriptInstruction.Compute(NederScriptOperator.Decr,3,0,3));

                instList.add(new NederScriptInstruction.Jump(new NederScriptTarget.Rel(-5)));

//            instList.add(new NederScriptInstruction.Compute(NederScriptOperator.Incr,3,0,3));
                instList.add(new NederScriptInstruction.Load(new NederScriptAddrImmDI.NederScriptIndAddr(3),5));
                instList.add(new NederScriptInstruction.Compute(NederScriptOperator.Decr,5,0,5));
                instList.add(new NederScriptInstruction.Push(5));
            }
        } else {
            instList.add(new NederScriptInstruction.Load(new NederScriptAddrImmDI.NederScriptDirAddr(this.result.getOffset(ctx)),2));
            instList.add(new NederScriptInstruction.Push(2));
        }
        return instList;
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
        String s = ctx.getText().split("\"",3)[1];
        Integer sLen = s.length();
        List<NederScriptInstruction> instList = new ArrayList<>();

        for (int i = sLen - 1; i >= 0; i--) {
            Integer c = (int) s.charAt(i);
            instList.add(new NederScriptInstruction.Load(new NederScriptAddrImmDI.NederScriptImmValue(c), 2));
            instList.add(new NederScriptInstruction.Push(2));
        }
        instList.add(new NederScriptInstruction.Load(new NederScriptAddrImmDI.NederScriptImmValue(sLen), 2));
        instList.add(new NederScriptInstruction.Push(2));
        return instList;
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
        //TODO nested array
        int arrLen = ctx.VAR().size() + ctx.primitive().size();

        List<NederScriptInstruction> instList = new ArrayList<>();

        for (int i = ctx.getChildCount() - 1; i >= 0 ; i--) {
            if (ctx.VAR().contains(ctx.getChild(i))) {
                //TODO array indexing
                assert this.result.getType(ctx.getChild(i)).equals(NederScriptType.GETAL)
                        || this.result.getType(ctx.getChild(i)).equals(NederScriptType.BOOLEAANS)
                        || this.result.getType(ctx.getChild(i)).equals(NederScriptType.KARAKTER);
                instList.add(new NederScriptInstruction.Load(new NederScriptAddrImmDI.NederScriptDirAddr(this.result.getOffset(ctx.getChild(i))),2));
                instList.add(new NederScriptInstruction.Push(2));
            } else if (ctx.primitive().contains(ctx.getChild(i))) {
                assert this.result.getType(ctx.getChild(i)).equals(NederScriptType.GETAL)
                        || this.result.getType(ctx.getChild(i)).equals(NederScriptType.BOOLEAANS)
                        || this.result.getType(ctx.getChild(i)).equals(NederScriptType.KARAKTER);

                instList.addAll(visit(ctx.getChild(i)));
            }
        }
        instList.add(new NederScriptInstruction.Load(new NederScriptAddrImmDI.NederScriptImmValue(arrLen), 2));
        instList.add(new NederScriptInstruction.Push(2));
        return instList;
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

    public List<NederScriptInstruction> getThreadInst() {
        List<NederScriptInstruction> instList = new ArrayList<>();

        for (Integer id : this.threads.keySet()) {
            instList.addAll(this.threads.get(id));
        }

        return instList;
    }

    public void addThreadInst(List<NederScriptInstruction> threadInst) {
        this.threads.put(this.threads.size() + 1,threadInst);
    }

    public Map<Integer, List<NederScriptInstruction>> getThreads() {
        return this.threads;
    }

    public Integer getThreadOffset(Integer threadID) {
        Integer offset = 0;

        for (Integer id : this.threads.keySet()) {
            if (id == threadID) {
                return offset;
            } else {
                offset += this.threads.get(id).size();
            }
        }

        return offset;
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
