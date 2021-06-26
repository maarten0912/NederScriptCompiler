package pp.project.generation;

import org.antlr.v4.runtime.tree.ParseTree;
import org.antlr.v4.runtime.tree.ParseTreeProperty;
import pp.iloc.model.Label;
import pp.iloc.model.Reg;
import pp.project.elaboration.NederScriptBaseVisitor;
import pp.project.elaboration.NederScriptResult;

public class NederScriptGenerator extends NederScriptBaseVisitor {

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
        this.makeFibonacci();
        return this.prog;
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
