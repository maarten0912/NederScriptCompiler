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
        return this.prog;
    }


}
