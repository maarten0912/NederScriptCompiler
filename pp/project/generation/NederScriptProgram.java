package pp.project.generation;

import java.util.ArrayList;
import java.util.List;

public class NederScriptProgram {
    private List<NederScriptInstruction> instList;

    public NederScriptProgram() {
        instList = new ArrayList<>();
        instList.add(new NederScriptInstruction.Debug("Beginning program! Enter a number:"));
        instList.add(new NederScriptInstruction.ReadInstr(new NederScriptAddrImmDI.NederScriptDirAddr(65536)));
        instList.add(new NederScriptInstruction.Receive(6));

        instList.add(new NederScriptInstruction.Load(new NederScriptAddrImmDI.NederScriptImmValue(0),2));
        instList.add(new NederScriptInstruction.Load(new NederScriptAddrImmDI.NederScriptImmValue(1),3));

        instList.add(new NederScriptInstruction.Compute(NederScriptOperator.Gt,2,6,4));
        instList.add(new NederScriptInstruction.Branch(4, new NederScriptTarget.Abs(13)));
        instList.add(new NederScriptInstruction.WriteInstr(2, new NederScriptAddrImmDI.NederScriptDirAddr(65536)));
        instList.add(new NederScriptInstruction.Compute(NederScriptOperator.Add,2,3,2));
        instList.add(new NederScriptInstruction.Compute(NederScriptOperator.Gt,3,6,4));
        instList.add(new NederScriptInstruction.Branch(4, new NederScriptTarget.Abs(13)));
        instList.add(new NederScriptInstruction.WriteInstr(3, new NederScriptAddrImmDI.NederScriptDirAddr(65536)));
        instList.add(new NederScriptInstruction.Compute(NederScriptOperator.Add,2,3,3));
        instList.add(new NederScriptInstruction.Jump(new NederScriptTarget.Rel(-8)));

        instList.add(new NederScriptInstruction.EndProg());
        for (NederScriptInstruction i : instList) {
            System.out.println(i);
        }
    }

    public SprockellBuilder toHaskell() {
        SprockellBuilder builder = new SprockellBuilder(this);
        return builder;
    }

    public void addInstruction(NederScriptInstruction inst) {
        this.instList.add(inst);
    }

    public String prettyPrint() {
        return null;
    }

    public List<NederScriptInstruction> getInstructions() {
        return instList;
    }

}