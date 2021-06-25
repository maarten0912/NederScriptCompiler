package pp.project.generation;

import pp.project.sprockell.SprockellBuilder;

import java.util.ArrayList;
import java.util.List;

public class NederScriptProgram {
    List<NederScriptInstruction> instList;

    public NederScriptProgram() {
        instList = new ArrayList<>();
        instList.add(new NederScriptInstruction.ReadInstr(new NederScriptAddrImmDI.NederScriptDirAddr(5)));
        instList.add(new NederScriptInstruction.Compute(NederScriptOperator.Add, 1, 2, 3));
        System.out.println(instList.get(0));
        System.out.println(instList.get(1));
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