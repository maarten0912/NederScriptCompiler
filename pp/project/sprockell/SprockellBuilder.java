package pp.project.sprockell;

import pp.project.generation.NederScriptInstruction;
import pp.project.generation.NederScriptProgram;

public class SprockellBuilder {

    NederScriptProgram prog;

    private StringBuilder builder;

    public SprockellBuilder(NederScriptProgram prog) {
        this.prog = prog;
        this.builder = new StringBuilder();
        createSprockell();
    }

    private void createSprockell() {
        initFile();
        for (NederScriptInstruction ins : prog.getInstructions()) {
            addInstr(ins);
            spacer();
        }
        addEndProg();
        endFile();
    }

    private void addInstr(NederScriptInstruction instr) {
        builder.append(instr.toString());
    }

    private void addEndProg() {
        builder.append("EndProg");
    }

    private void initFile() {
        builder.append("import Sprockell \n\n");
        builder.append("{- This file was automatically generated from a NederScript file.\n");
        builder.append("NederScript is a custom language created by Maarten Meijer and Pepijn Visser \n");
        builder.append("-} \n\n");
        builder.append("prog :: [Instruction] \n");
        builder.append("prog = [ \n\t");
    }

    private void spacer() {
        builder.append("\n\t, ");
    }

    private void endFile() {
        builder.append("\n\t]\n\n");
        builder.append("main = run [prog]");
    }

    public String getRes() {
        return builder.toString();
    }
}
