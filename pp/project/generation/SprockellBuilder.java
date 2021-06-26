package pp.project.generation;

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
        for (int i = 0; i < prog.getInstructions().size(); i++) {
            addInstr(prog.getInstructions().get(i));
            if (i + 1 < prog.getInstructions().size()){
                spacer();
            }
        }
        endFile();
    }

    private void addInstr(NederScriptInstruction instr) {
        builder.append(instr.toString());
    }

    private void initFile() {
        builder.append("import Sprockell \n\n");
        builder.append("{- This file was automatically generated from a NederScript file.\n");
        builder.append("NederScript is a custom language created by Maarten Meijer and Pepijn Visser \n");
        builder.append("-} \n\n");
        builder.append("prog :: [Instruction] \n");
        builder.append("prog = [ \n    ");
    }

    private void spacer() {
        builder.append("\n    , ");
    }

    private void endFile() {
        builder.append("\n    ]\n\n");
        Integer threadNumber = this.prog.getThreadNumber();
        String progString = "[";
        for (int i = 0; i < threadNumber - 1; i++) {
            progString += "prog,";
        }
        progString += "prog]";
        if (this.prog.getDebugMode()) {
            builder.append(String.format("main = runWithDebugger (%s %s) %s",this.prog.getDebugFunction(), this.prog.getDebugShowFunction(), progString));
        } else {
            builder.append("main = run " + progString);
        }
    }


    public String getRes() {
        return builder.toString();
    }
}
