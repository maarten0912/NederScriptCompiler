package pp.project.generation;

import java.util.ArrayList;
import java.util.List;

/**
 * reg0          = 0    :: Int                          -- names for registers. reg0 is ALWAYS 0
 * regSprID      = 1    :: Int                          -- regSprID: contains the sprockellID
 * regA          = 2    :: Int                          -- usage of registers A-F to be chosen by the user
 * regB          = 3    :: Int
 * regC          = 4    :: Int
 * regD          = 5    :: Int
 * regE          = 6    :: Int
 * regF          = 7    :: Int
 * regSP         = regbankSize                          -- register for stack pointer
 * regPC         = regbankSize + 1                      -- register for program counter
 *
 * -- defines the number of registers excluding the stack pointer & program counter
 * regbankSize   =  8   :: Int
 * localMemSize  = 32   :: Int
 *
 * shMemSize     =  8   :: Int
 * channelDelay  =  4   :: Int
 *
 * numberIOaddr, charIOaddr :: MemAddr
 * numberIOaddr = 0x10000                               -- 65536
 * charIOaddr   = 0x10001                               -- 65537
 *
 * numberIO, charIO :: AddrImmDI
 * numberIO = DirAddr numberIOaddr
 * charIO   = DirAddr charIOaddr
 */

public class NederScriptProgram {
    private List<NederScriptInstruction> instList;
    private Integer threadNumber;
    private Boolean debugMode;
    private String debugFunction;
    private String debugShowFunction;

    public NederScriptProgram() {
        this.instList = new ArrayList<>();
        this.threadNumber = 1;
        this.debugMode = false;
        this.debugFunction = "debuggerSimplePrintAndWait";
        this.debugShowFunction = "myShow";
    }

    public SprockellBuilder toHaskell() {
        SprockellBuilder builder = new SprockellBuilder(this);
        return builder;
    }

    public void addInstruction(NederScriptInstruction inst) {
        this.instList.add(inst);
    }

    public void addInstructions(List<NederScriptInstruction> insts) {
        this.instList.addAll(insts);
    }

    public void setInstList(List<NederScriptInstruction> instList) {
        this.instList = instList;
    }

    public void prettyPrint() {
        for (NederScriptInstruction i : instList) {
            System.out.println(i);
        }
    }


    public void incrementThreadNumber() {
        this.threadNumber++;
    }

    public Integer getThreadNumber() {
        return this.threadNumber;
    }

    public void setThreadNumber(Integer num) {
        this.threadNumber = num;
    }

    public Boolean getDebugMode() {
        return this.debugMode;
    }

    public void setDebugMode(Boolean mode) {
        this.debugMode = mode;
    }

    public String getDebugFunction() {
        return debugFunction;
    }

    public void setDebugFunction(String debugFunction) {
        this.debugFunction = debugFunction;
    }

    public String getDebugShowFunction() {
        return debugShowFunction;
    }

    public void setDebugShowFunction(String debugShowFunction) {
        this.debugShowFunction = debugShowFunction;
    }

    public List<NederScriptInstruction> getInstructions() {
        return this.instList;
    }

}