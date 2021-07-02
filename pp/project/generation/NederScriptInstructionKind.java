package pp.project.generation;

/**
 * The class contains the kinds of instructions that an instruction can have
 */
public enum NederScriptInstructionKind {
    Compute,
    Jump,
    Branch,
    Load,
    Store,
    Push,
    Pop,
    ReadInstr,
    Receive,
    WriteInstr,
    TestAndSet,
    EndProg,
    Nop,
    Debug
}
