package pp.project.generation;

import pp.project.elaboration.NederScriptTypeKind;

abstract public class NederScriptInstruction {

    /** Kind of a type. */
    private final NederScriptInstructionKind kind;
    protected NederScriptInstruction(NederScriptInstructionKind kind) { this.kind = kind;}

    static public class Compute extends NederScriptInstruction {

        NederScriptOperator op;
        Integer r0;
        Integer r1;
        Integer r2;

        /**
         * Compute op r0 r1 r2: go to "alu",
         *     do "op" on regs r0, r1, and put result in reg r2
         * @param op
         * @param r0
         * @param r1
         * @param r2
         */
        public Compute(NederScriptOperator op, Integer r0, Integer r1, Integer r2) {
            super(NederScriptInstructionKind.Compute);
            this.op = op;
            this.r0 = r0;
            this.r1 = r1;
            this.r2 = r2;
        }

        @Override
        public String toString() {
            return String.format("Compute (%s) (%s) (%s) (%s)",op, r0, r1, r2);
        }
    }

    static public class Jump extends NederScriptInstruction {
        NederScriptTarget target;

        /**
         * Jump t: jump to target t (absolute, relative, indirect)
         * @param t
         */
        public Jump(NederScriptTarget t) {
            super(NederScriptInstructionKind.Jump);
            this.target = t;
        }

        @Override
        public String toString() {
            return String.format("Jump (%s)",target);
        }
    }

    static public class Branch extends NederScriptInstruction {
        String regAddr;
        NederScriptTarget target;

        /**
         * Branch r t: conditional jump, depending on register r
         *     if r contains 0: don't jump; otherwise: jump
         * @param r
         * @param t
         */
        public Branch(Integer r, NederScriptTarget t) {
            super(NederScriptInstructionKind.Jump);
            this.regAddr = r.toString();
            this.target = t;
        }

        public Branch(String r, NederScriptTarget t) {
            super(NederScriptInstructionKind.Jump);
            this.regAddr = r;
            this.target = t;
        }

        @Override
        public String toString() {
            return String.format("Branch (%s) (%s)",regAddr, target);
        }
    }

    static public class Load extends NederScriptInstruction {
        NederScriptAddrImmDI addrImmDI;
        Integer regAddr;

        /**
         * Load (ImmValue n) r: put value n in register r
         * Load (DirAddr a) r : put value on memory address a in r
         * Load (IndAddr p) r : ibid, but memory address is in register p
         * @param n
         * @param r
         */
        public Load(NederScriptAddrImmDI n, Integer r) {
            super(NederScriptInstructionKind.Load);
            this.addrImmDI = n;
            this.regAddr = r;
        }

        @Override
        public String toString() {
            return String.format("Load (%s) (%s)",addrImmDI, regAddr);
        }
    }

    static public class Store extends NederScriptInstruction {
        Integer regAddr;
        NederScriptAddrImmDI addrImmDI;

        /**
         * Store r (DirAddr a): from register r to memory address a
         * Store r (IndAddr p): ibid, memory address contained in register p
         * Store r (ImmValue n): undefined
         * @param r
         * @param n
         */
        public Store(Integer r,NederScriptAddrImmDI n) {
            super(NederScriptInstructionKind.Store);
            this.regAddr = r;
            this.addrImmDI = n;
        }

        @Override
        public String toString() {
            return String.format("Store (%s) (%s)",regAddr, addrImmDI);
        }
    }

    static public class Push extends NederScriptInstruction {
        Integer regAddr;

        /**
         * Push r: put the value from register r on the stack
         * @param r
         */
        public Push(Integer r) {
            super(NederScriptInstructionKind.Push);
            this.regAddr = r;
        }

        @Override
        public String toString() {
            return String.format("Push (%s)",regAddr);
        }
    }

    static public class Pop extends NederScriptInstruction {
        Integer regAddr;

        /**
         * Pop r : put the top of the stack in register r
         *     and adapts the stack pointer
         * @param r
         */
        public Pop(Integer r) {
            super(NederScriptInstructionKind.Pop);
            this.regAddr = r;
        }

        @Override
        public String toString() {
            return String.format("Pop (%s)",regAddr);
        }
    }

    static public class ReadInstr extends NederScriptInstruction {

        NederScriptAddrImmDI addrImmDI;

        /**
         * ReadInstr a: Send read request for shMem address a
         * @param a
         */
        public ReadInstr(NederScriptAddrImmDI a) {
            super(NederScriptInstructionKind.ReadInstr);
            this.addrImmDI = a;
        }

        @Override
        public String toString() {
            return String.format("ReadInstr (%s)",addrImmDI);
        }
    }

    static public class Receive extends NederScriptInstruction {
        Integer regAddr;

        /**
         * Receive r  : Wait for reply and save it in register r
         * @param r
         */
        public Receive(Integer r) {
            super(NederScriptInstructionKind.Receive);
            this.regAddr = r;
        }

        @Override
        public String toString() {
            return String.format("Receive (%s)",regAddr);
        }
    }

    static public class WriteInstr extends NederScriptInstruction {
        Integer regAddr;
        NederScriptAddrImmDI addrImmDI;

        /**
         * WriteInstr r a: Write content of reg r to shMem address a
         * @param r
         * @param a
         */
        public WriteInstr(Integer r, NederScriptAddrImmDI a) {
            super(NederScriptInstructionKind.WriteInstr);
            this.regAddr = r;
            this.addrImmDI = a;
        }

        @Override
        public String toString() {
            return String.format("WriteInstr (%s) (%s)",regAddr, addrImmDI);
        }
    }

    static public class TestAndSet extends NederScriptInstruction {
        NederScriptAddrImmDI addrImmDI;

        /**
         * Request a test on address for 0 and sets it to 1 if it is.
         * Reply will contain 1 on success, and 0 on failure.
         * This is an atomic operation; it might therefore be
         * used to implement locks or synchronisation.
         *
         * For ReadInstr, WriteInstr, TestAndSet:
         *     address only as DirAddr, IndAddr; not as ImmValue
         * @param a
         */
        public TestAndSet(NederScriptAddrImmDI a) {
            super(NederScriptInstructionKind.TestAndSet);
            this.addrImmDI = a;
        }

        @Override
        public String toString() {
            return String.format("TestAndSet (%s)", addrImmDI);
        }
    }

    static public class EndProg extends NederScriptInstruction {

        /**
         * End of program, deactivates Sprockell. If all sprockells are at
         * this instruction, the simulation will halt.
         */
        public EndProg() {
            super(NederScriptInstructionKind.EndProg);
        }

        @Override
        public String toString() {
            return "EndProg";
        }
    }

    static public class Nop extends NederScriptInstruction {

        /**
         * Operation "do nothing"
         */
        public Nop() {
            super(NederScriptInstructionKind.Nop);
        }

        @Override
        public String toString() {
            return "Nop";
        }
    }

    static public class Debug extends NederScriptInstruction {
        String s;

        /**
         * No real instruction, for debug purposes.
         */
        public Debug(String s) {
            super(NederScriptInstructionKind.Debug);
            this.s = s;
        }

        @Override
        public String toString() {
            return String.format("Debug (\"%s\")", s);
        }
    }
}
