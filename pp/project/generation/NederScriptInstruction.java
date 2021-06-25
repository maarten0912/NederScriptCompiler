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
         *   do "op" on regs r0, r1, and put result in reg r2
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
        Integer regAddr;
        NederScriptTarget target;

        /**
         * Branch r t: conditional jump, depending on register r
         *     if r contains 0: don't jump; otherwise: jump
         * @param r
         * @param t
         */
        public Branch(Integer r, NederScriptTarget t) {
            super(NederScriptInstructionKind.Jump);
            this.regAddr = r;
            this.target = t;
        }

        @Override
        public String toString() {
            return String.format("Branch (%s) (%s)",regAddr, target);
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
}
