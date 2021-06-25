package pp.project.generation;

abstract public class NederScriptAddrImmDI {

    static public class NederScriptImmValue extends NederScriptAddrImmDI {
        private Integer immValue;

        /**
         * ImmValue n: is just the constants value n
         * @param n
         */
        public NederScriptImmValue(Integer n) {
            this.immValue = n;
        }

        @Override
        public String toString() {
            return "ImmValue " + immValue;
        }
    }

    static public class NederScriptDirAddr extends NederScriptAddrImmDI{
        private Integer memAddr;

        /**
         * DirAddr a: is an address in memory (local or shared)
         * @param a
         */
        public NederScriptDirAddr(Integer a) {
            this.memAddr = a;
        }

        @Override
        public String toString() {
            return String.format("DirAddr (MemAddr %s)",memAddr);
        }
    }

    static public class NederScriptIndAddr extends NederScriptAddrImmDI {
        private Integer regAddr;

        /**
         * IndAddr p: p is a register, the content of this register is an address in memory
         * @param p
         */
        public NederScriptIndAddr(Integer p) {
            this.regAddr = p;
        }

        @Override
        public String toString() {
            return String.format("IndAddr (RegAddr %s)",regAddr);

        }
    }


}
