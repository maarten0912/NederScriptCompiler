package pp.project.generation;

/**
 * This class is a Java wrapper of the Sprockell Target data type
 */
abstract public class NederScriptTarget {

    static public class Abs extends NederScriptTarget{
        private Integer codeAddr;

        /**
         * Abs n: instruction n
         * @param n
         */
        public Abs(Integer n) {
            this.codeAddr = n;
        }

        @Override
        public String toString() {
            return String.format("Abs (%s)",codeAddr);
        }
    }

    static public class Rel extends NederScriptTarget{
        private Integer codeAddr;

        /**
         * Rel n: increase current program counter with n
         * @param n
         */
        public Rel(Integer n) {
            this.codeAddr = n;
        }

        @Override
        public String toString() {
            return String.format("Rel (%s)",codeAddr);
        }
    }

    static public class Ind extends NederScriptTarget{
        private Integer regAddr;

        /**
         * Rel r: increase current program counter with n
         * @param r
         */
        public Ind(Integer r) {
            this.regAddr = r;
        }

        @Override
        public String toString() {
            return String.format("Ind (%s)",regAddr);
        }
    }
}
