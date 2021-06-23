package pp.project;

import pp.block5.cc.simple.Type;
import pp.iloc.eval.Machine;

abstract public class NederScriptType {
    /** Singleton instance of Booleaans type. */
    public static final NederScriptType BOOLEAANS = new Booleaans();

    /** Singleton instance of Karakter type. */
    public static final NederScriptType KARAKTER = new Karakter();

    /** Singleton instance of Getal type. */
    public static final NederScriptType GETAL = new Getal();

    /** Singleton instance of Draad type. */
    public static final NederScriptType DRAAD = new Draad();

    /** Singleton instance of Touw type. */
    public static final NederScriptType TOUW = new Touw(0);

    /** Kind of a type. */
    private final NederScriptTypeKind kind;
    protected NederScriptType(NederScriptTypeKind kind) { this.kind = kind;}

    /** returns the size (in bytes) of a value of this type. */
    abstract public int size();


    /** Booleaans type */
    static public class Booleaans extends NederScriptType {
        private Booleaans() { super(NederScriptTypeKind.BOOLEAANS);}

        @Override
        public int size() {
            return 4;
        }

        @Override
        public String toString() {return "Booleaans"; }

    }

    /** Getal type */
    static public class Getal extends NederScriptType {
        private Getal() { super(NederScriptTypeKind.GETAL);}

        @Override
        public int size() {
            return 4;
        }

        @Override
        public String toString() {return "Getal"; }

    }

    /** Getal type */
    static public class Karakter extends NederScriptType {
        private Karakter() { super(NederScriptTypeKind.KARAKTER);}

        @Override
        public int size() {
            return 4;
        }

        @Override
        public String toString() {return "Karakter"; }

    }

    /** Reeks type */
    static public class Reeks extends NederScriptType {
        private final int length;
        private final NederScriptType elemType;

        public Reeks(int length, NederScriptType elemType) {
            super(NederScriptTypeKind.REEKS);
            assert length >= 0;
            this.length = length;
            this.elemType = elemType;
        }

        public Integer getLength() { return this.length; }

        public NederScriptType getElemType() { return this.elemType; }

        @Override
        public int size() {
            return 4 * (getLength() + 1);
        }

        @Override
        public String toString() {return "Reeks of type " + getElemType(); }

        @Override
        public boolean equals(Object obj) {
            if (this == obj) {
                return true;
            }
            if (!(obj instanceof NederScriptType.Reeks)) {
                return false;
            }
            NederScriptType.Reeks other = (NederScriptType.Reeks) obj;
            if (!this.elemType.equals(other.elemType)) {
                return false;
            }
            return true;
        }

    }

    /** Touw type */
    static public class Touw extends NederScriptType {

        private final int length;

        public Touw(int length) {
            super(NederScriptTypeKind.TOUW);
            assert length >= 0;
            this.length = length;
        }

        public Integer getLength() { return this.length; }

        @Override
        public int size() {
            return 4 * (getLength() + 1);
        }

        @Override
        public String toString() {return "Touw"; }

        @Override
        public boolean equals(Object obj) {
            if (this == obj) {
                return true;
            }
            if (!(obj instanceof NederScriptType.Touw)) {
                return false;
            }
            return true;
        }

    }

    /** Draad type */
    static public class Draad extends NederScriptType {

        private Draad() {
            super(NederScriptTypeKind.DRAAD);
        }

        @Override
        public int size() {
            return 4;
        }

        @Override
        public String toString() {return "Draad"; }

    }

}
