package pp.project.elaboration;

abstract public class NederScriptType {
    /** Singleton instance of Booleaans type. */
    public static final NederScriptType BOOLEAANS = new Booleaans();

    /** Singleton instance of Karakter type. */
    public static final NederScriptType KARAKTER = new Karakter();

    /** Singleton instance of Getal type. */
    public static final NederScriptType GETAL = new Getal();

    /** Singleton instance of Leegte type. */
    public static final NederScriptType LEEGTE = new Leegte();

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
            return 1;
        }

        @Override
        public String toString() {return "Booleaans"; }

    }

    /** Getal type */
    static public class Getal extends NederScriptType {
        private Getal() { super(NederScriptTypeKind.GETAL);}

        @Override
        public int size() {
            return 1;
        }

        @Override
        public String toString() {return "Getal"; }

    }

    /** Getal type */
    static public class Karakter extends NederScriptType {
        private Karakter() { super(NederScriptTypeKind.KARAKTER);}

        @Override
        public int size() {
            return 1;
        }

        @Override
        public String toString() {return "Karakter"; }

    }

    /** Reeks type */
    static public class Reeks extends NederScriptType {
        private final NederScriptType elemType;
        private final int length;

        public Reeks (NederScriptType elemType, Integer length) {
            super(NederScriptTypeKind.REEKS);
            this.elemType = elemType;
            this.length = length;
        }

        public NederScriptType getElemType() { return this.elemType; }

        @Override
        public int size() {
            return 1 + this.length;
        }

        @Override
        public String toString() {
            return "Reeks<" + getElemType() + ">";
        }

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

        public Touw(Integer length) {
            super(NederScriptTypeKind.TOUW);
            this.length = length;
        }


        @Override
        public int size() {
            return 1 + this.length;
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


    /** Leegte type */
    // This is the return type of void functions
    static public class Leegte extends NederScriptType {
        private Leegte() { super(NederScriptTypeKind.LEEGTE); }

        @Override
        public int size() { return 0; }

        @Override
        public String toString() {return "Leegte"; }
    }

}
