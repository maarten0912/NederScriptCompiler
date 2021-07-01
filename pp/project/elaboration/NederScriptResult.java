package pp.project.elaboration;

import org.antlr.v4.runtime.ParserRuleContext;
import org.antlr.v4.runtime.tree.ParseTree;
import org.antlr.v4.runtime.tree.ParseTreeProperty;

public class NederScriptResult {

    /** Mapping from expressions to types. */
    private final ParseTreeProperty<NederScriptType> types = new ParseTreeProperty<>();
    /** Mapping from variables to coordinates. */
    private final ParseTreeProperty<Integer> offsets = new ParseTreeProperty<>();
    /** Mapping from variables to public coordinates. */
    private final ParseTreeProperty<Boolean> isPublic = new ParseTreeProperty<>();
    private Integer numThreads = 1;

    /** Adds an association from a parse tree node containing a
     * variable reference to the offset
     * of that variable. */
    public void setOffset(ParseTree node, int offset) {
        this.offsets.put(node, offset);
    }

    /** Returns the declaration offset of the variable
     * accessed in a given parse tree node. */
    public int getOffset(ParseTree node) {
        return this.offsets.get(node);
    }

    /** Adds an association from a parse tree expression, type,
     * or assignment target node to the corresponding (inferred) type. */
    public void setType(ParseTree node, NederScriptType type) {
        this.types.put(node, type);
    }

    public void setPublic(ParseTree node, Boolean isPublic) {
        this.isPublic.put(node, isPublic);
    }

    public Boolean isPublic (ParseTree node) {
        return this.isPublic.get(node);
    }

    /** Returns the type associated with a given parse tree node. */
    public NederScriptType getType(ParseTree node) {
        return this.types.get(node);
    }

    public void addThread() {
        numThreads++;
    }

    public Integer getNumThreads() {
        return numThreads;
    }
}
