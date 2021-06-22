package pp.project;

import java.util.LinkedHashMap;
import java.util.Map;

public class NederScriptScope {
    /** Current size of this scope (in bytes).
     * Used to calculate offsets of newly declared variables. */
    private int size;
    /** Map from declared variables to their types. */
    private final Map<String, NederScriptType> types;
    /** Map from declared variables to their offset within the allocation
     * record of this scope. */
    private final Map<String, Integer> offsets;

    /** Constructs a fresh, initially empty scope. */
    public NederScriptScope() {
        this.types = new LinkedHashMap<>();
        this.offsets = new LinkedHashMap<>();
    }

    /** Tests if a given identifier is declared in this scope. */
    public boolean contains(String id) {
        return this.types.containsKey(id);
    }

    /** Declares an identifier with a given type, if the identifier
     * is not yet in this scope.
     * @return <code>true</code> if the identifier was added;
     * <code>false</code> if it was already declared.
     */
    public boolean put(String id, NederScriptType type) {
        boolean result = !this.types.containsKey(id);
        if (result) {
            this.types.put(id, type);
            this.offsets.put(id, this.size);
            this.size += type.size();
        }
        return result;
    }

    /** Returns the type of a given (presumably declared) identifier.
     */
    public NederScriptType type(String id) {
        return this.types.get(id);
    }

    /** Returns the offset of a given (presumably declared) identifier.
     * with respect to the beginning of this scope's activation record.
     * Offsets are assigned in order of declaration.
     */
    public Integer offset(String id) {
        return this.offsets.get(id);
    }
}
