package pp.project.elaboration;

import java.util.*;

/**
 * This class contains a stack of scopes, where in each scope the offset and type of variables are stored
 */
public class ScopeTable {
    // Deque is a better version of Stack, because the iterator goes from top to bottom
    private final Deque<NederScriptScope> stack = new ArrayDeque<>();

    private int stackSize;

    /**
     * In the constructor of this class, the stack size is supplied. This is used when opening a scope to get the current offset of a new scope
     * @param stackSize
     */
    public ScopeTable(Integer stackSize) {
        this.stackSize = stackSize;
        openScope();
    }

    /**
     * This method opens a scope in a scope table
     */
    public void openScope() {

        if (stack.peek() != null) {
            stackSize = stack.peek().getSize();

        }
        NederScriptScope newScope = new NederScriptScope();
        newScope.setSize(stackSize);
        stack.push(newScope);
    }

    /**
     * This method closes a scope in a scope table
     */
    public void closeScope() {
        if (stack.size() <= 1) {
            throw new RuntimeException();
        }
        stack.pop();
    }

    public Integer getDepth() {
        return stack.size();
    }

    public boolean add(String id, NederScriptType type) {
        return stack.peek().put(id, type);
    }

    public boolean contains(String id) {
        for (NederScriptScope s : stack) {
            if (s.contains(id)) return true;
        }
        return false;
    }

    public NederScriptType getType(String id) {
        for (NederScriptScope s : stack) {
            if (s.contains(id)) {
                return s.type(id);
            }
        }
        return null;
    }

    public Integer getOffset(String id) {
        for (NederScriptScope s : stack) {
            if (s.contains(id)) {
                return s.offset(id);
            }
        }
        return null;
    }

    public void printScopeTableContents() {
        for (NederScriptScope s : stack) {
            System.out.println("====scope=====");
            s.printContents();
        }
    }
}
