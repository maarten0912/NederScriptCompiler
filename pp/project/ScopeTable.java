package pp.project;

import java.util.HashSet;
import java.util.Set;
import java.util.Stack;

public class ScopeTable implements SymbolTable {
    private final Stack<NederScriptScope> stack = new Stack<>();

    @Override
    public void openScope() {
        System.out.println("new scope");
        stack.push(new NederScriptScope());
    }

    @Override
    public void closeScope() {
        System.out.println("close scope");
        if (stack.size() <= 0) {
            throw new RuntimeException();
        }
        stack.pop();
    }

    @Override
    public boolean add(String id, NederScriptType type) {
        return stack.peek().put(id, type);
    }

    @Override
    public boolean contains(String id) {
        for (NederScriptScope s : stack) {
            if (s.contains(id)) return true;
        }
        return false;
    }

    @Override
    public NederScriptType getType(String id) {
        for (NederScriptScope s : stack) {
            if (s.contains(id)) {
                return s.type(id);
            }
        }
        return null;
    }

    @Override
    public Integer getOffset(String id) {
        for (NederScriptScope s : stack) {
            if (s.contains(id)) {
                return s.offset(id);
            }
        }
        return null;
    }
}
