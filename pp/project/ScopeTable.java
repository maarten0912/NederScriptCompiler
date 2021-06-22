package pp.project;

import java.util.HashSet;
import java.util.Set;
import java.util.Stack;

public class ScopeTable implements SymbolTable {
    private final Stack<NederScriptScope> stack = new Stack<>();

    public ScopeTable() {
        openScope();
    }

    @Override
    public void openScope() {
        stack.push(new NederScriptScope());
    }

    @Override
    public void closeScope() {
        if (stack.size() <= 1) {
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
}
