package pp.project;

import java.util.HashSet;
import java.util.Set;
import java.util.Stack;

public class ScopeTable {
    private final Stack<NederScriptScope> stack = new Stack<>();

    public void openScope() {
        System.out.println("new scope");
        stack.push(new NederScriptScope());
    }

    public void closeScope() {
        System.out.println("close scope");
        if (stack.size() <= 0) {
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

    public void printTypeContents() {
        for (NederScriptScope s : stack) {
            System.out.println("====scope=====");
            s.printTypes();
        }
    }
}
