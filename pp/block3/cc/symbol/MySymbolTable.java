package pp.block3.cc.symbol;

import java.util.HashSet;
import java.util.Set;
import java.util.Stack;

public class MySymbolTable implements SymbolTable {
    private final Stack<Set<String>> stack = new Stack<>();

    public MySymbolTable() {
        openScope();
    }

    @Override
    public void openScope() {
        stack.push(new HashSet<>());
    }

    @Override
    public void closeScope() {
        if (stack.size() <= 1) {
            throw new RuntimeException();
        }
        stack.pop();
    }

    @Override
    public boolean add(String id) {
        return stack.peek().add(id);
    }

    @Override
    public boolean contains(String id) {
        for (Set<String> s : stack) {
            if (s.contains(id)) return true;
        }
        return false;
    }
}
