package pp.block2.cc.ll;

import pp.block2.cc.NonTerm;
import pp.block2.cc.Symbol;
import pp.block2.cc.Term;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

/** Interface for a calculator that determines for a given grammar
 * (to be passed in as an argument to the constructor) what the
 * FIRST, FOLLOWS and FIRST+-sets are.
 */
public class BasicLLCalc implements LLCalc {

    Grammar g;

    public BasicLLCalc(Grammar g) {
        this.g = g;
    }

    /** Returns the FIRST-map for the grammar of this calculator instance. */
    public Map<Symbol, Set<Term>> getFirst() {
        Map<Symbol, Set<Term>> res = new HashMap<>();
        for (Symbol e : g.getNonterminals()) {
            res.put(e, new HashSet<>());
        }
//        for (Map.Entry<Symbol, Set<Term>> e : res.entrySet()) {
//            System.out.println(e.getKey() + " " + e.getValue());
//        }


        return null;
    };

    /** Returns the FOLLOW-map for the grammar of this calculator instance. */
    public Map<NonTerm, Set<Term>> getFollow() {

        return null;
    };

    /** Returns the FIRST+-map for the grammar of this calculator instance. */
    public Map<Rule, Set<Term>> getFirstp() {
        return null;
    };

    /** Indicates if the grammar of this calculator instance is LL(1). */
    public boolean isLL1() {
        return false;
    };
}