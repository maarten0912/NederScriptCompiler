package pp.block2.cc.ll;

import pp.block2.cc.NonTerm;
import pp.block2.cc.Symbol;
import pp.block2.cc.Term;

import java.util.*;

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
        Map<NonTerm, Set<Term>> follow = new HashMap<>();
        for (NonTerm e : g.getNonterminals()) {
            follow.put(e, new HashSet<>());
        }
        follow.get(g.getStart()).add(Symbol.EOF);
        //Initialised
        Map<NonTerm, Set<Term>> temp = new HashMap<>();
        while (temp != follow) {
            temp = follow;
            for (Rule r : g.getRules()) {
                Set<Term> trailer = follow.get(r.getLHS());
                System.out.println(trailer);
                for (int i = r.getRHS().size() - 1; i >= 1; i--) {
                    List<Symbol> bs = r.getRHS();
                    Symbol Bi = bs.get(i);
                    if (Bi instanceof NonTerm) {
                        System.out.println("yes");
                        follow.get(Bi).addAll(trailer); // update follow using trailer
                        if (getFirst().get(Bi).contains(Symbol.EMPTY)) {
                            Set<Term> addition = getFirst().get(Bi);
                            addition.remove(Symbol.EMPTY);
                            trailer.addAll(addition);
                        } else {
                            trailer = getFirst().get(Bi);
                        }
                    } else {
                        trailer = getFirst().get(Bi);
                    }
                }
            }
        }

        return follow;
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