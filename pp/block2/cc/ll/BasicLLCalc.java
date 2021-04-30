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
        Map<Symbol, Set<Term>> first = new HashMap<>();
        for (Symbol e : g.getNonterminals()) {
            first.put(e, new HashSet<>());
        }
        for (Term e : g.getTerminals()) {
            first.put(e, new HashSet<>(Collections.singletonList(e)));
        }


        Map<Symbol, Set<Term>> temp = new HashMap<>();
        while (first != temp) {
            temp = first;
            for (Rule r : g.getRules()) {
                List<Symbol> betas = r.getRHS();
                int k = r.getRHS().size();

                System.out.println(r);

                Set<Term> rhs = first.get(betas.get(0));
                rhs.remove(Symbol.EMPTY);

                int i = 1;
                while (first.get(betas.get(i - 1)).contains(Symbol.EMPTY) && i <= k - 1) {
                    Set<Term> u = first.get(betas.get(i));
                    u.remove(Symbol.EMPTY);
                    rhs.addAll(u);
                    i += 1;
                }

                if (i == k && first.get(betas.get(k - 1)).contains(Symbol.EMPTY)) {
                    rhs.add(Symbol.EMPTY);
                }

                first.get(r.getLHS()).addAll(rhs);
            }
        }
        System.out.println("====> FIRST <====");
        for (Map.Entry<Symbol, Set<Term>> e : first.entrySet()) {
            System.out.println(e.getKey() + " " + e.getValue());
        }
        System.out.println("====> FIRST <====");
        return first;
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