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
        while (!first.equals(temp)) {
            temp = new HashMap<>();
            for (Map.Entry<Symbol, Set<Term>> entry : first.entrySet()) {
                temp.put(entry.getKey(), new HashSet<>(entry.getValue()));
            }
            for (Rule r : g.getRules()) {
                List<Symbol> betas = r.getRHS();
                int k = r.getRHS().size();

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
        while (!follow.equals(temp)) {
            temp = new HashMap<>();
            for (Map.Entry<NonTerm, Set<Term>> entry : follow.entrySet()) {
                temp.put(entry.getKey(), new HashSet<>(entry.getValue()));
            }
            for (Rule r : g.getRules()) {
                Set<Term> trailer = follow.get(r.getLHS());
                for (int i = r.getRHS().size(); i >= 1; i--) {
                    List<Symbol> bs = r.getRHS();
                    Symbol Bi = bs.get(i - 1);
                    if (Bi instanceof NonTerm) {
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
        Map<Rule, Set<Term>> firstp = new HashMap<>();
        Map<Symbol, Set<Term>> first = getFirst();
        Map<NonTerm, Set<Term>> follow = getFollow();
        for (Rule r : g.getRules()) {
            firstp.put(r, new HashSet<>());
        }
        for (Rule r : g.getRules()) {
            List<Symbol> beta = r.getRHS();
            firstp.get(r).addAll(first.get(beta.get(0)));
            firstp.get(r).remove(Symbol.EMPTY);
            int k = beta.size();
            int i = 1;
            while (first.get(beta.get(i - 1)).contains(Symbol.EMPTY) && i <= k - 1) {
                Set<Term> u = first.get(beta.get(i));
                u.remove(Symbol.EMPTY);
                firstp.get(r).addAll(u);
                i += 1;
            }
            if (firstp.get(r).contains(Symbol.EMPTY)) {
                firstp.get(r).addAll(follow.get(r.getLHS()));
            }
        }
        return firstp;
    };

    /** Indicates if the grammar of this calculator instance is LL(1). */
    public boolean isLL1() {
        Map<Rule, Set<Term>> firstp = getFirstp();

        for (NonTerm s : g.getNonterminals()) {
            Set<Term> overlap = new HashSet<>();
            for (Rule r : g.getRules(s)) {
                Set<Term> newTerms = firstp.get(r);
                Set<Term> clone = new HashSet<>(overlap);
                clone.retainAll(newTerms);
                if (clone.size() != 0) {
                    //Intersection is nonempty -> there is overlap
                    return false;
                }
                overlap.addAll(newTerms);
            }

        }

        return true;
    };
}