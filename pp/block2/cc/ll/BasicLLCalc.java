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
        first.put(Symbol.EMPTY, new HashSet<>(Collections.singletonList(Symbol.EMPTY)));
        first.put(Symbol.EOF, new HashSet<>(Collections.singletonList(Symbol.EOF)));

        boolean changing;
        do {
            changing = false;
            for (Rule r : g.getRules()) {
                List<Symbol> betas = r.getRHS();
                int k = betas.size();

                Set<Term> rhs = new HashSet<>(first.get(betas.get(0)));
                rhs.remove(Symbol.EMPTY);

                int i = 1;
                while (first.get(betas.get(i - 1)).contains(Symbol.EMPTY) && i <= k - 1) {
                    Set<Term> u = new HashSet<>(first.get(betas.get(i)));
                    u.remove(Symbol.EMPTY);
                    rhs.addAll(u);
                    i += 1;
                }

                if (i == k && first.get(betas.get(k - 1)).contains(Symbol.EMPTY)) {
                    rhs.add(Symbol.EMPTY);
                }

                if (first.get(r.getLHS()).addAll(rhs)) {
                    changing = true;
                }
            }
        } while (changing);
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
        boolean changing;
        do {
            changing = false;
            for (Rule r : g.getRules()) {
                Set<Term> trailer = new HashSet<>(follow.get(r.getLHS()));
                for (int i = r.getRHS().size(); i >= 1; i--) {
                    List<Symbol> bs = r.getRHS();
                    Symbol Bi = bs.get(i - 1);
                    if (Bi instanceof NonTerm) {

                        if (follow.get(Bi).addAll(trailer)) { // update follow using trailer
                            changing = true;
                        }

                        if (getFirst().get(Bi).contains(Symbol.EMPTY)) {
                            Set<Term> addition = new HashSet<>(getFirst().get(Bi));
                            addition.remove(Symbol.EMPTY);
                            trailer.addAll(addition);
                        } else {
                            trailer = new HashSet<>(getFirst().get(Bi));
                        }
                    } else {
                        trailer = new HashSet<>(getFirst().get(Bi));
                    }
                }
            }
        } while (changing);

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
            List<Symbol> betas = r.getRHS();
            Set<Term> a = new HashSet<>(first.get(betas.get(0)));

            firstp.get(r).addAll(a);
            if (a.contains(Symbol.EMPTY)) {
                firstp.get(r).addAll(follow.get(r.getLHS()));
            }
            firstp.get(r).remove(Symbol.EMPTY);
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