package pp.block2.cc.test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.junit.Test;

import pp.block2.cc.NonTerm;
import pp.block2.cc.Symbol;
import pp.block2.cc.Term;
import pp.block2.cc.ll.*;

public class LLCalcTest {
	Grammar sentenceG = Grammars.makeSentence();
	Grammar lrqG = Grammars.makeLRQ();
	Grammar ifG = Grammars.makeIfstatement();
	// Define the non-terminals
	NonTerm subj = sentenceG.getNonterminal("Subject");
	NonTerm obj = sentenceG.getNonterminal("Object");
	NonTerm sent = sentenceG.getNonterminal("Sentence");
	NonTerm mod = sentenceG.getNonterminal("Modifier");
	NonTerm stat = ifG.getNonterminal ("Stat");
	NonTerm elsePart = ifG.getNonterminal ("ElsePart");
	NonTerm L = lrqG.getNonterminal("L");
	NonTerm R = lrqG.getNonterminal("R");
	NonTerm Rprime = lrqG.getNonterminal("R'");
	NonTerm Q = lrqG.getNonterminal("Q");
	NonTerm Qprime = lrqG.getNonterminal("Q'");
	// Define the terminals
	Term adj = sentenceG.getTerminal(Sentence.ADJECTIVE);
	Term noun = sentenceG.getTerminal(Sentence.NOUN);
	Term verb = sentenceG.getTerminal(Sentence.VERB);
	Term end = sentenceG.getTerminal(Sentence.ENDMARK);
	Term ifT = ifG. getTerminal(If.IF);
	Term elseT = ifG.getTerminal(If.ELSE);
	Term assign = ifG.getTerminal(If.ASSIGN);
	Term expr = ifG.getTerminal(If.COND);
	Term a = lrqG.getTerminal(LRQ.A);
	Term b = lrqG.getTerminal(LRQ.B);
	Term c = lrqG.getTerminal(LRQ.C);
	// (other terminals you need in the tests)
	Term eof = Symbol.EOF;
	Term empty = Symbol.EMPTY;
	// Now add the last rule, causing the grammar to fail
	Grammar sentenceXG = Grammars.makeSentence();
	{    sentenceXG.addRule(mod, mod, mod);
	}
	LLCalc sentenceXLL = createCalc(sentenceXG);
	LLCalc lrqLL = createCalc(lrqG);
	LLCalc ifLL = createCalc(ifG);

	/** Tests the LL-calculator for the Sentence grammar. */
	@Test
	public void testSentenceOrigLL1() {
		// Without the last (recursive) rule, the grammar is LL-1
		assertTrue(createCalc(sentenceG).isLL1());
	}

	@Test
	public void testSentenceXFirst() {
		Map<Symbol, Set<Term>> first = sentenceXLL.getFirst();
		assertEquals(set(adj, noun), first.get(sent));
		assertEquals(set(adj, noun), first.get(subj));
		assertEquals(set(adj, noun), first.get(obj));
		assertEquals(set(adj), first.get(mod));
	}

	@Test
	public void testIfFirst () {
		Map <Symbol, Set<Term>> first = ifLL.getFirst();
		assertEquals(set(assign, ifT), first.get(stat));
		assertEquals(set(elseT, empty), first.get(elsePart));
	}

	@Test
	public void testLRQFirst () {
		Map <Symbol, Set<Term>> first = lrqLL.getFirst();
		assertEquals(set(a, b, c), first.get(L));
		assertEquals(set(a, c), first.get(R));
		assertEquals(set(b, empty), first.get(Rprime));
		assertEquals(set(b), first.get(Q));
		assertEquals(set(b, c), first.get(Qprime));
	}
	
	@Test
	public void testSentenceXFollow() {
		// FOLLOW sets
		Map<NonTerm, Set<Term>> follow = sentenceXLL.getFollow();
		assertEquals(set(Symbol.EOF), follow.get(sent));
		assertEquals(set(verb), follow.get(subj));
		assertEquals(set(end), follow.get(obj));
		assertEquals(set(noun, adj), follow.get(mod));
	}

	@Test
	public void testIfFollow () {
		Map <NonTerm, Set<Term>> follow = ifLL.getFollow();
		assertEquals(set(eof, elseT), follow.get(stat));
		assertEquals(set(eof, elseT), follow.get(elsePart));
	}

	@Test
	public void testLRQFollow () {
		Map <NonTerm, Set<Term>> follow = lrqLL.getFollow();
		assertEquals(set(eof), follow.get(L));
		assertEquals(set(a), follow.get(R));
		assertEquals(set(a), follow.get(Rprime));
		assertEquals(set(b), follow.get(Q));
		assertEquals(set(b), follow.get(Qprime));
	}
	
	@Test
	public void testSentenceXFirstPlus() {
		// Test per rule
		Map<Rule, Set<Term>> firstp = sentenceXLL.getFirstp();
		List<Rule> subjRules = sentenceXG.getRules(subj);
		assertEquals(set(noun), firstp.get(subjRules.get(0)));
		assertEquals(set(adj), firstp.get(subjRules.get(1)));
	}


	@Test
	public void testIfFirstPlus() {
		// Test per rule
		Map<Rule, Set<Term>> firstp = ifLL.getFirstp();
		List<Rule> elseRules = ifG.getRules(elsePart);
		List<Rule> statRules = ifG.getRules(stat);
		assertEquals(set(elseT), firstp.get(elseRules.get(0)));
		assertEquals(set(elseT, eof), firstp.get(elseRules.get(1)));
		assertEquals(set(assign), firstp.get(statRules.get(0)));
		assertEquals(set(ifT), firstp.get(statRules.get(1)));
	}

	@Test
	public void testSentenceXLL1() {
		assertFalse(sentenceXLL.isLL1());
	}

	@Test
	public void testLRQLL1() {
		assertTrue(lrqLL.isLL1());
	}

	@Test
	public void testIfLL1() {
		assertFalse(ifLL.isLL1());
	}
	
	/** Creates an LL1-calculator for a given grammar. */
	private LLCalc createCalc(Grammar g) {
		return new BasicLLCalc(g); // TODO your implementation of LLCalc (Ex. 2-CC.3)
	}

	@SuppressWarnings("unchecked")
	private <T> Set<T> set(T... elements) {
		return new HashSet<>(Arrays.asList(elements));
	}
}
