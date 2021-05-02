package pp.block2.cc.ll;

import java.util.List;

import org.antlr.v4.runtime.CharStream;
import org.antlr.v4.runtime.CharStreams;
import org.antlr.v4.runtime.Lexer;
import org.antlr.v4.runtime.Token;
import pp.block2.cc.*;

import static pp.block2.cc.ll.LRQ.*;

public class LRQParser implements Parser {
    public LRQParser() {
    this.fact = new SymbolFactory(LRQ.class);
    }

    private final SymbolFactory fact;

    @Override
    public AST parse(Lexer lexer) throws ParseException {
        this.tokens = lexer.getAllTokens();
        this.index = 0;
        return parseL();
    }

    private List<? extends Token> tokens;

    private AST parseL() throws ParseException {
        AST result = new AST(L);
        Token next = peek();
        // choose between rules based on the lookahead
        switch (next.getType()) {
            case A:
            case C:
                result.addChild(parseR());
                result.addChild(parseToken(A));
                break;
            case B:
                result.addChild(parseQ());
                result.addChild(parseToken(B));
                result.addChild(parseToken(A));
                break;
            default:
                throw unparsable(L);
        }
        return result;
    }

    private AST parseR() throws ParseException {
        AST result = new AST(R);
        Token next = peek();
        // choose between rules based on the lookahead
        switch (next.getType()) {
            case A:
                result.addChild(parseToken(A));
                result.addChild(parseToken(B));
                result.addChild(parseToken(A));
                result.addChild(parseRPrime());
                break;
            case C:
                result.addChild(parseToken(C));
                result.addChild(parseToken(A));
                result.addChild(parseToken(B));
                result.addChild(parseToken(A));
                result.addChild(parseRPrime());
                break;
            default:
                throw unparsable(R);
        }
        return result;
    }

    private AST parseRPrime() throws ParseException {
        AST result = new AST(RPRIME);
        Token next = peek();
        // choose between rules based on the lookahead
        if (next.getType() == B) {
            result.addChild(parseToken(B));
            result.addChild(parseToken(C));
            result.addChild(parseRPrime());
        }
        return result;
    }

    private AST parseQ() throws ParseException {
        AST result = new AST(Q);
        Token next = peek();
        // choose between rules based on the lookahead
        if (next.getType() == B) {
            result.addChild(parseToken(B));
            result.addChild(parseQPrime());
        } else {
            throw unparsable(Q);
        }
        return result;
    }

    private AST parseQPrime() throws ParseException {
        AST result = new AST(QPRIME);
        Token next = peek();
        // choose between rules based on the lookahead
        switch (next.getType()) {
            case B:
                result.addChild(parseToken(B));
                result.addChild(parseToken(C));
                break;
            case C:
                result.addChild(parseToken(C));
                break;
            default:
                throw unparsable(QPRIME);
        }
        return result;
    }

    private ParseException unparsable(NonTerm nt) {
        try {
            Token next = peek();
            return new ParseException(String.format(
                    "Line %d:%d - could not parse '%s' at token '%s'",
                    next.getLine(), next.getCharPositionInLine(), nt.getName(),
                    this.fact.get(next.getType())));
        } catch (ParseException e) {
            return e;
        }
    }

    /** Creates an AST based on the expected token type. */
    private AST parseToken(int tokenType) throws ParseException {
        Token next = next();
        if (next.getType() != tokenType) {
            throw new ParseException(String.format(
                    "Line %d:%d - expected token '%s' but found '%s'",
                    next.getLine(), next.getCharPositionInLine(),
                    this.fact.get(tokenType), this.fact.get(next.getType())));
        }
        return new AST(this.fact.getTerminal(tokenType), next);
    }

    /** Returns the next token, without moving the token index. */
    private Token peek() throws ParseException {
        if (this.index >= this.tokens.size()) {
            throw new ParseException("Reading beyond end of input");
        }
        return this.tokens.get(this.index);
    }

    /** Returns the next token and moves up the token index. */
    private Token next() throws ParseException {
        Token result = peek();
        this.index++;
        return result;
    }

    private int index;

    private static final NonTerm L = new NonTerm("L");
    private static final NonTerm R = new NonTerm("R");
    private static final NonTerm RPRIME = new NonTerm("R'");
    private static final NonTerm Q = new NonTerm("Q");
    private static final NonTerm QPRIME = new NonTerm("Q'");

    public static void main(String[] args) {
        if (args.length == 0) {
            System.err.println("Usage: [text]+");
        } else {
            for (String text : args) {
                CharStream stream = CharStreams.fromString(text);
                Lexer lexer = new LRQ(stream);
                try {
                    System.out.printf("Parse tree: %n%s%n",
                            new LRQParser().parse(lexer));
                } catch (ParseException e) {
                    e.printStackTrace();
                }
            }
        }
    }
}
