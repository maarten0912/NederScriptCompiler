package pp.block4.cc.cfg;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import org.antlr.v4.runtime.CharStream;
import org.antlr.v4.runtime.CharStreams;
import org.antlr.v4.runtime.CommonTokenStream;
import org.antlr.v4.runtime.Lexer;
import org.antlr.v4.runtime.ParserRuleContext;
import org.antlr.v4.runtime.TokenStream;
import org.antlr.v4.runtime.tree.ParseTree;
import org.antlr.v4.runtime.tree.ParseTreeProperty;
import org.antlr.v4.runtime.tree.ParseTreeWalker;
import pp.block4.cc.ErrorListener;
import pp.block4.cc.cfg.FragmentParser.BreakStatContext;
import pp.block4.cc.cfg.FragmentParser.ContStatContext;

/** Template bottom-up CFG builder. */
public class BottomUpCFGBuilder extends FragmentBaseListener {
	/** The CFG being built. */
	private Graph graph;
	private ParseTreeWalker walker = new ParseTreeWalker();
	private ParseTreeProperty<Node> entryNode = new ParseTreeProperty<>();
	private ParseTreeProperty<Node> exitNode = new ParseTreeProperty<>();

	/** Builds the CFG for a program contained in a given file. */
	public Graph build(File file) {
		Graph result = null;
		ErrorListener listener = new ErrorListener();
		try {
			CharStream chars = CharStreams.fromPath(file.toPath());
			Lexer lexer = new FragmentLexer(chars);
			lexer.removeErrorListeners();
			lexer.addErrorListener(listener);
			TokenStream tokens = new CommonTokenStream(lexer);
			FragmentParser parser = new FragmentParser(tokens);
			parser.removeErrorListeners();
			parser.addErrorListener(listener);
			ParseTree tree = parser.program();
			if (listener.hasErrors()) {
				System.out.printf("Parse errors in %s:%n", file.getPath());
				for (String error : listener.getErrors()) {
					System.err.println(error);
				}
			} else {
				result = build(tree);
			}
		} catch (IOException e) {
			e.printStackTrace();
		}
		return result;
	}

	/** Builds the CFG for a program given as an ANTLR parse tree. */
	public Graph build(ParseTree tree) {
		this.graph = new Graph();
		walker.walk(this, tree);
		System.out.println(graph.size());
		for (Node n : graph.getNodes()) {
			System.out.println(n);
		}
		System.out.println("Done");
		return this.graph;
	}

	@Override
	public void exitProgram(FragmentParser.ProgramContext ctx) {
//		entryNode.put(ctx,entryNode.get(ctx.getChild(0)));
//		exitNode.put(ctx,exitNode.get(ctx.getChild(ctx.getChildCount() - 1)));
//		for (int i = 0; i < ctx.getChildCount() - 1; i++) {
//			Node a = exitNode.get(ctx.getChild(i));
//			Node b = entryNode.get(ctx.getChild(i + 1));
//			if (a != null) {
//				a.addEdge(b);
//			} else {
//				System.err.println("a was null");
//			}
//		}
	}

	@Override
	public void exitDecl(FragmentParser.DeclContext ctx) {
		Node newNode = addNode(ctx, ctx.getText());
		entryNode.put(ctx, newNode);
		exitNode.put(ctx, newNode);
	}

	@Override
	public void exitAssignStat(FragmentParser.AssignStatContext ctx) {
		Node newNode = addNode(ctx, ctx.getText());
		entryNode.put(ctx, newNode);
		exitNode.put(ctx, newNode);
	}

	@Override
	public void exitIfStat(FragmentParser.IfStatContext ctx) {
		Node ifNode = addNode(ctx, ctx.getText());
		entryNode.put(ctx, ifNode);
		Node fakeEndNode = addNode(ctx, "End if");
		exitNode.put(ctx, fakeEndNode);

		Node thenNode = addNode(ctx.stat(0), ctx.stat(0).getText());
		ifNode.addEdge(thenNode);
		thenNode.addEdge(fakeEndNode);
		if (ctx.stat().size() > 1) {
			Node elseNode = addNode(ctx.stat(1), ctx.stat(1).getText());
			ifNode.addEdge(elseNode);
			elseNode.addEdge(fakeEndNode);
		} else {
			ifNode.addEdge(fakeEndNode);
		}
	}

	@Override
	public void exitWhileStat(FragmentParser.WhileStatContext ctx) {
		Node whileNode = addNode(ctx, ctx.getText());
		entryNode.put(ctx, whileNode);
		Node fakeEndNode = addNode(ctx, "End while");
		exitNode.put(ctx, fakeEndNode);

		Node statNode = addNode(ctx.stat(), ctx.stat().getText());
		whileNode.addEdge(statNode);
		whileNode.addEdge(fakeEndNode);
		statNode.addEdge(whileNode);
	}

	@Override
	public void exitPrintStat(FragmentParser.PrintStatContext ctx) {
		Node newNode = addNode(ctx, ctx.getText());
		entryNode.put(ctx, newNode);
		exitNode.put(ctx, newNode);
	}

	@Override
	public void exitBlockStat(FragmentParser.BlockStatContext ctx) {
		List<Node> nodeList = new ArrayList<>();
		for (int i = 0; i < ctx.stat().size() - 1; i++) {
			nodeList.add(addNode(ctx.stat(i), ctx.stat(i).getText()));
		}
		for (int i = 0; i < nodeList.size() - 1; i++) {
			Node a = nodeList.get(i);
			Node b = nodeList.get(i + 1);
			a.addEdge(b);
		}
		if (!nodeList.isEmpty()) {
			entryNode.put(ctx, nodeList.get(0));
			exitNode.put(ctx, nodeList.get(nodeList.size()- 1));
		}
	}

	@Override
	public void enterBreakStat(BreakStatContext ctx) {
		throw new IllegalArgumentException("Break not supported");
	}

	@Override
	public void enterContStat(ContStatContext ctx) {
		throw new IllegalArgumentException("Continue not supported");
	}

	/** Adds a node to he CGF, based on a given parse tree node.
	 * Gives the CFG node a meaningful ID, consisting of line number and 
	 * a further indicator.
	 */
	private Node addNode(ParserRuleContext node, String text) {
		return this.graph.addNode(node.getStart().getLine() + ": " + text);
	}

	/** Main method to build and print the CFG of a simple Java program. */
	public static void main(String[] args) {
		if (args.length == 0) {
			System.err.println("Usage: [filename]+");
			return;
		}
		BottomUpCFGBuilder builder = new BottomUpCFGBuilder();
		for (String filename : args) {
			File file = new File(filename);
			System.out.println(filename);
			System.out.println(builder.build(file));
		}
	}
}
