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
import org.antlr.v4.runtime.tree.ParseTreeProperty;
import org.antlr.v4.runtime.tree.ParseTreeWalker;
import org.antlr.v4.runtime.tree.TerminalNode;
import pp.block4.cc.ErrorListener;
import pp.block4.cc.cfg.FragmentParser.BreakStatContext;
import pp.block4.cc.cfg.FragmentParser.ContStatContext;
import pp.block4.cc.cfg.FragmentParser.ProgramContext;

/** Template top-down CFG builder. */
public class TopDownCFGBuilder extends FragmentBaseListener {
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
			ProgramContext tree = parser.program();
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
	public Graph build(ProgramContext tree) {
		this.graph = new Graph();
		walker.walk(this, tree);
		return this.graph;
	}

	@Override
	public void enterProgram(ProgramContext ctx) {
		List<Node> entryNodeList = new ArrayList<>();
		List<Node> exitNodeList = new ArrayList<>();
		for (int i = 0; i < ctx.getChildCount(); i++) {
			if (!(ctx.getChild(i) instanceof TerminalNode)) {
				//ctx.getChild(i+1) is probably not EOF

				Node a = addNode((ParserRuleContext) ctx.getChild(i), ctx.getChild(i).getText());
				entryNodeList.add(a);
				entryNode.put(ctx.getChild(i), a);
				Node b = addNode((ParserRuleContext) ctx.getChild(i), ctx.getChild(i).getText() + " END");
				exitNodeList.add(b);
				exitNode.put(ctx.getChild(i), b);
			}
		}

		for (int i = 0; i < entryNodeList.size() - 1; i++) {
			exitNodeList.get(i).addEdge(entryNodeList.get(i+1));
		}
	}

	@Override
	public void enterDecl(FragmentParser.DeclContext ctx) {
		Node a = entryNode.get(ctx);
		Node b = exitNode.get(ctx);
		a.addEdge(b);

	}

	@Override
	public void enterAssignStat(FragmentParser.AssignStatContext ctx) {
		Node a = entryNode.get(ctx);
		Node b = exitNode.get(ctx);
		a.addEdge(b);
	}

	@Override
	public void enterIfStat(FragmentParser.IfStatContext ctx) {
		Node ifNode = entryNode.get(ctx);
		Node endIfNode = exitNode.get(ctx);

		Node thenNode = addNode(ctx.stat(0), ctx.stat(0).getText());
		Node endThenNode = addNode(ctx.stat(0), ctx.stat(0).getText() + " END");

		entryNode.put(ctx.stat(0), thenNode);
		exitNode.put(ctx.stat(0), endThenNode);

		ifNode.addEdge(thenNode);
		endThenNode.addEdge(endIfNode);

		if (ctx.stat().size() > 1) {
			Node elseNode = addNode(ctx.stat(1), ctx.stat(0).getText());
			Node endElseNode = addNode(ctx.stat(1), ctx.stat(0).getText() + " END");

			entryNode.put(ctx.stat(1), elseNode);
			exitNode.put(ctx.stat(1), endElseNode);

			ifNode.addEdge(elseNode);
			endElseNode.addEdge(endIfNode);
		} else {
			ifNode.addEdge(endIfNode);
		}
	}

	@Override
	public void enterWhileStat(FragmentParser.WhileStatContext ctx) {
		Node whileNode = entryNode.get(ctx);
		Node endWhileNode = exitNode.get(ctx);

		Node statNode = addNode(ctx.stat(), ctx.stat().getText());
		Node endStatNode = addNode(ctx.stat(), ctx.stat().getText() + " END");

		entryNode.put(ctx.stat(), statNode);
		exitNode.put(ctx.stat(), endStatNode);

		whileNode.addEdge(statNode);
		endStatNode.addEdge(whileNode);
		whileNode.addEdge(endWhileNode);
	}

	@Override
	public void enterBlockStat(FragmentParser.BlockStatContext ctx) {
		List<Node> entryNodeList = new ArrayList<>();
		List<Node> exitNodeList = new ArrayList<>();
		for (int i = 0; i < ctx.getChildCount(); i++) {
			if (!(ctx.getChild(i) instanceof TerminalNode)) {
				//ctx.getChild(i+1) is probably not EOF

				Node a = addNode((ParserRuleContext) ctx.getChild(i), ctx.getChild(i).getText());
				entryNodeList.add(a);
				entryNode.put(ctx.getChild(i), a);
				Node b = addNode((ParserRuleContext) ctx.getChild(i), ctx.getChild(i).getText() + " END");
				exitNodeList.add(b);
				exitNode.put(ctx.getChild(i), b);
			}
		}

		for (int i = 0; i < entryNodeList.size() - 1; i++) {
			exitNodeList.get(i).addEdge(entryNodeList.get(i+1));
		}

		entryNode.get(ctx).addEdge(entryNodeList.get(0));
		exitNodeList.get(exitNodeList.size() - 1).addEdge(exitNode.get(ctx));
	}

	@Override
	public void enterPrintStat(FragmentParser.PrintStatContext ctx) {
		Node a = entryNode.get(ctx);
		Node b = exitNode.get(ctx);
		a.addEdge(b);
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
		TopDownCFGBuilder builder = new TopDownCFGBuilder();
		for (String filename : args) {
			File file = new File(filename);
			System.out.println(filename);
			Graph g = builder.build(file);
			System.out.println(g);
			try {
				g.writeDOT(filename + ".topdown.dot", true);
			} catch (IOException e) {
				e.printStackTrace();
			}
		}
	}
}
