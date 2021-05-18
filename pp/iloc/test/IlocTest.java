package pp.iloc.test;

import org.junit.Test;

import pp.iloc.Assembler;
import pp.iloc.Simulator;
import pp.iloc.eval.Machine;
import pp.iloc.model.Program;
import pp.iloc.parse.FormatException;

import java.io.File;
import java.io.IOException;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.fail;

public class IlocTest {

    @Test
    public void Test1() {
        Program p = parse("max");
        String print = p.prettyPrint();
        System.out.println("Program max.iloc:");
        System.out.print(print);
        Program other = null;
        try {
            other = Assembler.instance().assemble(print);
        } catch (FormatException e) {
            fail(e.getMessage());
        }
        assertEquals(p, other);
    }

    @Test
    public void Test2() {
        Program p = parse("max");
        Simulator sim = new Simulator(p);
        Machine vm = sim.getVM();
        vm.init("a",2,5,7);
        vm.setNum("alength",3);
        sim.run();
        assertEquals(sim.getVM().getReg("r_max"),7);
    }

    Program parse(String filename) {
        File file = new File(filename + ".iloc");
        if (!file.exists()) {
            file = new File("pp/block4/cc/iloc/" + filename + ".iloc");
        }
        try {
            Program result = Assembler.instance().assemble(file);
            return result;
        } catch (FormatException | IOException e) {
            fail(e.getMessage());
            return null;
        }
    }
}
