package pp.block2.cc.test;


import java.util.HashSet;
import java.util.Set;

public class Test {

    public static void main(String[] args) {
        Set<Integer> set = new HashSet<>(Set.of(1,2,3));
        Set<Integer> set2 = set;
        Set<Integer> set3 = new HashSet<>(set);
        set.add(4);
        System.out.println(set2);
        System.out.println(set3);

    }

}
