/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package com.utils.collections;

import com.intf.runnable.RunnableEx;
import com.utils.Utils;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Iterator;
import java.util.LinkedHashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Spliterator;
import java.util.function.Consumer;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.stream.Stream;
import org.junit.After;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;
import static org.junit.Assert.*;

/**
 *
 * @author milosz
 */
public class ListTest {

    public ListTest() {
    }

    @BeforeClass
    public static void setUpClass() {
    }

    @AfterClass
    public static void tearDownClass() {
    }

    @Before
    public void setUp() {
    }

    @After
    public void tearDown() {
    }

    /**
     * Test of readOnly method, of class TList.
     */
    @Test
    public void testReadOnly() throws Exception {

        Performance p = new Performance(10);

        p.run("GlueList", () -> {
            TList<Integer> list = new TList<>();
            for (int i = 0; i < 1000_000; i++)
                list.add(i);
        });

        p.run("ArrayList", () -> {
            ArrayList<Integer> list = new ArrayList<>();
            for (int i = 0; i < 1000_000; i++)
                list.add(i);

        });

        p.run("LinkedList", () -> {
            LinkedList<Integer> list = new LinkedList<>();
            for (int i = 0; i < 1000_000; i++)
                list.add(i);
        });

    }
}

class Performance {

    public int count;
    public final List<Test> testst = new LinkedList<>();

    public Performance(int count) {
        this.count = count;
    }

    public void run(String name, RunnableEx runnable) throws Exception {
        testst.add(new Test(name, runnable));
    }

    public class Test {

        public final String name;
        public final double min;
        public final double max;
        public final double avg;
        public final double med;
        public final double[] times;

        Test(String name, RunnableEx runnable) throws Exception {
            this.name = name;
            ArrayList<Double> times = new ArrayList<>();
            for (int i = 0; i < count; i++) {
                long ts = System.nanoTime();
                runnable.run();
                ts = System.nanoTime() - ts;
                times.add(ts / 1000_000d);
            }

            double max = Double.MIN_VALUE;
            double min = Double.MAX_VALUE;

            double avg = 0;

            double[] tt = new double[count];
            for (int i = 0; i < count; i++)
                tt[i] = times.get(i);
            this.times = tt;

            // System.out.println(new Strings(times).toString(", ") + " ms");
            Collections.sort(times);

            double med = count / 2d;
            if (count % 2 == 1)
                med = times.get((int) med);
            else
                med = (times.get((int) med - 1) + times.get((int) med)) / 2;

            for (double l : times) {
                if (l > max)
                    max = l;
                if (l < min)
                    min = l;
                avg += l;
            }

            this.min = min;
            this.max = max;
            this.med = med;
            this.avg = avg /= (double) count;

            System.out.println(name + ": min "
                    + Utils.formatFloat(min, ".") + ", max "
                    + Utils.formatFloat(max, ".") + ", avg "
                    + Utils.formatFloat(avg, ".") + ", med "
                    + Utils.formatFloat(med, ".") + " ms");

        }
    }
}
