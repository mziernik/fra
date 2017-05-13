/*
 */
package com.utils.reflections;

import org.junit.After;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;
import static org.junit.Assert.*;

/**
 *
 * @author user
 */
public class TExecutableTest {

    public TExecutableTest() {
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

    @Test
    public void sdfdsf() throws ClassNotFoundException {

        // com.utils.collections.Strings.class
        TClass<?> cls = new TClass<>("com.utils.collections.Strings");

        TField ff = new TField("com.utils.collections.Strings.trim");

    }

    /**
     * Test of newInstance method, of class TExecutable.
     */
    @Test
    public void constructors() {

        TExecutableTest self = (TExecutableTest) new TExecutable(this,
                TExecutableTest.class).invoke();

        final TExecutable exec = new TExecutable(this, CC.class);

        // --------------- dopasowanie 1 : 1 ----------------------------
        assertEquals(0, ((CC) exec.invoke()).mode);
        assertEquals(1, ((CC) exec.invoke("abc")).mode);
        assertEquals(2, ((CC) exec.invoke(11)).mode);
        assertEquals(3, ((CC) exec.invoke("aaa", 12)).mode);
        assertEquals(4, ((CC) exec.invoke("aaa", 12, false)).mode);

        // ---------------- dopasowanie NULL-i -------------------
        assertEquals(1, ((CC) exec.invoke(new Object[]{null})).mode);
        assertEquals(3, ((CC) exec.invoke(new Object[]{null, 0})).mode);
        assertEquals(4, ((CC) exec.invoke(new Object[]{null, null, null})).mode);
        assertEquals(3, ((CC) exec.invoke("aaa", 12)).mode);

    }

    @Test
    public void methods() {

        // ----------------------- metody ---------------------
        final TExecutable exec = new TExecutable(new CC(), CC.class.getDeclaredMethods());

        // --------------- dopasowanie 1 : 1 ----------------------------
        assertEquals(0, exec.invoke());
        assertEquals(1, exec.invoke("abc"));
        assertEquals(2, exec.invoke(11));
        assertEquals(3, exec.invoke("aaa", 12));
        assertEquals(4, exec.invoke("aaa", 12, false));

        // ---------------- dopasowanie NULL-i -------------------
        assertEquals(1, exec.invoke(new Object[]{null}));
        assertEquals(3, exec.invoke(new Object[]{null, 0}));
        assertEquals(4, exec.invoke(new Object[]{null, null, null}));
        assertEquals(3, exec.invoke("aaa", 12));
    }

    private class CC {

        final int mode;

        public CC() {
            mode = 0;
        }

        public CC(String s1) {
            mode = 1;
        }

        public CC(int i) {
            mode = 2;
        }

        public CC(String str, int i) {
            mode = 3;
        }

        public CC(String str, Integer i, Boolean bb) {
            mode = 4;
        }

        public int mode0() {
            return 0;
        }

        public int mode1(String s1) {
            return 1;
        }

        public int mode2(int i) {
            return 2;
        }

        public int mode3(String str, int i) {
            return 3;
        }

        public int mode4(String str, Integer i, Boolean bb) {
            return 4;
        }
    }

}
