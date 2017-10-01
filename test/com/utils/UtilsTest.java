/*
 */
package com.utils;

import com.utils.Utils;
import com.json.JObject;
import com.utils.TCurrency;
import com.utils.collections.Strings;
import java.util.function.Predicate;
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
public class UtilsTest {

    public UtilsTest() {
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
    public void frmt() {
        assertEquals(" int=1234 bool=true tekst=\"xxx\"", Utils.frmt(" int=%3 bool=%2 tekst=%1", "xxx", true, 1234));
    }

    @Test
    public void range() {

        assertEquals(3, (int) Utils.range(0, 3, 5));
        assertEquals(5, (int) Utils.range(8, 3, 5));
        assertEquals(0, (int) Utils.range(0, null, 5));
        assertEquals(8, (int) Utils.range(8, 3, null));
        assertEquals(8, (int) Utils.range(8, null, null));
    }

    @Test
    public void test_findFirst() {

        Strings sss = new Strings("a", "b", "c");
        Utils.findFirst(sss.astList(), (String t) -> {
            System.out.println(t);
            return true;
        });

    }

    /**
     * Test of formatDate method, of class Utils.
     */
    @Test
    public void testFormatDate() {

        double xx = 945.7d + 27.93d;

        //   tam: 973.6300000000001
        double dd = Math.round(405.29999999999995d * 100d) / 100d;

        new TCurrency(405.29999999999995d).toString();
        TCurrency c = new TCurrency(405.29999999999995d);

        c.intValue();
        c.doubleValue();
        c.byteValue();

        JObject j = new JObject();
        j.put("cc", new TCurrency(405.29999999999995d));

        j.toString();

    }

}
