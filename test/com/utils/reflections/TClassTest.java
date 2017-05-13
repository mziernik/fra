package com.utils.reflections;

import com.utils.date.TDate;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Collection;
import java.util.Date;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;
import org.junit.After;
import org.junit.AfterClass;
import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

/**
 *
 * @author milosz
 */
public class TClassTest {

    public TClassTest() {
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

    public static Boolean boolC;
    public static boolean boolT;
    public static Integer intC;
    public static int intT;

    public static byte[] byteA;
    public static Byte[] ByteA;

    public static List<Integer> intList = new LinkedList<>();
    public static Set<Integer> intSet;
    public static Collection coll;

    public static Fruit fruit;

    public static Fruit[] fruitArray;

    public static List<Fruit> fruitList;

    public final static Map<Fruit, Integer> fruitCount = new HashMap<>();
    public static Record record;

    public static Date date;

    public static char c;

    /**
     * Test of parse method, of class TClass.
     */
    @Test
    public void testParse_String() throws NoSuchFieldException, ParseException {

        new TField(getClass().getField("boolC")).set(null, "true");
        assertEquals(Boolean.TRUE, boolC);

        new TField(getClass().getField("boolT")).set(null, "true");
        assertEquals(Boolean.TRUE, boolT);

        new TField(getClass().getField("intC")).set(null, "12345");
        assertEquals(Integer.valueOf(12345), intC);

        new TField(getClass().getField("intT")).set(null, "-9999");
        assertEquals(-9999, intT);

        new TField(getClass().getField("byteA")).set(null, "1", "5", "100");
        assertArrayEquals(new byte[]{1, 5, 100}, byteA);

        new TField(getClass().getField("ByteA")).set(null, "1", "5", "100");
        assertArrayEquals(new Byte[]{1, 5, 100}, ByteA);

        new TField(getClass().getField("intList")).set(null, "5", "10", "15", null);
        assertArrayEquals(new Integer[]{5, 10, 15, null}, intList.toArray(new Integer[0]));

        new TField(getClass().getField("intSet")).set(null, "5", "10", "15", null);
        assertArrayEquals(new Integer[]{5, 10, 15, null}, intSet.toArray(new Integer[0]));

        new TField(getClass().getField("coll")).set(null, "a", "b", "c");
        assertArrayEquals(new String[]{"a", "b", "c"}, coll.toArray(new String[0]));

        new TField(getClass().getField("fruit")).set(null, "orange");
        assertEquals(Fruit.orange, fruit);

        new TField(getClass().getField("c")).set(null, "a");
        assertEquals('a', c);

        new TField(getClass().getField("fruitArray")).set(null, "orange", "apple");
        assertArrayEquals(new Fruit[]{Fruit.orange, Fruit.apple}, fruitArray);

        new TField(getClass().getField("fruitList")).set(null, "orange", "apple");
        assertArrayEquals(new Fruit[]{Fruit.orange, Fruit.apple},
                fruitList.toArray(new Fruit[0]));

        new TField(getClass().getField("date")).set(null, "2015-05-12 22:11:13.013");
        assertEquals(new SimpleDateFormat(TDate.FULL_MS).parse("2015-05-12 22:11:13.013"), date);

        new TField(getClass().getField("record")).set(null, "{label='ll',value=12}");
        assertEquals(record.label, "ll");
        assertEquals(record.value, 12);

        new TField(getClass().getField("fruitCount")).set(null, "{bananna='4',apple=10,orange=3}");
        assertEquals(fruitCount.get(Fruit.bananna), Integer.valueOf(4));
        assertEquals(fruitCount.get(Fruit.apple), Integer.valueOf(10));
        assertEquals(fruitCount.get(Fruit.orange), Integer.valueOf(3));
    }

}

class Record {

    public String label;
    public int value;
}

enum Fruit {
    apple,
    orange,
    bananna,
    strawberry
}
