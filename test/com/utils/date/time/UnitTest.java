/*
 */
package com.utils.date.time;

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
public class UnitTest {

    public UnitTest() {
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
    public void testMultipier() {
        // ilosc godzin w dniu
        assertTrue(new Double(24d).equals(Unit.HOURS.multipier(Unit.DAYS)));

        // ilosc godzin w dniu
        assertTrue(new Double(365d).equals(Unit.DAYS.multipier(Unit.YEARS)));

        assertTrue(new Double(3600d).equals(Unit.SECONDS.multipier(Unit.HOURS)));

        assertTrue(new Double(1d / 3600d).equals(Unit.HOURS.multipier(Unit.SECONDS)));
    }

}
