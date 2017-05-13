/*
 */
package com.exceptions;

import java.io.IOException;
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
public class EErrorTest {

    public EErrorTest() {
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
     * Test of toString method, of class EError.
     */
    @Test
    public void testProcessSocketException() {

        EError err = new EError(
                new RuntimeException("!!!",
                        new IOException("aaaa",
                                new NumberFormatException("1234"))));

        assertEquals("[Runtime, IO, NumberFormat] !!!: aaaa: 1234", err.toString());

        err = new EError(
                new RuntimeException(
                        new IOException(
                                new NumberFormatException("1234"))));

        assertEquals("[Runtime, IO, NumberFormat] 1234", err.toString());

        err = new EError(
                new RuntimeException(
                        new IOException("IOERR",
                                new NumberFormatException("1234"))));

        assertEquals("[Runtime, IO, NumberFormat] IOERR: 1234", err.toString());

        assertEquals("[NumberFormat] 1234",
                new EError(new NumberFormatException("1234")).toString());

        assertEquals("[Exception] 1234",
                new EError(new Exception("1234")).toString());

        assertEquals("[Error] 1234",
                new EError(new Error("1234")).toString());

        assertEquals("[Throwable] 1234",
                new EError(new Throwable("1234")).toString());

    }
}
