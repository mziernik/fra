/*
 */
package com.utils.hashes;

import java.io.File;
import java.io.InputStream;
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
public class HashesTest {

    public HashesTest() {
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
     * Test of idHash6 method, of class Hashes.
     */
    @Test
    public void testIdHash10() {

        assertEquals("NSRBwg", Hashes.idHash6("abc"));
        assertEquals("NSRBwmAD", Hashes.idHash8("abc"));
        assertEquals("kcjub0BCyQ2O", Hashes.idHash12("abc"));

    }

}
