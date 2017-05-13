/*
 */
package com.utils.hashes;

import com.utils.Utils;
import context.ServiceTest;
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
public class CryptoTest extends ServiceTest {

    public CryptoTest() {
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
     * Test of encrypt method, of class Crypto.
     */
    @Test
    public void testEncrypt() {

        //  AppConfigBridge.setUpClass();
        String str = "Zażółć gęślą jaźń";
        byte[] data = Crypto.encrypt(str.getBytes(Utils.UTF8));
        data = Crypto.decrypt(data);
        assertEquals(str, new String(data, Utils.UTF8));
    }

}
