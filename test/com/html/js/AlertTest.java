/*
 */
package com.html.js;

import com.utils.text.StrWriter;
import com.html.core.tag.Element;
import com.html.js.core.JsAction;
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
public class AlertTest {

    public AlertTest() {
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
     * Test of setTag method, of class Alert.
     */
    @Test
    public void testToString() {

        StrWriter writer = new StrWriter();

        new Alert("nagłówek", "Treść").getContent(writer);

        System.out.println(writer.toString());
    }

}
