package com.config.engine.field.core;

import com.config.engine.field.CfString;
import com.config.engine.field.CfStringList;
import com.json.JArray;
import com.utils.Utils;
import com.utils.collections.Strings;
import com.utils.collections.TList;
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
public class ConfigFieldTest {

    public ConfigFieldTest() {
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
     * Test of getStructure method, of class ConfigField.
     */
    @Test
    public void test1() throws Exception {
        CfString field = new CfString(null, null, null);

        assertNull(field.value());
        field.setDefaultValue("def");
        assertEquals("def", field.value());

        field.setUserValue("val");
        assertEquals("def", field.value());
        field.setDefaultState(false);
        assertEquals("val", field.value());

//        assertEquals("\"def\"", field.getValueJson(true).toString());
//        assertEquals("\"val\"", field.getValueJson(false).toString());
        //assertEquals("val", field.getStructure().toString());
    }

    @Test
    public void test2() throws Exception {
        CfStringList field = new CfStringList(null, null);

        TList<String> value = field.value();

        field.addValue("a");
        field.addValue("b");

        assertTrue(field.value().isEmpty());

        field.setDefaultValue(Utils.asList("1", "2", "3"));

        assertEquals("1, 2, 3", new Strings(field.value()).toString());

        field.setDefaultState(false);

        assertEquals("a, b", new Strings(field.value()).toString());

        //----------------------
    }

}
