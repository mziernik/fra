/*
 */
package com.json;

import com.utils.Utils;
import com.google.gson.*;
import com.google.gson.reflect.TypeToken;
import com.json.exceptions.JException;
import com.utils.TObject;
import com.utils.collections.Strings;
import java.io.*;
import java.lang.reflect.Modifier;
import java.lang.reflect.Type;
import java.util.*;
import org.junit.After;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

import static org.junit.Assert.*;

/**
 *
 * @author User
 */
public class Deserializacja {

    public Deserializacja() {
    }

    @BeforeClass
    public static void setUpClass() {
    }

    @AfterClass
    public static void tearDownClass() {
    }

    @Before
    public void setUp() {
        JOptions.defaultIntent = "  ";
    }

    @After
    public void tearDown() {
    }

    static class Item {

        public String name;
        public String value;
    }

    static class Foo {

        public final HashMap<String, String> map = new HashMap<>();
        public final List<Item> items = new LinkedList<>();
        public String text;
        //    public final TObject<String> tstr = new TObject<>();
        public int number;
    }

    @Test
    public void deserialize() throws IOException {

        Foo foo = new Foo();

        JObject json = JObject.parse(getClass().getResource("deserialize.json"));

        json.deserialize(foo);

        assertEquals("Element", foo.map.get("element"));

        assertEquals(1, foo.items.size());
    }

}
