package com.utils;

import java.io.File;
import java.nio.file.attribute.BasicFileAttributes;
import org.junit.After;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;
import static org.junit.Assert.*;

public class PathTest {

    public PathTest() {
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
    public void t1() throws Exception {
        assertEquals("", new Path("aa/..").toString());
        assertEquals("bbb", new Path("aa/../bbb").toString());
        assertEquals("/bbb", new Path("/aa/../bbb").toString());
        assertEquals("/bbb", new Path("/aa/../..\\bbb").toString());
        assertEquals("/", new Path("/aa/bbb/../..").toString());
    }

    @Test
    public void envs() throws Exception {

        String temp = System.getenv("temp").replace("\\", "/");
        assertEquals(temp, new Path("%temp%").toString());

        assertEquals("%1/" + temp, new Path("%1/%temp%").toString());

    }

}
