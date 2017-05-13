/*
 */
package com.utils;

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
public class UrlBuilderTest {

    public UrlBuilderTest() {
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
    public void test1() {

        String str = new Url("/target").param("param").toString();

        Url url = new Url("https://użytkownik:hasło@subdomena.domena/katalog1/katalog2"
                + "?parametr1=wartość1&parametr2=wartość2#jakiś hasz");

        assertEquals("https", url.protocol());
        assertEquals("użytkownik", url.username());
        assertEquals("hasło", url.password());
        assertEquals("subdomena.domena", url.host());
        assertEquals("katalog1/katalog2", url.path().toString());
        assertEquals("parametr1=warto%C5%9B%C4%871&parametr2=warto%C5%9B%C4%872", url.params().toURI());
        assertEquals("jakiś hasz", url.hash());

        assertEquals("https://użytkownik:hasło@subdomena.domena/katalog1/katalog2"
                + "?parametr1=warto%C5%9B%C4%871&parametr2=warto%C5%9B%C4%872#jaki%C5%9B%20hasz", url.toString());
    }

}
