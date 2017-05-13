/*
 */
package com.utils;

import java.net.MalformedURLException;
import java.net.URL;
import org.junit.Test;
import static org.junit.Assert.*;

/**
 *
 * @author user
 */
public class UrlTest {

    public UrlTest() {
    }

    @Test
    public void testSomeMethod() throws MalformedURLException {

        Url url = new Url("protokol://uzytkownik:haslo@serwer/folder/plik.txt?parametr=wartosc#hasz");

        URL url1 = new URL("http://serwer/folder/plik.txt");

        assertEquals("protokol", url.protocol());
        assertEquals("uzytkownik", url.username());
        assertEquals("haslo", url.password());
        assertEquals("/folder/plik.txt", url.toPath(false).toString());
        assertEquals("/serwer/folder/plik.txt", url.toPath(true).toString());
        assertEquals("wartosc", url.params().get("parametr"));
        assertEquals("hasz", url.hash());

    }

}
