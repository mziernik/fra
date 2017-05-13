/*
 */
package com.resources.core;

import java.io.IOException;
import org.junit.Test;
import static org.junit.Assert.*;

/**
 *
 * @author user
 */
public class ResDataTest {

    public ResDataTest() {
    }

    @Test
    public void testGetSourceURL() {
    }

    @Test
    public void testGetSourceType() {
    }

    @Test
    public void testGetFileName() throws IOException {

        ResData res = new ResData(null, "/META-INF/fra/config.conf");

        String data = res.getStringUtf8();

        assertTrue(data.startsWith("# Plik konfiguracyjny usługi "));

        res.getStringUtf8();

    }

}
