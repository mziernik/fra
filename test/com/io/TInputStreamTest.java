/*
 */
package com.io;

import java.io.*;
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
public class TInputStreamTest {

    /**
     * Test of addMirror method, of class TInputStream.
     */
    @Test
    public void testAddMirror() throws IOException {

        TInputStream in = new TInputStream(new ByteArrayInputStream("Ala ma kota".getBytes()));

        byte[] buff = new byte[3];
        in.readFully(buff);

        String string = new String(buff);

    }

}
