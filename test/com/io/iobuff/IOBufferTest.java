/*
 */
package com.io.iobuff;

import com.io.HashOutputStream;
import com.io.IOUtils;
import com.utils.hashes.Hashes;
import java.io.*;
import java.util.Random;
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
public class IOBufferTest {

    public IOBufferTest() {
    }
    static File file;
    static byte[] input;
    static String md5;
    final static int length = 1024 * 898;

    @BeforeClass
    public static void setUpClass() throws IOException {
        file = File.createTempFile("test_", null);
        try (BufferedOutputStream out = new BufferedOutputStream(new FileOutputStream(file));
                ByteArrayOutputStream bout = new ByteArrayOutputStream();
                HashOutputStream hout = new HashOutputStream(Hashes.Hash.MD5, bout);) {
            for (int i = 0; i < length; i++) {
                int val = i + (i % 10);
                out.write(val);
                hout.write(val);
            }
            out.flush();
            input = bout.toByteArray();
            md5 = hout.getAsString();
        }
    }

    @AfterClass
    public static void tearDownClass() {
        file.delete();
    }

    @Before
    public void setUp() {
    }

    @After
    public void tearDown() {
    }

    @Test
    public void testToString() throws IOException {

        assertEquals(md5, Hashes.hash(Hashes.Hash.MD5, file));
        assertEquals(length, file.length());

        IOBuffer buff = new IOBuffer("ddd");
        buff.loadFromFile(file);
        assertEquals(length, buff.length());

        IOBufferInput in = buff.getInputStream();
        assertEquals(1, buff.getOppenedInputs().size());
        assertEquals(md5, Hashes.md5(IOUtils.read(in)));

        buff.getOppenedInputs().iterator().next().close();
        assertEquals(0, buff.getOppenedInputs().size());

        //---------------------------
        buff.clear();

        buff.write("abc".getBytes());
        assertEquals(0, buff.length());
        buff.flush();
        assertEquals(3, buff.length());

        in = buff.getInputStream();
        assertEquals("abc", new String(IOUtils.read(in)));
        in.close();

        File ff = buff.getFile();
        //buff.flush();
        // IOUtils.copy(new ByteArrayInputStream(src), buff);
    }

}
