package com.thread;

import java.io.IOException;
import org.junit.After;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;
import static org.junit.Assert.*;

public class ThreadDumpTest {

    public ThreadDumpTest() {
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
    public void testSomeMethod() throws InterruptedException {

        new TThread("dfsdf") {
            @Override
            protected void run() throws Exception {
                method1();
            }

            @Override
            protected void onException(Throwable e) {
                super.onException(e); //To change body of generated methods, choose Tools | Templates.
            }

        }.start().join();

    }

    private void method1() throws IOException {
        throw new IOException("aaaaa");

    }

}
