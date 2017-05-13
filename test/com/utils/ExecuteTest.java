/*
 */
package com.utils;

import org.junit.Test;
import static org.junit.Assert.*;

/**
 *
 * @author user
 */
public class ExecuteTest {

    public ExecuteTest() {
    }

    @Test
    public void test() throws Exception {

        Execute.ExecResult result = new Execute("ipconfig", "/all").run();

        System.out.println(result.buffer);
    }

}
