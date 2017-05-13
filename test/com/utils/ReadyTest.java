/*
 */
package com.utils;

import com.intf.runnable.RunnableEx1;
import org.junit.Test;

/**
 *
 * @author user
 */
public class ReadyTest {

    public ReadyTest() {
    }

    @Test
    public void test1_errors() throws InterruptedException {

        Ready.on((Throwable error) -> {
            System.out.println(error);
        }, Integer.class);

        Ready.confirm(Integer.class, new Error("Błąd inta"));

        Ready.on((Throwable error) -> {
            System.out.println(error);
        }, Integer.class);

        try {
            Ready.waitFor(Integer.class);
        } catch (Throwable e) {
            System.out.println(e);
        }

        Thread.sleep(100);

    }

}
