/*
 */
package com.html.js.core;

import com.html.js.*;
import org.junit.Test;

import static org.junit.Assert.*;

/**
 *
 * @author user
 */
public class JsActionTest {

    public JsActionTest() {
    }

    @Test
    public void t1() {

//        String toString = new Timeout(100, new Alert("Minęło 100 ms"))
//                .param("param1", 1)
//                .toString();
        String toString = new Eval("var x =").append(new JQuery("#edt1").val()).toString();
        String toString1 = new Alert(new JQuery("#edt1").val()).toString();

        System.out.println("");
    }

}
