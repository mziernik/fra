/*
 */
package com.utils;

import com.script.Nashorn;
import com.intf.callable.Callable1;
import javax.script.*;
import org.junit.Test;

import static org.junit.Assert.*;

/**
 *
 * @author user
 */
public class NashornTest {

    public NashornTest() {
    }

    public interface Adder {

        int sum(int a, int b);
    }

    @Test
    public void parse() throws ScriptException, NoSuchMethodException {

        String scr = "var n = ${nil}, "
                + "x = ${src.number}, "
                + "y = ${dst.text}, "
                + "z = ${boolean}, "
                + "u = ${unknown};";

        String res = Nashorn.parseVariablesScript(scr, (String var) -> {
            switch (var) {
                case "nil":
                    return new Unquoted("null");
                case "src.number":
                    return 123456;
                case "dst.text":
                    return "qwerty";
                case "boolean":
                    return true;

            }
            return null;
        });

        assertEquals("aa", res);

    }

    @Test
    public void testSomeMethod() throws ScriptException, NoSuchMethodException {

        Nashorn nashorn = new Nashorn();

        nashorn.eval("function sum(a, b) { return a + b; }");
        System.out.println(nashorn.eval("sum(1, 2);"));

        System.out.println(nashorn.invokeFunction("sum", 10, 2));

        final String script
                = "print(java.lang.System.getProperty(\"java.home\"));"
                + "print(\"Create file variable\");"
                + "var File = Java.type(\"java.io.File\");";

        nashorn.eval(script);

        Adder adder = nashorn.engine.getInterface(Adder.class);
        System.out.println(adder.sum(2, 3));

    }

    @Test
    public void testGetFactory() {
    }

    @Test
    public void testCreateBindings() {
    }

    @Test
    public void testEval_String() throws Exception {
    }

    @Test
    public void testCompile_String() throws Exception {
    }

    @Test
    public void testCompile_Reader() throws Exception {
    }

    @Test
    public void testInvokeFunction() throws Exception {
    }

    @Test
    public void testInvokeMethod() throws Exception {
    }

    @Test
    public void testGetInterface_Class() {
    }

    @Test
    public void testGetInterface_Object_Class() {
    }

    @Test
    public void testEval_Reader_ScriptContext() throws Exception {
    }

    @Test
    public void testEval_String_ScriptContext() throws Exception {
    }

    @Test
    public void testEval_Reader() throws Exception {
    }

    @Test
    public void testEval_String_Bindings() throws Exception {
    }

    @Test
    public void testEval_Reader_Bindings() throws Exception {
    }

    @Test
    public void testPut() {
    }

    @Test
    public void testGet() {
    }

    @Test
    public void testGetBindings() {
    }

    @Test
    public void testSetBindings() {
    }

    @Test
    public void testGetContext() {
    }

    @Test
    public void testSetContext() {
    }

    @Test
    public void testParseVariablesScript_String_Callable1() throws Exception {
    }

    @Test
    public void testParseVariablesScript_3args() throws Exception {
    }

}
