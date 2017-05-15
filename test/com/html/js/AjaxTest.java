/*
 */
package com.html.js;

import com.utils.text.StrWriter;
import com.html.core.tag.Element;
import com.html.js.core.JsAction;
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
public class AjaxTest {

    public AjaxTest() {
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

    /**
     * Test of reload method, of class Ajax.
     */
    @Test
    public void testToString() {

        StrWriter writer = new StrWriter();
        writer.append("  ");

        new Ajax("http://serwer:11/katalog?parametr")
                .onDone(new Eval("console.log('aaa');"))
                .addHeader("header1", "header1_value")
                .addHeader("header2", "header2_value")
                .param("parametr")
                .param("param2", "param2_value")
                .confirm("Potwierdź")
                .contentType("text/plain")
                .post("Treśc posta")
                .formId("id_formularza")
                .onError(false, new Eval("alert(this.error);"))
                .prompt("promptName", "promptMessage", "promptDefaultValue")
                .top(false)
                .formObj("form_obj")
                .getContent(writer);

        System.out.println(writer.toString());

    }

}
