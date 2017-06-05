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
public class FunctionTest {

    public FunctionTest() {
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
     * Test of body method, of class Function.
     */
    @Test
    public void testToString() {
        StrWriter writer = new StrWriter();

        writer.append("<script>");

        writer.nextLevel(() -> {
            writer.br().intent();

            new Function("nazwa_funkcji", "param1", "param2")
                    .body(new Alert("Komunikat"))
                    .body(new Call("document.reload", true))
                    .body("/* komentarz\n"
                            + "1 linia\n"
                            + "\t2 linia\n"
                            + "*/")
                    .body(new Load("http://google.pl"))
                    .body(new Prompt("zmienna", "Tytuł", "Pytanie?", new Alert("OK")))
                    .body(new OnLoadDoc(new Alert("Wczytano dokument")))
                    .body(new Timeout(1000, new Alert("Upłynęła 1 sekunda"))
                    //.param("pram1")
                    // .param(2)
                    )
                    .getContent(writer);
        });

        writer.br().intent().append("</script>");

        System.out.println(writer.toString());

    }

}
