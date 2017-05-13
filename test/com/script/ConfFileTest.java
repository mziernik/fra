/*
 */
package com.script;

import com.script.ConfFile;
import com.io.IOUtils;
import com.json.JObject;
import com.script.evaluator.EvaluationException;
import com.sun.istack.Nullable;
import java.io.IOException;
import org.junit.Test;

/**
 *
 * @author user
 */
public class ConfFileTest {

    public ConfFileTest() {
    }

    public static void metoda(String param1) {

    }

    @Nullable
    Object obj = null;

    @Test
    public void testLoad() throws NoSuchMethodException, IOException, EvaluationException {
        ConfFile config = new ConfFile(IOUtils.readUtf(getClass().getResourceAsStream("config.conf")));
        JObject json = config.process();
        System.out.println(json);
        JObject jservice = json.objectD("service");

    }

}
