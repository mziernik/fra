package com.model;

import com.model.dataset.AbstractDataSet;
import com.model.dataset.AbstractDataSet.Col;
import com.json.JArray;
import com.json.JObject;
import com.utils.collections.TList;
import java.io.IOException;
import static org.junit.Assert.assertEquals;
import org.junit.Test;

public class DataSetTest {

    class SimpleDS extends AbstractDataSet<SimpleDS, JObject, Integer> {

        final Col<Integer> pk = column(Integer.class, "klucz_glowny", "Klucz główny", obj -> obj.getInt("pk"))
                .primaryKey();
        final Col<String> login = column("login", "Login", obj -> obj.getStr("login"));
        final Col<Boolean> active = column("active", "Aktywny", obj -> obj.getBool("active"));

        public SimpleDS() {
            super("nazwa", "Tytuł");
            column(String.class, "a", "A", o -> "A");
            column(Integer.class, "xxxx", "YYYY", null);
            init();
        }

    }

    @Test
    public void testToString() throws IOException {

        SimpleDS ds = new SimpleDS();
        ds.fillRows(JArray.parse(getClass().getResource("data.json")).getObjects());

        for (SimpleDS dd : ds)
            System.out.println(dd);

        TList<SimpleDS> list = ds.asList();

        assertEquals("nazwa: klucz_glowny[PK] = 1, login = \"root\", active = true, a = \"A\", xxxx = null",
                list.removeFirst().toString());

        assertEquals("nazwa: klucz_glowny[PK] = 3, login = \"admin\", active = true, a = \"A\", xxxx = null",
                list.removeFirst().toString());

        assertEquals("nazwa: klucz_glowny[PK] = 6, login = \"jan\", active = false, a = \"A\", xxxx = null",
                list.removeFirst().toString());

    }

    @Test
    public void getData() throws IOException {
        SimpleDS ds = new SimpleDS();
        ds.fillRows(JArray.parse(getClass().getResource("data.json")).getObjects());

        SimpleDS x = ds.getByKey(6);
        assertEquals(1, x.size());
        assertEquals("nazwa: klucz_glowny[PK] = 6, login = \"jan\", active = false, a = \"A\", xxxx = null",
                x.toString());

    }
}
