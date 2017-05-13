package com.script.evaluator;

import com.script.evaluator.element.EValue;
import com.utils.Undefined;
import java.io.File;
import java.util.LinkedHashMap;
import java.util.LinkedList;
import java.util.Map;
import org.junit.Test;
import static org.junit.Assert.*;
import org.junit.BeforeClass;
import org.junit.FixMethodOrder;
import org.junit.runners.MethodSorters;

@FixMethodOrder(MethodSorters.NAME_ASCENDING)
public class EvaluatorTest {

    @BeforeClass
    public static void setUp() {

        //    Evaluator e = new Evaluator("plik = file.getAbsolutePath();");
        /*   Evaluator e = new Evaluator("plik = import('http://api.ipify.org')");
        e.constants.put("file", new File(""));
        e.eval();

        Object get = e.variables.get("plik");
        System.out.println(get);
         */
    }

    @Test
    public void test01() throws EvaluationException {

        assertEquals(4, new Evaluator(" /*c1*/ 2 / 2 // pierwsza   \n"
                + "1 + 3 # druga\n  \n \t ").eval().asInteger());

        assertEquals("H2O", new Evaluator("'H' + 2 + \"O\"").eval().asString());

        assertEquals(6, new Evaluator("2 * 2 + 2").eval().asInteger());

        assertEquals(6, new Evaluator("2 + 2 * 2").eval().asInteger());
        assertEquals(2, new Evaluator("2 + 2 - 2").eval().asInteger());
        assertEquals(9, new Evaluator(" 12 / 4 + 2 * 3").eval().asInteger());

        assertTrue(new Evaluator("2==2").eval().asBoolean());

    }

    @Test
    public void test02() throws EvaluationException {

        assertEquals(5, new Evaluator("abs(abs(-5))").eval().asInteger());
        assertEquals("bDDD", new Evaluator("charAt('abc', 1) + upperCase('ddd')").eval().asString());

        {
            Evaluator evaluator = new Evaluator("a = 1; b = 'qaz' \n c = null; 12");
            assertEquals(12, evaluator.eval().asInteger());
            assertEquals(1l, evaluator.variables.get("a"));
            assertEquals("qaz", evaluator.variables.get("b"));
            assertEquals(null, evaluator.variables.get("c"));
        }

        assertEquals(123, new Evaluator("x = 123; x")
                .setVariableResolver((String variableName, Object value) -> {
                    return value == Undefined.TYPE ? null : value;
                })
                .setVariableAssigner((String variableName, EValue value) -> {
                    value.evaluator.variables.put(variableName, value.getValue());
                })
                .eval().asInteger());

        assertEquals(3, new Evaluator("var = 2 + abs(-1)")
                .setVariableResolver((String variableName, Object value) -> {
                    switch (variableName) {
                        case "PI":
                            return Math.PI;
                        case "E":
                            return Math.E;
                    }
                    return null;
                })
                .eval().asInteger());

//        new Evaluator().eval(" 2 + 2 * 2 - random   (0.3) + 5 10 "
//                + "15 abs(-3) + new Integer( '13' ) - !3");
        // assertEquals(10, new Evaluator().eval("10 - 3 + 1 * 2 * 4 - 5").asInteger());
        assertEquals(5, new Evaluator("5").eval().asInteger());

        Evaluator e = new Evaluator("$intf = 'interfejs';\n"
                + "global = {\n"
                + "    a.b.c.          = 'A.B.C.'\n"
                + "    domain          = $intf\n"
                + "    listenIP        = $intf + '2';\n"
                + "    publicPort      = (80 + 1)\n"
                + "    localPort       = 8080\n"
                + "    arr = [1,2,3, true, 'xxx']\n"
                + "    obj = {aaa : 'AAAA'; bbb: 'BBB'}"
                + "};\n\n\n"
                + "global.qaz = 'QAZ';"
                + "xtrue = global.arr.3;"
                + "xdomain = global.domain;"
                + "xaaa = global.obj.aaa;"
                + "qaz = global.qaz"
                + "").eval().evaluator;

        Map<String, String> global = (Map<String, String>) e.variables.get("global");
        assertEquals("A.B.C.", global.get("a.b.c."));

        assertEquals("interfejs", e.variables.get("xdomain"));
        assertEquals(true, e.variables.get("xtrue"));
        assertEquals("AAAA", e.variables.get("xaaa"));
        assertEquals("QAZ", e.variables.get("qaz"));
    }

    @Test
    public void test03_if() throws EvaluationException {

        assertEquals(3l, new Evaluator(
                "a = 5; "
                + "if(2 == 1){ a = 222 };"
                + "if(2 == 2){ a = 3 };")
                .eval().evaluator.variables.get("a"));
    }

    @Test
    public void test04_arrays() throws EvaluationException {

        LinkedList<Object> list = new LinkedList<>();
        list.add(12l);
        list.add("DDD");
        list.add(false);
        assertEquals(list, new Evaluator("[abs(10 + 2), upperCase('ddd'), 2 > 4]").eval().asArray());
    }

    @Test
    public void test05_objects() throws EvaluationException {

        LinkedHashMap<Object, Object> map = new LinkedHashMap<>();
        map.put("dwanascie", 12l);
        map.put('b', "DDD");
        map.put(3l, false);
        assertEquals(map, new Evaluator("{"
                + "'dwa' + 'nascie': abs(10 + 2), "
                + "charAt('abc', 1): upperCase('ddd'), "
                + "3: 2 > 4}").eval().asObject());
    }

    @Test
    public void test06_arrayInMap() throws EvaluationException {

        LinkedList list = new LinkedList<>();
        list.add(12l);
        list.add("DDD");
        list.add(false);

        LinkedHashMap map = new LinkedHashMap<>();

        map.put("lista", list);
        map.put("dwanascie", 12l);
        map.put('b', "DDD");
        map.put("c:c", false);

        assertEquals(map, new Evaluator("{lista = [abs(10 + 2), upperCase('ddd'), 2 > 4],"
                + "'dwa' + 'nascie': abs(10 + 2), "
                + "charAt('abc', 1): upperCase('ddd'), "
                + "'c:c': 2 > 4}").eval().asObject());

    }

    @Test
    public void test07_mapInArray() throws EvaluationException {

        LinkedHashMap map = new LinkedHashMap<>();
        map.put("dwanascie", 12l);
        map.put('b', "DDD");
        map.put("c:c", false);

        LinkedList list = new LinkedList<>();
        list.add(12l);
        list.add("DDD");
        list.add(false);
        list.add(map);

        assertEquals(list, new Evaluator("[abs(10 + 2), upperCase('ddd'), 2 > 4, {"
                + "'dwa' + 'nascie': abs(10 + 2), "
                + "charAt('abc', 1): upperCase('ddd'), "
                + "'c:c': 2 > 4}]").eval().asArray());

    }

}
