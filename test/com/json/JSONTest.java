/*
 */
package com.json;

import com.utils.Utils;
import com.json.exceptions.JException;
import com.utils.collections.Strings;
import java.io.*;
import java.util.*;
import org.junit.After;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

import static org.junit.Assert.*;

/**
 *
 * @author User
 */
public class JSONTest {

    public JSONTest() {
    }

    @BeforeClass
    public static void setUpClass() {
    }

    @AfterClass
    public static void tearDownClass() {
    }

    @Before
    public void setUp() {
        JOptions.defaultIntent = "  ";
    }

    @After
    public void tearDown() {
    }

    static class Foo {

        public final Map<String, String> map = new HashMap<>();
    }

    @Test
    public void deser() throws IOException {
        JElement el = JSON.parse("\"http://10.1.0.254/\"");

        System.out.println(el);
    }

    @Test
    public void deserialize() throws IOException {

        Foo foo = new Foo();
        JObject.parse("{map: { aaa: \"bbbb\"  } }").deserialize(foo);

        System.out.println(foo.map.get("aaa"));
    }

    @Test
    public void comments() {

        JObject json = new JObject();
        json.options.quotaNames(false);

        json.commentBefore("nagłówek");
        json.commentAfter("Stopka");
        json.put("zzzzz", "xxxxxx");
        json.put("aaa", 1);
        json.element("aaa").uncommented(true);
        json.put("bool", true);

        json.element("bool").commentAfter("za bool-em");
        json.element("bool").commentBefore("przed bool-em");

        assertEquals("/*nagłówek*/\n"
                + "{\n"
                + "  zzzzz: \"xxxxxx\",\n"
                + "  //aaa: 1,\n"
                + "  /*przed bool-em*/\n"
                + "  bool: true\n"
                + "  /*za bool-em*/\n"
                + "}\n"
                + "/*Stopka*/", json.toString());

        json.options.singleLine(true).intent("");

        assertEquals("/*nagłówek*/{zzzzz:\"xxxxxx\",//aaa:1,/*przed bool-em*/"
                + "bool:true/*za bool-em*/}/*Stopka*/", json.toString());

        json = JObject.parse("{\n"
                + "// odkomentowane\n"
                + "nazwa = 'wartość'\n"
                + " /*  komentarz */"
                + "\n}");

        assertEquals("{\n"
                + "  \"nazwa\": \"wartość\"\n"
                + "}", json.toString());

    }

    /**
     * Test of serialize method, of class JSON.
     */
    @Test
    public void parse() throws JException {
        JObject obj = JSON.parse("{ "
                + "v1 = 'Wartość1',"
                + "v2 : 'Wartość2'; "
                + "'v3' : 'Wartość 3' ,"
                + "obiekt : {"
                + "o1: 1, o2: true, o3: null, o4: 'aaa'"
                + "}, tablica : ["
                + "1, true, null, 'aaa']"
                + "  }").asObject();

        obj.elementF("v1").comment("aaa");
        obj.elementF("v3").uncommented(true);

        obj.elementF("obiekt").comment("komentarz obiektu");
        obj.elementF("tablica").uncommented(true);
    }

    @Test
    public void readOnly() {

        JObject parse = JObject.parse(JSON1);

        parse.options.quotaNames(false);
        parse.toString();

    }

    @Test
    public void printTest() {

        JObject obj = new JObject();
        obj.options.escapeUnicode(false).quotaNames(false);

        double[] dd = {1.0d, 1.1d, 1.9d, 2.5d, 3.0d, 5.0d};
        JObject jcars = obj.objectC("samochody");

        jcars.arrayC("typy").add("sedan").add("kombi").add("limuzyna");

        jcars.arrayC("pojemnosci").add(dd);

        /* JObject oAudi = obj.object("samochody", "osobowe", "audi");

         obj.object("samochody", "osobowe", "seat");

         JObject a8 = oAudi.object("A8");
         a8.put("kolor nadwozia", "czerowny");
         a8.put("limuzyna", true);
         a8.put("cena", 40000);
         a8.put("pojemność skokowa", 2.5d);

         obj.object("samochody").array("ciężarowe").addAll("man", "scania",
         new Object());
         */
        //       System.out.println(obj.toString());
    }

    @Test
    public void path() throws IOException {
        JObject json = new JObject();
        json.objectC("aaa");
        JObject bbb = json.objectC("bbb");
        json.objectC("ccc");
        json.arrayC("gggggg");
        bbb.object("b1");
        JArray arr = bbb.arrayC("b2");
        bbb.put("b3", 234234);
        arr.add("asd");
        JObject oo = arr.object();
        JElement dst = oo.addElement("dst", new JValue(null), false);
        assertEquals("\"bbb\"/\"b2\"/[1]/\"dst\"", dst.getPath().toString());

    }

    @Test
    public void addingTest() throws IOException {

        JObject obj = new JObject("pierwszy");

        obj.options.intent("  ")
                .quotaNames(false)
                .singleLine(true);

        obj.add("jeden", 1);
        obj.add("dwa", 2);
        obj.add("trzy", 3);
        obj.add("null", null);
        obj.insert("zero", 0);
        obj.insert("bool", false);
        obj.insert("obiekt", new Object());

        assertEquals("{obiekt: { }, bool: false, zero: 0, jeden: 1, dwa: 2, trzy: 3, null: null}", obj.toString());

        obj.sort();
        assertEquals("{bool: false, dwa: 2, jeden: 1, null: null, obiekt: { }, trzy: 3, zero: 0}", obj.toString());

        // posortuj na podstawie wartosci liczbowej
        obj.sort(new Comparator<JElement>() {
            @Override
            public int compare(JElement o1, JElement o2) {
                boolean a1 = o1.isValue() && o1.asValue().isNumber();
                boolean a2 = o2.isValue() && o2.asValue().isNumber();
                if (!a1)
                    return 1;
                if (!a2)
                    return -1;
                return Double.compare(o1.asValue().asNumber().doubleValue(), o2.asValue().asNumber().doubleValue());
            }
        });
        assertEquals("{zero: 0, jeden: 1, dwa: 2, trzy: 3, bool: false, null: null, obiekt: { }}", obj.toString());

        // ---------------------- przeniesienie calosci -------------------------
        JObject secondObject = new JObject();
        secondObject.options.intent("  ").quotaNames(false).singleLine(true);
        obj.move(secondObject);
        assertEquals("{zero: 0, jeden: 1, dwa: 2, trzy: 3, bool: false, "
                + "null: null, obiekt: { }}", obj.toString());
        //   assertEquals("{pierwszy: {zero: 0, jeden: 1, dwa: 2, trzy: 3, bool: false, obiekt: { }}}", secondObject.toString());

        // dodaj obiekt
        JObject nowy = secondObject.objectC("nowy");
        nowy.add("NowaWartosc", true);
        nowy.add("CzteryZera", new int[4]);

        assertEquals("{pierwszy: {zero: 0, jeden: 1, dwa: 2, trzy: 3, bool: false, null: null, obiekt: { }},"
                + " nowy: {NowaWartosc: true, CzteryZera: [0, 0, 0, 0]}}", secondObject.toString());
        nowy.remove();
        // przenies obiekt
        nowy.move(obj);

        //       "{pierwszy: {zero: 0, jeden: 1, dwa: 2, trzy: 3, bool: false, "
        assertEquals("{obiekt: {zero: 0, jeden: 1, dwa: 2, trzy: 3, bool: false, "
                + "null: null, obiekt: { }}}}", secondObject.toString());
        //  + "null: null, obiekt: { }, nowy: {NowaWartosc: true, CzteryZera: [0, 0, 0, 0]}}}"
    }

    private final static String JSON1 = "{"
            + "  menu: {"
            + "    header: 'SVG Viewer',"
            + "    items: ["
            + "      {"
            + "        id: 'Open'"
            + "      },"
            + "      { id: 'OpenNew',"
            + "        label: 'Open New'},"
            + "      null,"
            + "      { id: 'ZoomIn',"
            + "        label: 'Zoom In' },"
            + "      { id: 'ZoomOut',"
            + "        label: 'Zoom Out' },"
            + "      {"
            + "        id: 'OriginalView',"
            + "        label: 'Original View'"
            + "      },"
            + "      null,"
            + "      {"
            + "        id: 'Quality'"
            + "      },"
            + "      {"
            + "        id: 'Pause'"
            + "      },"
            + "      {"
            + "        id: 'Mute'"
            + "      },"
            + "      null,"
            + "      {"
            + "        id: 'Find',"
            + "        label: 'Find...'"
            + "      },"
            + "      {"
            + "        id: 'FindAgain',"
            + "        label: 'Find Again'"
            + "      },"
            + "      {"
            + "        id: 'Copy'"
            + "      },"
            + "      {"
            + "        id: 'CopyAgain',"
            + "        label: 'Copy Again'"
            + "      },"
            + "      {"
            + "        id: 'CopySVG',"
            + "        label: 'Copy SVG'"
            + "      },"
            + "      {"
            + "        id: 'ViewSVG',"
            + "        label: 'View SVG'"
            + "      },"
            + "      {"
            + "        id: 'ViewSource',"
            + "        label: 'View Source'"
            + "      },"
            + "      {"
            + "        id: 'SaveAs',"
            + "        label: 'Save As'"
            + "      },"
            + "      null,"
            + "      {"
            + "        id: 'Help'"
            + "      },"
            + "      {"
            + "        id: 'About',"
            + "        label: 'About Adobe CVG Viewer...'"
            + "      }"
            + "    ]"
            + "  }"
            + "}";

    private final static String JSON2 = "{"
            + "  web-app: {"
            + "    servlet: ["
            + "      {"
            + "        servlet-name: 'cofaxCDS',"
            + "        servlet-class: 'org.cofax.cds.CDSServlet',"
            + "        init-param: {"
            + "          'configGlossary:installationAt': 'Philadelphia, PA',"
            + "          'configGlossary:adminEmail': 'ksm@pobox.com',"
            + "          'configGlossary:poweredBy': 'Cofax',"
            + "          'configGlossary:poweredByIcon': '/images/cofax.gif',"
            + "          'configGlossary:staticPath': '/content/static',"
            + "          templateProcessorClass: 'org.cofax.WysiwygTemplate',"
            + "          templateLoaderClass: 'org.cofax.FilesTemplateLoader',"
            + "          templatePath: 'templates',"
            + "          templateOverridePath: '',"
            + "          defaultListTemplate: 'listTemplate.htm',"
            + "          defaultFileTemplate: 'articleTemplate.htm',"
            + "          useJSP: false,"
            + "          jspListTemplate: 'listTemplate.jsp',"
            + "          jspFileTemplate: 'articleTemplate.jsp',"
            + "          cachePackageTagsTrack: 200,"
            + "          cachePackageTagsStore: 200,"
            + "          cachePackageTagsRefresh: 60,"
            + "          cacheTemplatesTrack: 100,"
            + "          cacheTemplatesStore: 50,"
            + "          cacheTemplatesRefresh: 15,"
            + "          cachePagesTrack: 200,"
            + "          cachePagesStore: 100,"
            + "          cachePagesRefresh: 10,"
            + "          cachePagesDirtyRead: 10,"
            + "          searchEngineListTemplate: 'forSearchEnginesList.htm',"
            + "          searchEngineFileTemplate: 'forSearchEngines.htm',"
            + "          searchEngineRobotsDb: 'WEB-INF/robots.db',"
            + "          useDataStore: true,"
            + "          dataStoreClass: 'org.cofax.SqlDataStore',"
            + "          redirectionClass: 'org.cofax.SqlRedirection',"
            + "          dataStoreName: 'cofax',"
            + "          dataStoreDriver: 'com.microsoft.jdbc.sqlserver.SQLServerDriver',"
            + "          dataStoreUrl: 'jdbc:microsoft:sqlserver://LOCALHOST:1433;DatabaseName=goon',"
            + "          dataStoreUser: 'sa',"
            + "          dataStorePassword: 'dataStoreTestQuery',"
            + "          dataStoreTestQuery: 'SET NOCOUNT ON;select test='test';',"
            + "          dataStoreLogFile: '/usr/local/tomcat/logs/datastore.log',"
            + "          dataStoreInitConns: 10,"
            + "          dataStoreMaxConns: 100,"
            + "          dataStoreConnUsageLimit: 100,"
            + "          dataStoreLogLevel: 'debug',"
            + "          maxUrlLength: 500"
            + "        }"
            + "      },"
            + "      {"
            + "        servlet-name: 'cofaxEmail',"
            + "        servlet-class: 'org.cofax.cds.EmailServlet',"
            + "        init-param: {"
            + "          mailHost: 'mail1',"
            + "          mailHostOverride: 'mail2'"
            + "        }"
            + "      },"
            + "      {"
            + "        servlet-name: 'cofaxAdmin',"
            + "        servlet-class: 'org.cofax.cds.AdminServlet'"
            + "      },"
            + "      {"
            + "        servlet-name: 'fileServlet',"
            + "        servlet-class: 'org.cofax.cds.FileServlet'"
            + "      },"
            + "      {"
            + "        servlet-name: 'cofaxTools',"
            + "        servlet-class: 'org.cofax.cms.CofaxToolsServlet',"
            + "        init-param: {"
            + "          templatePath: 'toolstemplates/',"
            + "          log: 1,"
            + "          logLocation: '/usr/local/tomcat/logs/CofaxTools.log',"
            + "          logMaxSize: '',"
            + "          dataLog: 1,"
            + "          dataLogLocation: '/usr/local/tomcat/logs/dataLog.log',"
            + "          dataLogMaxSize: '',"
            + "          removePageCache: '/content/admin/remove?cache=pages&id=',"
            + "          removeTemplateCache: '/content/admin/remove?cache=templates&id=',"
            + "          fileTransferFolder: '/usr/local/tomcat/webapps/content/fileTransferFolder',"
            + "          lookInContext: 1,"
            + "          adminGroupID: 4,"
            + "          betaServer: true"
            + "        }"
            + "      }"
            + "    ],"
            + "    servlet-mapping: {"
            + "      cofaxCDS: '/',"
            + "      cofaxEmail: '/cofaxutil/aemail/*',"
            + "      cofaxAdmin: '/admin/*',"
            + "      fileServlet: '/static/*',"
            + "      cofaxTools: '/tools/*'"
            + "    },"
            + "    taglib: {"
            + "      taglib-uri: 'cofax.tld',"
            + "      taglib-location: '/WEB-INF/tlds/cofax.tld'"
            + "    }"
            + "  }"
            + "}";

}
