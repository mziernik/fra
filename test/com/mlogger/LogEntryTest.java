package com.mlogger;

import com.config.CService.ServiceMode;
import com.utils.Utils;
import com.io.TInputStream;
import com.io.TOutputStream;
import com.mlogger.LogElement.DataPair;
import com.mlogger.LogElement.DataPairs;
import com.utils.collections.Strings;
import com.utils.date.TDate;
import com.utils.hashes.Hashes;
import java.io.IOException;
import java.text.ParseException;
import java.util.*;
import org.junit.After;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

import static org.junit.Assert.*;

/**
 *
 * @author milosz
 */
public class LogEntryTest {

    public LogEntryTest() {
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

    @Test
    public void test1() throws ParseException, IOException {

        byte[] data;
        {
            Log log = new Log(null);
            log.kind.set(LogKind.EXCEPTION);
            log.address.add("127.0.0.1");
            log.address("8.8.8.8");
            log.attribute("Nagłówek", "Content-Type", "text/plain");
            log.attribute("Nagłówek", "Content-Disposition", "plik.txt");
            log.attribute("Metoda", "POST");
            log.attribute("Pusty atrybut");
            log.background("Black");
            log.color("red");
            log.className("com.mlogger.Log");
            log.comment("Komentarz");
            log.counter.set(10l);
            log.data("Zawartość", "?????");
            log.data("-xml-", "<root/>", DataType_old.XML);
            log.data("-hex-", "384732698476239876932", DataType_old.HEX);
            log.device("PC");
            log.hostName("user-komputer");
            log.userAgent("WebKit");
            log.os.set("Windows 10");
            log.date.set(new TDate("2010-01-01 12:12:12.777"));
            log.uid.set(UUID.fromString("7c2d8570-adc9-4f61-9c87-745a46707b5e"));
            log.errorStack.add(new Strings("Linia błędu A1", "Linia błędu A2").astList());
            log.errorStack.add(new Strings("Linia błędu B1", "Linia błędu B2").astList());
            log.callStack.clear();
            log.callStack.add(new Strings("Stos wywołań A1", "Stos wywołań A2").astList());
            log.callStack.add(new Strings("Stos wywołań B1", "Stos wywołań B2").astList());
            // log.fieldKey.
            log.instance.set("Instancja");
            log.flags.add("Flaga 1");
            log.flags.add("Flaga 2");
            log.group.set("Grupa");
            log.id = 12l;
            log.keys.add("Klucz 1");
            log.keys.add("Klucz 2");
            log.level.set(8);
            log.levelName.set("TRACE");
            log.logger.set("Junit Logger");
            log.method.set("log_test()");
            log.mode.set(ServiceMode.RELEASE);
            log.processId.set(1234l);
            log.threadId.set(54321l);
            log.progress.set(13.33);
            log.request.set("RequestID");
            log.session.set("SessionID");
            log.source.set("Testy jednostkowe");
            log.tag.add("Testy");
            log.tag("TAG");
            log.threadName.set("Nazwa wątku");
            log.threadPriority.set(10);
            log.url.add("http://url");
            log.url("www.wp.pl");
            log.user("ROOT");
            log.version.set("1.0");
            log.value("[\"Wartość\"]", DataType_old.JSON);

            TOutputStream out = new TOutputStream(true);
            for (LogEntry<?> en : log.entries) {
                TOutputStream osEntry = new TOutputStream(true);
                en.write(osEntry, null);
                byte[] buff = osEntry.memory();
                if (buff == null || buff.length == 0)
                    continue;

                out.write(en.attr.id);
                out.writeUnsignedDyn(buff.length);
                out.write(buff);

                //    System.out.println(en.attr.key + ": " + Hashes.md5(data));
            }
            data = out.memory();
        }

        System.out.println(Hashes.md5(data));
        //    assertEquals("B25C2EDF33461832FAC314FFE0D5B6F5", Hashes.md5(data));

        TInputStream in = new TInputStream(data);
        Log log = new Log(null);
        while (in.available() > 0) {
            LogAttr attr = LogAttr.get(in.readByte());
            int size = in.readUnsignedDynInt();
            byte[] buff = in.read(size);

            for (LogEntry<?> en : log.entries)
                if (en.attr == attr) {
                    en.read(buff);
                    System.out.println(attr.key + ": " + Utils.toString(en.value));
                    attr = null;
                    break;
                }
            if (attr != null)
                throw new UnsupportedOperationException();
        }

        //--------------------------------------------------
        assertEquals(LogKind.EXCEPTION, log.kind.value());

        ArrayList<String> list = new ArrayList<>(log.address.value());
        assertEquals("127.0.0.1", list.get(0));
        assertEquals("8.8.8.8", list.get(1));

        assertEquals("Instancja", log.instance.value());

        //----------------------------------------------------------------
        LogElement.DataPairs header = log.attributes.value.get("Nagłówek");
        assertEquals(2, header.size());
        {
            DataPair pair = header.get(0);
            assertEquals("Content-Type", pair.name);
            assertEquals("text/plain", pair.value);
        }
        {
            DataPair pair = header.get(1);
            assertEquals("Content-Disposition", pair.name);
            assertEquals("plik.txt", pair.value);
        }
        DataPairs attrs = log.attributes.value.get("");
        assertEquals(2, attrs.size());
        {

            DataPair pair = attrs.get(0);
            assertEquals("Metoda", pair.name);
            assertEquals("POST", pair.value);
        }
        {

            DataPair pair = attrs.get(1);
            assertEquals("", pair.name);
            assertEquals("Pusty atrybut", pair.value);
        }

//                            log.attribute("Nagłówek", "Content-Type", "text/plain");
//            log.attribute("Content-Disposition", "plik.txt");
//            log.attribute("Metoda", "POST");
//            log.attribute("Pusty atrybut");
//
//
//        
//        JsonBuilder jb = new JsonBuilder();
//        log.toJson(jb, null);
    }
}
