/*
 */
package com.utils.date.time;

import com.utils.date.time.*;
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
public class TTimeTest {

    public TTimeTest() {
    }

    @BeforeClass
    public static void setUpClass() {
    }

    @AfterClass
    public static void tearDownClass() {
    }

    private Interval full;

    private Interval hourAndMs;

    @Before
    public void setUp() {
        // 3 lata, 100 dni, 14 godz, 44 min, 59 sek, 333 milisek
        full = new Interval(
                (long) (333)
                + (long) (59 * 1000)
                + (long) (44 * 60 * 1000)
                + (long) (14 * 60 * 60 * 1000)
                + (long) (100l * 24l * 60l * 60l * 1000l)
                + (long) (3l * 365l * 24l * 60l * 60l * 1000l),
                Unit.MILLISECONDS);

        // 2 godz, 800 ms
        hourAndMs = new Interval(2 * 60 * 60 * 1000 + 800, Unit.MILLISECONDS);
    }

    @Test
    public void checkParts() {
        assertEquals(3, full.getPart(Unit.YEARS));
        assertEquals(100, full.getPart(Unit.DAYS));
        assertEquals(14, full.getPart(Unit.HOURS));
        assertEquals(44, full.getPart(Unit.MINUTES));
        assertEquals(59, full.getPart(Unit.SECONDS));
        assertEquals(333, full.getPart(Unit.MILLISECONDS));
    }

    @Test
    public void isSame() {
        assertFalse(new Interval(10, 15, 23, 111)
                .isSame(new Interval(10, 15, 23)));

        assertTrue(new Interval(10, 15, 23, 111)
                .isSame(new Interval(10, 15, 23), Unit.SECONDS));

        assertTrue(new Interval(10, 15, 11)
                .isSame(new Interval(10, 15, 22), Unit.MINUTES));

    }

    @Test
    public void floatMilliseconds() {
        assertEquals("201ms", new Interval(200, Unit.MILLISECONDS).add(678, Unit.MICROSECONDS).toString());
        assertEquals("22,7ms", new Interval(22, Unit.MILLISECONDS).add(678, Unit.MICROSECONDS).toString());
    }

    @Test
    public void set() {
        Interval time = new Interval(10, 0, 0);
        assertEquals("10:00:00", time.toStringFrmtS());

        time.setTime(20, Unit.MINUTES);
        assertEquals("00:20:00", time.toStringFrmtS());

        time.setPart(45, Unit.SECONDS);
        assertEquals("00:20:45", time.toStringFrmtS());

        time.setPart(123, Unit.NANOSECONDS);
        time.setPart(987, Unit.MICROSECONDS);
        assertEquals("20m 45s 987us 123ns", time.toString());
    }

    @Test
    public void add() {

        assertEquals("10:15:00",
                new Interval(10, 0, 0)
                        .add(15, Unit.MINUTES)
                        .toStringFrmtS());

        assertEquals("11:20:03.222",
                new Interval(10, 0, 0)
                        .add(80, Unit.MINUTES)
                        .add(3, Unit.SECONDS)
                        .add(222, Unit.MILLISECONDS)
                        .toStringFrmtMS());

        assertEquals("09:45:00",
                new Interval(10, 0, 0)
                        .add(-15, Unit.MINUTES)
                        .toStringFrmtS());

    }

    @Test
    public void diff() {
        assertEquals("-20s",
                new Interval(10, 15, 11)
                        .diff(new Interval(10, 15, 31)).toString());

        assertEquals("1s 987ms",
                new Interval(10, 15, 11, 987)
                        .diff(new Interval(10, 15, 10)).toString());
    }

    @Test
    public void separator() {
        // domyślny separator
        assertEquals("3y 100d 14h 44m 59s 333ms", full
                .format()
                .toString());

        // zadeklarowany separator
        assertEquals("3y-100d-14h-44m-59s-333ms", full
                .format()
                .separator("-")
                .toString());

        // zadeklarowany separator
        assertEquals("3y100d14h44m59s333ms", full
                .format()
                .separator("")
                .toString());
    }

    @Test
    public void space() {
        // domyślny odstęp
        assertEquals("3y 100d 14h 44m 59s 333ms", full
                .format()
                .toString());

        // zadeklarowany odstep
        assertEquals("3-y 100-d 14-h 44-m 59-s 333-ms", full
                .format()
                .unitSpace("-")
                .toString());
    }

    @Test
    public void constLength() {

        // 20 minut, 3 sekundy, 13 milisekund
        Interval time = new Interval(20 * 60 * 1000 + 3 * 1000 + 13, Unit.MILLISECONDS);

        // domyślnie
        assertEquals("20m 3s 13ms",
                time.format()
                        .toString());

        // stały rozmiar
        assertEquals("20m 03s 013ms",
                time.format()
                        .constValLen(true)
                        .toString());

        // stały rozmiar, brak jednostki, separatory
        assertEquals("20-03-013",
                time.format()
                        .constValLen(true)
                        .separator("-")
                        .unitMode(UnitMode.NONE)
                        .toString());

    }

    @Test
    public void get() {
        // 20 minut, 55 sekundy, 666 milisekund
        Interval time = new Interval(20 * 60 * 1000 + 55 * 1000 + 666, Unit.MILLISECONDS);
        assertEquals("20m 55s 666ms", time.toString());
        assertEquals(1255666, time.getTime(Unit.MILLISECONDS));
        assertEquals(21, time.getTime(Unit.MINUTES));
    }

    @Test
    public void unitMode() {
        // domyślna jednostka
        assertEquals("3y 100d 14h 44m 59s 333ms", full
                .format()
                .toString());

        // brak jednostki
        assertEquals("3 100 14 44 59 333", full
                .format()
                .unitMode(UnitMode.NONE)
                .toString());

        // pełne
        assertEquals("3lata 100dni 14godzin 44minut 59sekund 333milisekund",
                full.format()
                        .unitMode(UnitMode.FULL)
                        .toString());

        // pełne + odstępy
        assertEquals("3 lata, 100 dni, 14 godzin, 44 minut, 59 sekund, 333 milisekund",
                full.format()
                        .unitMode(UnitMode.FULL)
                        .unitSpace(" ")
                        .separator(", ")
                        .toString());
    }

    @Test
    public void masked() {
        assertEquals("3y 100d 14:44:59.333",
                full.format()
                        .unitMode(UnitMode.SHORT)
                        .toString(true));

        assertEquals("3y 100d 14:44:59.333 000",
                full.format()
                        .precision(Unit.MICROSECONDS)
                        .unitMode(UnitMode.SHORT)
                        .toString(true));

        assertEquals("3y 100d 14:44:59.333 000 000",
                full.format()
                        .precision(Unit.NANOSECONDS)
                        .unitMode(UnitMode.SHORT)
                        .toString(true));

        assertEquals("3509:44:59.333",
                full.format()
                        .base(Unit.HOURS)
                        .precision(Unit.MILLISECONDS)
                        .unitMode(UnitMode.SHORT)
                        .toString(true));
    }

    @Test
    public void includeZeros() {

        // domyslna konfiguracja
        assertEquals("2h 800ms", hourAndMs
                .format()
                .toString());

        // brak zer
        assertEquals("2h 800ms", hourAndMs
                .format()
                .includeZeroValues(false)
                .toString());

        // brak zer przy precyzji nanosekundowej
        assertEquals("2h 800ms", hourAndMs
                .format()
                .precision(Unit.NANOSECONDS)
                .includeZeroValues(false)
                .toString());

        // uwzględnij zera
        assertEquals("2h 0m 0s 800ms", hourAndMs
                .format()
                .includeZeroValues(true)
                .toString());

        // uzwględnij zera przy precyzji nanosekundowej
        assertEquals("2h 0m 0s 800ms 0us 0ns", hourAndMs
                .format()
                .precision(Unit.NANOSECONDS)
                .includeZeroValues(true)
                .toString());

        // uzwględnij zera przy precyzji nanosekundowej i bazie lat
        assertEquals("0y 0d 2h 0m 0s 800ms 0us 0ns", hourAndMs
                .format()
                .base(Unit.YEARS)
                .precision(Unit.NANOSECONDS)
                .includeZeroValues(true)
                .toString());

    }

    @Test
    public void toStringBasePrecision() {

        // podstawa: auto, precyzja: auto
        assertEquals("3y 100d 14h 44m 59s 333ms", full
                .format()
                .toString());

        // podstawa: dni, precyzja: auto
        assertEquals("1195d 14h 44m 59s 333ms", full
                .format()
                .base(Unit.DAYS)
                .toString());

        // podstawa: godziny, precyzja: auto
        assertEquals("3509h 44m 59s 333ms", full
                .format()
                .base(Unit.HOURS)
                .toString());

        // podstawa: minuty, precyzja: sekundy
        assertEquals("4379m 59s", full
                .format()
                .base(Unit.MINUTES)
                .precision(Unit.SECONDS)
                .toString());

        // podstawa: minuty, precyzja: auto
        assertEquals("66308ms", full
                .format()
                .base(Unit.MILLISECONDS)
                .toString());

        // podstawa: mikrosekundy, precyzja: auto
        assertEquals("398975us", full
                .format()
                .base(Unit.MICROSECONDS)
                .toString());

        // podstawa: nanosekundy, precyzja: auto
        assertEquals("398975ns", full
                .format()
                .base(Unit.NANOSECONDS)
                .toString());

    }

    /**
     * Zamienione są miesjcami podstawa i precyzja. Metoda powinna to skorygować
     * i dac poprawny wynik
     */
    @Test
    public void toStringSwap() {

        // podstawa: dni, precyzja: auto
        assertEquals("1195d 14h 44m 59s 333ms", full
                .format()
                .base(Unit.MILLISECONDS)
                .precision(Unit.DAYS)
                .toString());

    }

}
