/*
 */
package com.utils.date.time;

import java.text.ParseException;
import org.junit.Test;
import static org.junit.Assert.*;

/**
 *
 * @author user
 */
public class IntervalTest {

    @Test
    public void testIsSame_Interval() throws ParseException {

        assertEquals("0 milisekund", new Interval(0, Unit.MILLISECONDS).format().unitMode(UnitMode.FULL).toString());
        assertEquals("0 godzin", new Interval(0, Unit.HOURS).format().unitMode(UnitMode.FULL).toString());

        //     assertEquals("1s", new Interval(1, Unit.SECONDS).toString());
        //   assertEquals("1s 200ms", new Interval(1200, Unit.MILLISECONDS).toString());
        //     assertEquals("1m 200ms", new Interval(60200, Unit.MILLISECONDS).toString());
        //     assertEquals("1m 31s", new Interval(90700, Unit.MILLISECONDS).toString());
        //   assertEquals("123ms 457us", new Interval(123456789, Unit.NANOSECONDS).toString());
        assertEquals("10m 100ms", new Interval(600_100, Unit.MILLISECONDS).toString());

    }

}
