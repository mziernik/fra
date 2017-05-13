package com.utils.date.schedule;

import com.json.JSON;
import com.utils.collections.Pair;
import com.utils.date.TDate;
import com.utils.date.schedule.TimeSchedule.ScheduleGroup;
import com.utils.date.time.TTime;
import com.utils.date.time.Unit;
import java.text.ParseException;
import java.util.Arrays;
import java.util.Date;
import java.util.LinkedList;
import org.junit.After;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

import static org.junit.Assert.*;

public class TimeScheduleTest {

    public TimeScheduleTest() {
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
    public void testAddGroup() throws ParseException {
        TimeSchedule ts = new TimeSchedule(30 * 60, (int) new TTime().getTime(Unit.SECONDS));

        ScheduleGroup group = ts.addGroup();
        group.timeRange.add(1 * 60 * 60, 3 * 60 * 60); // miedzy godz 01:00:00 a 03:00:00
        group.timeRange.add(7 * 60 * 60, 8 * 60 * 60); // miedzy godz 07:00:00 a 08:00:00
        group.daysOfWeek.addAll(Arrays.asList(1, 3)); // poniedziałki i środy
        group.daysOfMonth.addAll(Arrays.asList(13, 20, 27, 28)); // dni miesiąca
        group.dateRange.add(new Pair<>(1471046400000l, null)); // data początkowa: 13 Aug 2016 00:00:00 GMT
        group.dateRange.add(new Pair<>(null, 1502582400000l)); // data początkowa: 13 Aug 2016 00:00:00 GMT

        LinkedList<Date> series = ts.getSeries(new TDate("2016-05-01 15:05:01"), 30);

        for (Date date : series)
            System.out.println(new TDate(date).toString("EEE, yyyy-MM-dd HH:mm:ss"));
    }

    @Test
    public void parseJsonTest() {
        TimeSchedule ts = TimeSchedule.fromJson(JSON.parse(
                "[true,1800,0,[{\"en\":true,\"dw\":[1,3],\"dm\":[13,20,27,28],\"dr\":[[1471046400000,null],[null,1502582400000]],\"tr\":[[3600,10800],[25200,28800]]}]]"
        ));

        assertTrue(ts.enabled);
        assertTrue(ts.interval == 1800);
        assertTrue(ts.referenceTime == 0);
        assertTrue(ts.groups.size() == 1);

        ScheduleGroup group = ts.groups.get(0);

        assertTrue(group.enabled);
        assertTrue(group.daysOfWeek.containsAll(Arrays.asList(1, 3)));
        assertTrue(group.daysOfMonth.containsAll(Arrays.asList(13, 20, 27, 28)));
        assertTrue(group.dateRange.size() == 2);
        assertTrue(group.timeRange.size() == 2);

        System.out.println(ts.toJson());
    }
}
