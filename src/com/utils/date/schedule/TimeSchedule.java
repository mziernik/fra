package com.utils.date.schedule;

import com.exceptions.ServiceException;
import com.json.JArray;
import com.json.JElement;
import com.json.JObject;
import com.json.JValue;
import com.utils.collections.Pair;
import com.utils.collections.Pairs;
import com.utils.date.time.TTime;
import com.utils.date.time.Unit;
import java.util.Calendar;
import java.util.Date;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

public class TimeSchedule {

    public boolean enabled = true;
    public final List<ScheduleGroup> groups = new LinkedList<>();

    //-------- poniższe dane wymagane są w trybie cyklicznym (cron)--------
    public final Integer interval; // minimalny interwał 10s
    public final Integer referenceTime; // musi być zdefiniowana razem z interwałem (czas w sek)

    public TimeSchedule(Integer interval, Integer referenceTime) {
        this.interval = interval;
        this.referenceTime = referenceTime;
    }

    public class ScheduleGroup {

        // wartości czasu zapisywane jako Integer to całkowita liczba sekund 0..86400
        public boolean enabled = true;
        public final List<Integer> daysOfWeek = new LinkedList<>(); // 0 - poniedziałek
        public final List<Integer> daysOfMonth = new LinkedList<>();
        public final Pairs<Integer, Integer> timeRange = new Pairs<>();
        public final Pairs<Long, Long> dateRange = new Pairs<>();
        public Boolean holidays; // true: święto musi się zawierać w zakresie, false wyklucza, null bez znaczniea

        private ScheduleGroup() {

        }

        public boolean isInRange(Date date) {
            if (!enabled)
                return false;

            Calendar cal = Calendar.getInstance();
            cal.setTime(date);

            if (holidays != null) {
                //ToDo.....
            }

            if (!daysOfWeek.isEmpty()
                    && !daysOfWeek.contains(cal.get(Calendar.DAY_OF_WEEK) - 1))
                return false;

            if (!daysOfMonth.isEmpty()
                    && !daysOfMonth.contains(cal.get(Calendar.DAY_OF_MONTH)))
                return false;

            if (!dateRange.isEmpty()) {
                boolean has = false;

                long time = cal.getTimeInMillis();

                for (Pair<Long, Long> p : dateRange)
                    has |= (p.first != null && p.second != null && p.first <= time && p.second >= time)
                            || (p.first != null && p.second == null && p.first <= time)
                            || (p.first == null && p.second != null && p.second >= time);

                if (!has)
                    return false;

            }

            if (!timeRange.isEmpty()) {
                boolean has = false;

                int sec = cal.get(Calendar.HOUR_OF_DAY) * 60 * 60
                        + cal.get(Calendar.MINUTE) * 60
                        + cal.get(Calendar.SECOND);

                for (Pair<Integer, Integer> p : timeRange)
                    has |= (p.first != null && p.second != null && p.first <= sec && p.second >= sec)
                            || (p.first != null && p.second == null && p.first <= sec)
                            || (p.first == null && p.second != null && p.second >= sec);

                if (!has)
                    return false;

            }

            return true;
        }
    }

    public ScheduleGroup addGroup() {
        ScheduleGroup group = new ScheduleGroup();
        groups.add(group);
        return group;
    }

    public boolean isInRange(Date date) {
        if (!enabled)
            return false;

        // sprawdz czy pokrywa się z interwalem
        if (referenceTime != null
                && interval != null
                && interval > 0
                && (new TTime(date).getTime(Unit.SECONDS) - referenceTime) % interval != 0)
            return false;

        if (groups.isEmpty())
            return true;

        for (ScheduleGroup group : groups)
            if (group.isInRange(date))
                return true;

        return false;
    }

    public LinkedList<Date> getSeries(Date beginDate, int count) {
        LinkedList<Date> list = new LinkedList<>();

        if (enabled && referenceTime != null && interval != null) {

            long mod = beginDate.getTime() % (interval * 1000l);
            long time = beginDate.getTime();

            if (mod > 0)
                time += (interval * 1000l) - mod;

            int cnt = 0; // zabezpieczenie przed zapetleniem
            while (list.size() < count && cnt < 1_000_000) {
                Date date = new Date(time);
                if (isInRange(date))
                    list.add(date);

                time += interval * 1000;
                ++cnt;
            }

        }

        return list;
    }

    /* ZAPIS HARMONOGRAMU W JSON-ie
    [
      true,                                       // 0 - harmonogram aktywny
      15,                                         // 1 - interwał (w sekundach)
      0,                                          // 2 - czas referencyjny (w sekundach)
      [                                           // 3 - grupy
         {                                        // pierwsza grupa (wszystkie poniższe elementy są opcjonalne)
            "en": true,                           // grupa aktywna
            "dw": ["2", "3"],                 // dni tygodnia (poniedziałek -> 1;  niedziela -> 7)
            "dm": ["12", "13", "14"],             // dni miesiąca (tablica int-ów)
            "dr": [                               // tablica przedziałów dat
               [1465423200000, 1466805600000]     // przedział dat (tablica od do  ->  timestampy)
            ],
            "tr": [                               // tablica przedziałów czasu (godziny i minuty)
               [7200, 52200]                      // przedział czasu (tablica od do  ->  w sekundach)
            ]
         }
      ]
    ]
     */
    public static TimeSchedule fromJson(JElement json) {
        try {
            if (json.isArray()) {
                JArray jts = json.asArray();

                // Parametry harmonogramu
                int interval = jts.element(1).asValue().asNumber().intValue(); // Interwał
                int refTime = jts.element(2).asValue().asNumber().intValue(); // Czas referencyjny

                TimeSchedule ts = new TimeSchedule(interval, refTime); // TODO => zamienić drugi parametr na 'referenceTime' (Integer)
                ts.enabled = jts.element(0).asValue().asBoolean(); // Aktywny
                // Grupy
                for (JObject jgroup : jts.element(3, new JArray()).asArray().getObjects()) {
                    Map<String, JElement> items = jgroup.getItems();

                    ScheduleGroup group = ts.addGroup();
                    group.enabled = items.getOrDefault("en", new JValue(false)).asValue().asBoolean();

                    for (JElement dw : items.getOrDefault("dw", new JArray()).asArray())
                        group.daysOfWeek.add(dw.asValue().asNumber().intValue());

                    for (JElement dm : items.getOrDefault("dm", new JArray()).asArray())
                        group.daysOfMonth.add(dm.asValue().asNumber().intValue());

                    for (JElement dr : items.getOrDefault("dr", new JArray()).asArray()) {
                        JArray arr = dr.asArray();

                        JElement drFrom = arr.element(0);
                        JElement drTo = arr.element(1);

                        group.dateRange.add(new Pair<>(
                                drFrom.isNull() ? null : drFrom.asValue().asNumber().longValue(),
                                drTo.isNull() ? null : drTo.asValue().asNumber().longValue()
                        ));
                    }

                    for (JElement tr : items.getOrDefault("tr", new JArray()).asArray()) {
                        JArray arr = tr.asArray();

                        JElement trFrom = arr.element(0);
                        JElement trTo = arr.element(1);

                        group.timeRange.add(new Pair<>(
                                trFrom.isNull() ? null : trFrom.asValue().asNumber().intValue(),
                                trTo.isNull() ? null : trTo.asValue().asNumber().intValue()
                        ));
                    }
                }

                return ts;
            }
        } catch (Throwable ex) {
            throw new ServiceException("Błąd parsowania harmonogramu", ex);
        }

        return null;
    }

    public JElement toJson() {
        if (groups.isEmpty())
            return null;

        JArray jts = new JArray();
        jts.options.compactMode(true);

        JArray jgroups = new JArray();

        for (ScheduleGroup group : groups) {
            JObject jgr = new JObject();

            jgr.add("en", group.enabled);

            if (!group.daysOfWeek.isEmpty())
                jgr.add("dw", new JArray().addAll(group.daysOfWeek));

            if (!group.daysOfMonth.isEmpty())
                jgr.add("dm", new JArray().addAll(group.daysOfMonth));

            if (!group.dateRange.isEmpty()) {
                JArray dr = new JArray();

                for (Pair<Long, Long> dp : group.dateRange)
                    dr.add(new JArray().addAll(dp.first, dp.second));

                jgr.add("dr", dr);
            }

            if (!group.timeRange.isEmpty()) {
                JArray tr = new JArray();

                for (Pair<Integer, Integer> tp : group.timeRange)
                    tr.add(new JArray().addAll(tp.first, tp.second));

                jgr.add("tr", tr);
            }

            jgroups.add(jgr);
        }

        jts.addAll(
                enabled,
                interval,
                referenceTime,
                jgroups
        );

        return jts;
    }
}

/*
harmonogram

--------------------------------------------
data początkowa: 	2016-01-01 10:30:12
data końcowa: 		2017-01-01 00:00:00
dzien tygodnia: 	pn, sr, pt
dni swiateczne:		nie
dzien miesiaca:	 	1,2,3,4,5,6
godz poczatkowa:	08.00 
godzina koncowa:	16.00
--------------------------------------------
data początkowa: 	2016-01-01 10:30:12
data końcowa: 		2017-01-01 00:00:00
dzien tygodnia: 	pn, sr, pt
dni swiateczne:		Tak
dzien miesiaca:	 	10-31
godz poczatkowa:	16.00 
godzina koncowa:	20.15
--------------------------------------------
interwał:			30 min
 */
