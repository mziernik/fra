package com.utils.date.time;

import com.lang.LInterval;
import com.lang.core.LString;

/**
 * @author Miłosz Ziernik
 * @date 07 grudnia 2015
 * @encoding UTF-8
 */
public enum Unit {

    YEARS(0, 0, "y", LInterval.YEARS),
    DAYS(1, 365, "d", LInterval.DAYS),
    HOURS(2, 24, "h", LInterval.HOURS),
    MINUTES(3, 60, "m", LInterval.MINUTES),
    SECONDS(4, 60, "s", LInterval.SECONDS),
    MILLISECONDS(5, 1000, "ms", LInterval.MILLISECONDS),
    MICROSECONDS(6, 1000, "us",LInterval.MICROSECONDS),
    NANOSECONDS(7, 1000, "ns", LInterval.NANOSECONDS);

    public double multipier(Unit destination) {
        double m = 1;

        if (order > destination.order)
            for (Unit u : values())
                if (u.multiper > 0
                        && u.order <= order
                        && u.order > destination.order)
                    m *= u.multiper;

        if (order < destination.order)
            for (Unit u : values())
                if (u.multiper > 0
                        && u.order > order
                        && u.order <= destination.order)
                    m /= u.multiper;

        return m;
    }

    public String getFullUnit(long value) {

        switch (this) {
            case NANOSECONDS:
                return value == 1 ? LInterval.NANOSECOND.toString() : LInterval.NANOSECONDS.toString() ;
            case MICROSECONDS:
                return value == 1 ? LInterval.MICROSECOND.toString() : LInterval.MICROSECONDS.toString() ;
            case MILLISECONDS:
                return value == 1 ? LInterval.MILLISECOND.toString() : LInterval.MILLISECONDS.toString() ;
            case SECONDS:
                return value == 1 ? LInterval.SECOND.toString() : LInterval.SECONDS.toString() ;
            case MINUTES:
                return value == 1 ? LInterval.MINUTE.toString() : LInterval.MINUTES.toString() ;
            case HOURS:
                return value == 1 ? LInterval.HOUR.toString() : LInterval.HOURS.toString() ;
            case DAYS:
                return value == 1 ? LInterval.DAY.toString() : LInterval.DAYS.toString() ;
            //   case week:
            //        return value == 1 ? LInterval.WEEK.toString() : LInterval.WEEKS.toString() ;
            case YEARS:
                return value == 1 ? LInterval.YEAR.toString() : LInterval.YEARS.toString() ;
        }
        return "";
    }
    
    // FixMe: stara wersja (prawdopodobnie do usunięcia)
//    public String getFullUnit(long value) {
//        switch (this) {
//            case NANOSECONDS:
//                return value == 1 ? "nanosekunda" : value > 0 && value < 5 ? "nanosekundy" : "nanosekund";
//            case MICROSECONDS:
//                return value == 1 ? "mikrosekunda" : value > 0 && value < 5 ? "mikrosekundy" : "mikrosekund";
//            case MILLISECONDS:
//                return value == 1 ? "milisekunda" : value > 0 && value < 5 ? "milisekundy" : "milisekund";
//            case SECONDS:
//                return value == 1 ? "sekunda" : value > 0 && value < 5 ? "sekundy" : "sekund";
//            case MINUTES:
//                return value == 1 ? "minuta" : value > 0 && value < 5 ? "minuty" : "minut";
//            case HOURS:
//                return value == 1 ? "godzina" : value > 0 && value < 5 ? "godziny" : "godzin";
//            case DAYS:
//                return value == 1 ? "dzień" : "dni";
//            //   case week:
//            //        return value == 1 ? "tydzień" : value < 5 ? "tygodnie" : "tygodni";
//            case YEARS:
//                return value == 1 ? "rok" : value < 5 ? "lata" : "lat";
//        }
//        return "";
//    }

    public Unit higher() {
        return Unit.get(order - 1);
    }

    public Unit lower() {
        return Unit.get(order + 1);
    }

    private Unit(int order, long multiper, String shortName, LString name) {
        this.order = order;
        this.multiper = multiper;
        this.shortName = shortName;
        this.name = name;
    }

    static Unit get(int order) {
        for (Unit unit : values())
            if (unit.order == order)
                return unit;

        return null;
    }

    public static Unit get(String shortName) {
        for (Unit unit : values())
            if (unit.shortName.equals(shortName))
                return unit;
        return null;
    }

    final int order;
    final long multiper;
    public final String shortName;
    public final LString name;

}
