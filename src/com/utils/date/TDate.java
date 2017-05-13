package com.utils.date;

import com.json.Escape;
import com.lang.LUtil;
import com.utils.collections.Strings;
import com.utils.date.time.Interval;
import com.utils.date.time.Unit;
import java.text.*;
import java.util.*;

public class TDate extends Date {

    public final static String FULL = "yyyy-MM-dd HH:mm:ss";
    public final static String FULL_MS = "yyyy-MM-dd HH:mm:ss.SSS";
    public final static String DATE = "yyyy-MM-dd";
    public final static String TIME = "HH:mm:ss";
    public final static String TIME_MS = "HH:mm:ss.SSS";
    public final static String DATE_NAMED = "dd MMMM yyyy"; // 02 lipca 2015

    private String defaultFormat = FULL_MS;

    private final static Strings parsePatterns = new Strings()
            .add("yyyy-MM-dd HH:mm:ss.SSS")
            .add("yyyy/MM/dd HH:mm:ss.SSS")
            .add("yyyy-MM-dd HH:mm:ss")
            .add("yyyy/MM/dd HH:mm:ss")
            .add("yyyy-MM-dd")
            .add("yyyy/MM/dd")
            .add("yyMMdd");

    /*
     G Era designator Text AD
     y Year Year 1996; 96
     Y Week year Year 2009; 09
     M Month in year Month July; Jul; 07
     w Week in year Number 27
     W Week in month Number 2
     D Day in year Number 189
     d Day in month Number 10
     F Day of week in month Number 2
     E Day name in week Text Tuesday; Tue
     u Day number of week (1 = Monday, ..., 7 = Sunday) Number 1
     a Am/pm marker Text PM
     H Hour in day (0-23) Number 0
     k Hour in day (1-24) Number 24
     K Hour in am/pm (0-11) Number 0
     h Hour in am/pm (1-12) Number 12
     m Minute in hour Number 30
     s Second in minute Number 55
     S Millisecond Number 978
     z Time zone General time zone Pacific Standard Time; PST; GMT-08:00
     Z Time zone RFC 822 time zone -0800
     X Time zone ISO 8601 time zone -08; -0800; -08:00

     Examples:

     "yyyy.MM.dd G 'at' HH:mm:ss z" 2001.07.04 AD at 12:08:56 PDT
     "EEE, MMM d, ''yy" Wed, Jul 4, '01
     "h:mm a" 12:08 PM
     "hh 'o''clock' a, zzzz" 12 o'clock PM, Pacific Daylight Time
     "K:mm a, z" 0:08 PM, PDT
     "yyyyy.MMMMM.dd GGG hh:mm aaa" 02001.July.04 AD 12:08 PM
     "EEE, d MMM yyyy HH:mm:ss Z" Wed, 4 Jul 2001 12:08:56 -0700
     "yyMMddHHmmssZ" 010704120856-0700
     "yyyy-MM-dd'T'HH:mm:ss.SSS'Z'" 2001-07-04T12:08:56.235-0700
     "yyyy-MM-dd'T'HH:mm:ss.SSSXXX" 2001-07-04T12:08:56.235-07:00
     "YYYY-'W'ww-u" 2001-W27-3
     */
    public TDate() {
        super();
    }

    public TDate(Date date) {
        super(date.getTime());
    }

    public TDate(long date) {
        super(date);
    }

    /**
     * Porównuje biężącą datę z limitem expireMs w milisekundach
     */
    public boolean isExpired(int expireMs) {
        return new Date().getTime() > getTime() + expireMs;
    }

    private static Date autoParse(String date, boolean forced) throws ParseException {
        if (date != null)
            for (String pattern : parsePatterns)
                try {
                    Date result = new SimpleDateFormat(pattern).parse(date);
                    return result;
                } catch (Exception e) {
                }
        if (forced)
            throw new ParseException("Nieprawidłwy format daty: " + Escape.escape(date), -1);
        return null;
    }

    public static TDate parse(String date, TDate def) {
        try {
            Date d = autoParse(date, false);
            return d != null ? new TDate(d) : def;
        } catch (ParseException ex) {
            return def;
        }
    }

    public TDate(String date) throws ParseException {
        super(autoParse(date, true).getTime());
    }

    private static long parse(String date, String mask) throws ParseException {
        try {
            return new SimpleDateFormat(mask).parse(date).getTime();
        } catch (ParseException e) {
            throw new ParseException(LUtil.DATE_PARSE_FORMAT_ERROR.toString(date, mask), 0);
        }
    }

    public static String format(TDate date, String format) {
        return date != null ? date.toString(format) : null;
    }

    public TDate(String date, String mask) throws ParseException {
        super(parse(date, mask));
    }

    public String toString(String format) {
        return new SimpleDateFormat(format).format(this);
    }

    public String toString(boolean includeMilliseconds) {
        return new SimpleDateFormat(includeMilliseconds
                ? FULL_MS : FULL).format(this);
    }

    public Interval diff(Date date) {
        return new Interval(getTime() - date.getTime());
    }

    public Interval diff(long date) {
        return new Interval(getTime() - date);
    }

    public TDate defaultFormat(String format) {
        defaultFormat = format;
        return this;
    }

    @Override
    public String toString() {
        return toString(defaultFormat);
    }

    public boolean isSameDay(Date other) {
        if (other == null)
            return false;

        Calendar cal1 = getCalendar();

        Calendar cal2 = Calendar.getInstance();
        cal2.setTime(other);

        return cal1.get(Calendar.YEAR) == cal2.get(Calendar.YEAR)
                && cal1.get(Calendar.DAY_OF_YEAR) == cal2.get(Calendar.DAY_OF_YEAR);
    }

    @Override
    public int getYear() {
        return getCalendar().get(Calendar.YEAR);
    }

    @Override
    public int getMonth() {
        return getCalendar().get(Calendar.MONTH) + 1;
    }

    @Override
    public int getDay() {
        return getCalendar().get(Calendar.DAY_OF_MONTH);
    }

    public int getWeekOfYear() {
        return getCalendar().get(Calendar.WEEK_OF_YEAR);
    }

    public int getWeekOfMonth() {
        return getCalendar().get(Calendar.WEEK_OF_MONTH);
    }

    public int getDayOfYear() {
        return getCalendar().get(Calendar.DAY_OF_YEAR);
    }

    public int getDayOfWeek() {
        return getCalendar().get(Calendar.DAY_OF_WEEK) - 1;
    }

    public int getHour() {
        return getCalendar().get(Calendar.HOUR_OF_DAY);
    }

    public int getMinute() {
        return getCalendar().get(Calendar.MINUTE);
    }

    public int getSecond() {
        return getCalendar().get(Calendar.SECOND);
    }

    public int getMillisecond() {
        return getCalendar().get(Calendar.MILLISECOND);
    }

    public final Calendar getCalendar() {
        Calendar cal = Calendar.getInstance();
        cal.setTime(this);
        return cal;
    }

    public final TDate addYears(int years) {
        return add(years, 0, 0, 0, 0, 0, 0, 0);
    }

    public final TDate addMonths(int months) {
        return add(0, months, 0, 0, 0, 0, 0, 0);
    }

    public final TDate addWeeks(int weeks) {
        return add(0, 0, weeks, 0, 0, 0, 0, 0);
    }

    public final TDate addDays(int days) {
        return add(0, 0, 0, days, 0, 0, 0, 0);
    }

    public final TDate addHours(int hours) {
        return add(0, 0, 0, 0, hours, 0, 0, 0);
    }

    public final TDate addMinutes(int minutes) {
        return add(0, 0, 0, 0, 0, minutes, 0, 0);
    }

    public final TDate addSeconds(int seconds) {
        return add(0, 0, 0, 0, 0, 0, seconds, 0);
    }

    public final TDate addMilliseconds(int milliseconds) {
        return add(0, 0, 0, 0, 0, 0, 0, milliseconds);
    }

    public final TDate add(int milliseconds) {
        return add(0, 0, 0, 0, 0, 0, 0, milliseconds);
    }

    public final TDate add(int seconds, int milliseconds) {
        return add(0, 0, 0, 0, 0, 0, seconds, milliseconds);
    }

    public final TDate add(int minutes, int seconds, int milliseconds) {
        return add(0, 0, 0, 0, 0, minutes, seconds, milliseconds);
    }

    public final TDate add(int hours, int minutes, int seconds, int milliseconds) {
        return add(0, 0, 0, 0, hours, minutes, seconds, milliseconds);
    }

    public final TDate add(int days, int hours, int minutes, int seconds, int milliseconds) {
        return add(0, 0, 0, days, hours, minutes, seconds, milliseconds);
    }

    public final TDate add(int weeks, int days, int hours, int minutes, int seconds, int milliseconds) {
        return add(0, 0, weeks, days, hours, minutes, seconds, milliseconds);
    }

    public final TDate add(int months, int weeks, int days, int hours, int minutes, int seconds, int milliseconds) {
        return add(0, months, weeks, days, hours, minutes, seconds, milliseconds);
    }

    public final TDate add(int years, int months, int weeks, int days, int hours, int minutes, int seconds, int milliseconds) {
        Calendar cal = getCalendar();
        cal.add(Calendar.MILLISECOND, milliseconds);
        cal.add(Calendar.SECOND, seconds);
        cal.add(Calendar.MINUTE, minutes);
        cal.add(Calendar.HOUR_OF_DAY, hours);
        cal.add(Calendar.DAY_OF_MONTH, days);
        cal.add(Calendar.WEEK_OF_YEAR, weeks);
        cal.add(Calendar.MONTH, months);
        cal.add(Calendar.YEAR, years);
        setTime(cal.getTime().getTime());
        return this;
    }

    public final TDate setMilliseconds(Integer milliseconds) {
        return set(0, 0, 0, 0, 0, 0, 0, milliseconds);
    }

    public final TDate set(Integer year, Integer month, Integer week, Integer day,
            Integer hour, Integer minute, Integer second, Integer millisecond) {
        Calendar cal = getCalendar();
        if (millisecond != null)
            cal.set(Calendar.MILLISECOND, millisecond);
        if (second != null)
            cal.set(Calendar.SECOND, second);
        if (minute != null)
            cal.set(Calendar.MINUTE, minute);
        if (hour != null)
            cal.set(Calendar.HOUR_OF_DAY, hour);
        if (day != null)
            cal.set(Calendar.DAY_OF_MONTH, day);
        if (week != null)
            cal.set(Calendar.WEEK_OF_YEAR, week);
        if (month != null)
            cal.set(Calendar.MONTH, month - 1);
        if (year != null)
            cal.set(Calendar.YEAR, year);
        setTime(cal.getTime().getTime());
        return this;
    }

    public final TDate clearMilliseconds() {
        return set(null, null, null, null, null, null, null, 0);
    }

    public final TDate clearTime() {
        return set(null, null, null, null, 0, 0, 0, 0);
    }

    public boolean between(Date from, Date to) {
        return after(from) && before(to);
    }

}
