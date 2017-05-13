package com.lang;

import com.lang.core.LString;
import com.lang.core.LangDict;
import com.lang.core.Languages;

@LangDict(name = "Interval")
public enum LInterval implements LString {
    YEAR("lata", "year"),
    YEARS("lat", "years"),
    MONTH("miesiąc", "month"),
    MONTHS("miesiący", "months"),
    WEEK("tydzień", "week"),
    WEEKS("tygodni", "weeks"),
    DAY("dzień", "day"),
    DAYS("dni", "days"),
    HOUR("godzina", "hour"),
    HOURS("godzin", "hours"),
    MINUTE("minuta", "minute"),
    MINUTES("minut", "minutes"),
    SECOND("sekunda", "second"),
    SECONDS("sekund", "seconds"),
    MILLISECOND("milisekunda", "millisecond"),
    MILLISECONDS("milisekund", "milliseconds"),
    MICROSECOND("mikrosekunda", "microsecond"),
    MICROSECONDS("mikrosekund", "microseconds"),
    NANOSECOND("nanosekunda", "nanosecond"),
    NANOSECONDS("nanosekund", "nanoseconds");

    // <editor-fold defaultstate="collapsed">
    private LInterval(String pl, String en) {
        Languages.addLstringEntry(this, Languages.en, en);
        Languages.addLstringEntry(this, Languages.pl, pl);
    }

    @Override
    public String toString() {
        return toString(new Object[0]);
    }
    // </editor-fold>
}
