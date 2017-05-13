package com.utils.date;

import com.utils.console.TConsole;
import com.utils.Utils;
import com.utils.Is;
import com.utils.console.VT100.CColor;
import com.utils.date.time.Interval;
import com.utils.date.time.Unit;
import java.text.SimpleDateFormat;
import java.util.Date;

/**
 * Miłosz Ziernik 2013/11/05
 */
public final class Timestamp {

    private final long nanoTime = System.nanoTime();

    public final Interval diff() {
        return new Interval(System.nanoTime() - nanoTime, Unit.NANOSECONDS);
    }

    public final Interval diff(Timestamp previous) {
        return new Interval(nanoTime - previous.nanoTime, Unit.NANOSECONDS);
    }

    public final long getNanoTime() {
        return nanoTime;
    }

    public void console(Object value) {

        new TConsole()
                .foreground(CColor.blue)
                .append(new SimpleDateFormat("HH:mm:ss.SSS").format(new Date()))
                .reset()
                .append(" ")
                .append(value)
                .append(" ")
                .foreground(CColor.blue)
                .appendLn(diff().toString())
                .reset()
                .flush();
    }

    /*
     Czas wykonania danej metody
     */
    public void consoleDiffMethod(String text) {
        console(Utils.getCurrentMethodName(1) + (text != null && !text.isEmpty()
                ? ", " + text : ""));
    }

    public void consoleDiffMethod() {
        console(Utils.getCurrentMethodName(1));
    }
}
