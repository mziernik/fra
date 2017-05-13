package com.mlogger.handlers;

import com.context.AppContext;
import com.utils.console.TConsole;
import java.io.PrintStream;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.LinkedList;
import java.util.logging.Handler;
import java.util.logging.LogRecord;
import com.mlogger.Log;
import com.mlogger.LogElement;
import com.mlogger.LogKind;
import com.utils.Utils;
import com.utils.Is;
import com.utils.collections.Strings;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.util.*;

/**
 * Miłosz Ziernik 2014/06/13
 */
public class ConsoleHandler extends LogHandler {

    public boolean colorsEnabled = true;
    public boolean printDetails = false;
    public final List<LogKind> excludes = new LinkedList<>();

    public ConsoleHandler() {
        excludes.add(LogKind.QUERY);
    }

    @Override
    public void publish(LogElement le, LinkedList<Handler> handlers, LogRecord record) {

        if (!(le instanceof Log))
            return;

        Log log = (Log) le;

        if (excludes.contains(log.kind.value()))
            return;

        int col = -1;
        StringWriter sw = new StringWriter();

        if (!log.color.isEmpty()) {
            String c = log.color.value();

            col = c.equals("black") ? 0
                    : c.equals("red") ? 1
                    : c.equals("green") ? 2
                    : c.equals("yellow") ? 3
                    : c.equals("blue") ? 4
                    : c.equals("magenta") ? 5
                    : c.equals("cyan") ? 6
                    : c.equals("white") ? 7
                    : -1;
        }

        if (col == -1 && log.kind.value() == LogKind.ERROR)
            col = 1;

        if (col == -1 && log.kind.value() == LogKind.WARNING)
            col = 5;

        Throwable exception = log.getException();

//        if (log.kind == LogKind.error)
//            ps = System.err;
        // jesli poprzedni log zakonczony byl enterem to nie drukuj czasu
        String value = Utils.toString(log.value.value().value);
        if (value == null)
            value = "";

        if (value.startsWith("\n")) {
            sw.append("\n");
            value = value.substring(1);
        }

        printTime(sw, false);

        if (colorsEnabled && col >= 0)
            sw.append("\033[").append(Integer.toString(30 + col)).append("m");

        if (!log.tag.isEmpty())
            sw.append("[" + new Strings().addAll(log.tag).toString(", ")).append("]: ");

        sw.append(value);

//
//        if (printDetails && !log.details.isEmpty())
//            ps.append("\n").append(Utils.toString(log.details.value().value));
        Strings stack = new Strings();
        List<String> lstStack = new LinkedList<>(log.errorStack.value()).peekLast();

        if (lstStack != null)
            for (String s : lstStack) {
                if (s.startsWith("*") || s.startsWith("+"))
                    stack.add(s.substring(1).replace(" (", "("));
                if (stack.size() > 8)
                    break;
            }

        if (!stack.isEmpty())
            sw.append("\n").append(stack.prefix("\t ").toString("\n"));

        if (colorsEnabled && col >= 0)
            sw.append("\033[0m");

        if (exception != null && AppContext.devMode) {
            sw.append("\n");
            exception.printStackTrace(new PrintWriter(sw));
        }

        TConsole.print(sw.toString());

    }

    private void printTime(StringWriter ps, boolean full) {
        String ms = new SimpleDateFormat("SSS").format(new Date());
        while (ms.length() < 3)
            ms = "0" + ms;
        if (colorsEnabled)
            ps.append("\033[").append(Integer.toString(30 + 4)).append("m");
        ps.append(new SimpleDateFormat((full ? "yyyy-MM-dd " : "") + "HH:mm:ss").format(new Date()));
        ps.append(".").append(ms);
        if (colorsEnabled)
            ps.append("\033[0m");
        ps.append(" ");
    }

    @Override
    public String toString() {
        return "system out";
    }

}
