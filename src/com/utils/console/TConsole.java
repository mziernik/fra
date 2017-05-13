package com.utils.console;

import com.utils.Utils;
import com.utils.Is;
import com.utils.text.StrWriter;
import com.utils.console.VT100.CColor;
import com.context.AppContext;
import java.io.*;
import java.text.SimpleDateFormat;
import java.util.*;
import java.util.Map.Entry;

public class TConsole {

    static {
        System.setOut(new TConsoleWriter(System.out));
        System.setErr(new TConsoleWriter(System.err));
    }

    private boolean terminalMode = AppContext.devMode; // tryb VT100
    private final StringBuilder writer = new StringBuilder();
    private final VT100 vt100 = new VT100(writer);
    private static String lineBreak = "\n";

    private final PrintStream dst;

    public TConsole(PrintStream dst) {
        this.dst = dst;
    }

    public TConsole() {
        this.dst = System.out;
    }

    public TConsole terminalMode(boolean terminalMode) {
        this.terminalMode = terminalMode;
        return this;
    }

    public boolean isTerminalMode() {
        return terminalMode;
    }

    public TConsole append(Object value) {
        return append(value, false);
    }

    public TConsole appendLn(Object value) {
        return append(value, true);
    }

    public TConsole append(Object value, boolean lineBreak) {

        if (value instanceof Number)
            value = Utils.formatValue((Number) value);

        String text = Utils.toString(value);

        Appendable wr = terminalMode ? vt100 : writer;

        try {
            wr.append(text);
            if (lineBreak)
                wr.append(TConsole.lineBreak);
        } catch (IOException ex) {
        }

        return this;
    }

    @Override
    public String toString() {
        return writer.toString();
    }

    public TConsole foreground(CColor color) {
        vt100.attr(color.getForeground());
        return this;
    }

    public TConsole background(CColor color) {
        vt100.attr(color.getForeground());
        return this;
    }

    // czyszczenie atrybutów
    public TConsole reset() {
        vt100.attrReset();
        return this;
    }

    public TConsole bright() {
        vt100.bright();
        return this;
    }

    public TConsole dim() {
        vt100.dim();
        return this;
    }

    public static void write(Object value) {
        new TConsole().append(value, false).flush();
    }

    public static void print(Object value) {
        new TConsole().append(value, true).flush();
    }

    public static void printF(Object value) {
        new TConsole().append(value, true).flush();
    }

    public static void printErr(Throwable ex) {
        StringWriter sw = new StringWriter();
        //FixMe: Przerobić tak, aby nie przycinało, np:    
//        at java.lang.ClassLoader.loadClass(ClassLoader.java:357)
//        ... 20 more

        try (PrintWriter pw = new PrintWriter(sw)) {
            ex.printStackTrace(pw);

            new TConsole(System.err)
                    .foreground(CColor.blue)
                    .append("\n")
                    .append(new SimpleDateFormat("HH:mm:ss.SSS").format(new Date()))
                    .reset()
                    .append(" ")
                    .appendLn(sw.toString())
                    .flush();
        }
    }

    public static void printErr(String ex) {
        new TConsole(System.err)
                .foreground(CColor.blue)
                .append(new SimpleDateFormat("HH:mm:ss.SSS").format(new Date()))
                .reset()
                .append(" ")
                .appendLn(ex)
                .flush();
    }

    public TConsole flush() {
        syncPrint(dst, writer.toString());
        writer.setLength(0);
        return this;
    }

    private synchronized static void syncPrint(PrintStream ps, String s) {
        ps.append(s);
    }

    public static void printTs(Object value, Object... args) {
        new TConsole()
                .foreground(CColor.blue)
                .append(new SimpleDateFormat("HH:mm:ss.SSS").format(new Date()))
                .reset()
                .append(" ")
                .append(String.format(Utils.toString(value), args), true)
                .flush();
    }

    public static void printParams(String title, String attributes) {
        if (attributes == null)
            return;
        LinkedHashMap<String, String> map = new LinkedHashMap<>();
        for (String s : attributes.split("\\n")) {
            String name = s;
            if (name.contains(":")) {
                name = name.substring(0, name.indexOf(":"));
                s = s.substring(name.length() + 1);
            }
            map.put(name, s);
        }
        TConsole.printParams("", "", title, map);
    }

    public static void printParams(String title, Map<String, String> entries) {
        TConsole.printParams("", "", title, entries);
    }

    public static void printParams(String prefix, String sufix, String title,
            Map<String, String> entries) {

        int keyLen = 0;
        int valLen = 0;
        for (Entry<String, String> en : entries.entrySet()) {
            String key = Utils.coalesce(en.getKey(), "").trim();
            String val = Utils.coalesce(en.getValue(), "").trim();
            if (keyLen < key.length())
                keyLen = key.length();
            if (valLen < val.length())
                valLen = val.length();
        }

        int total = keyLen + valLen + 3 + prefix.length() + sufix.length();

        int lblLen = title.length();

        int span = total / 2 - lblLen / 2 - 1;

        StrWriter sb = new StrWriter();

        for (int i = 0; i < span; i++)
            sb.append("-");

        sb.append(" ").append(title).append(" ");

        for (int i = 0; i < span; i++)
            sb.append("-");

        sb.append("\n");

        for (Entry<String, String> en : entries.entrySet()) {
            String key = Utils.coalesce(en.getKey(), "").trim();
            String val = Utils.coalesce(en.getValue(), "").trim();

            sb.append(prefix).append(key);

            //  .append(key);
            for (int j = 0; j < keyLen - key.length(); j++)
                sb.append(" ");

            sb.append(" : ");
            sb.append(val);

            for (int j = 0; j < valLen - val.length(); j++)
                sb.append(" ");

            sb.append(sufix);
            sb.append("\n");
        }

        for (int i = 0; i < total; i++)
            sb.append("-");

        sb.append("\n");

        new TConsole().append(sb.toString()).flush();
    }

}

class TConsoleWriter extends PrintStream {

    public TConsoleWriter(PrintStream out) {
        super(out);

    }

    @Override
    public void println(String x) {
        super.println(x);
    }

    @Override
    public void print(String s) {
        super.print(s);
    }

}
