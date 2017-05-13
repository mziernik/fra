package com.dev;

import com.utils.Path;
import com.utils.Is;
import com.context.AppContext;
import com.mlogger.Log;
import java.util.LinkedHashSet;
import java.util.Set;

/**
 * Wszystko co związane z trybem deweloperskim, debugowanie
 *
 * @author Miłosz Ziernik
 * @date 27 sierpnia 2015
 * @encoding UTF-8
 */
public class Dev {

    public final static String remote;
    public final static String config;
    public final static String basedir;
    public final static String srcDir;
    public final static String webDir;
    // alternatywne ścieżki źródeł (w przypadku, gdy mamy podlinkowane inne projekty)
    public final static Set<Path> sources = new LinkedHashSet<>();

    static {
        basedir = System.getProperty("basedir");
        remote = System.getProperty("nbremote");
        config = System.getProperty("nbconfig");
        srcDir = System.getProperty("src.dir");
        webDir = System.getProperty("web.dir");

        if (!Is.empty(basedir) && !Is.empty(srcDir))
            AppContext.sourcesPath.set(basedir, srcDir);

        if (!Is.empty(basedir) && !Is.empty(webDir)) {
            AppContext.webPath.set(basedir, webDir);
            if (AppContext.webPath.exists())
                AppContext.webPath.readOnly(true);
            else
                warning(AppContext.webPath + " nie istnieje");
        }
    }

    public static void debug(Object value) {
        if (AppContext.devMode)
            Log.debug(value);
    }

    public static void debug(String tag, Object value) {
        if (AppContext.devMode)
            Log.debug(tag, value);
    }

    public static void debug(String tag, Object value, Object details) {
        if (AppContext.devMode)
            Log.debug(tag, value, details);
    }

    public static void info(Object value) {
        if (AppContext.devMode)
            Log.info(value);
    }

    public static void info(String tag, Object value) {
        if (AppContext.devMode)
            Log.info(tag, value);
    }

    public static void info(String tag, Object value, Object details) {
        if (AppContext.devMode)
            Log.info(tag, value, details);
    }

    public static void warning(Object value) {
        if (AppContext.devMode)
            Log.warning(value);
    }

    public static void warning(String tag, Object value) {
        if (AppContext.devMode)
            Log.warning(tag, value);
    }

    public static void warning(String tag, Object value, Object details) {
        if (AppContext.devMode)
            Log.warning(tag, value, details);
    }

    public static void warning(Throwable ex) {
        if (AppContext.devMode)
            Log.warning(ex);
    }

    public static void warning(String tag, Throwable ex) {
        if (AppContext.devMode)
            Log.warning(tag, ex);
    }

    public static void error(Object value) {
        if (AppContext.devMode)
            Log.error(value);
    }

    public static void error(String tag, Object value) {
        if (AppContext.devMode)
            Log.error(tag, value);
    }

    public static void error(String tag, Object value, Object details) {
        if (AppContext.devMode)
            Log.error(tag, value, details);
    }

    public static void error(Throwable ex) {
        if (AppContext.devMode)
            Log.error(ex);
    }

    public static void error(String tag, Throwable ex) {
        if (AppContext.devMode)
            Log.error(tag, ex);
    }

    public static void error(String tag, Throwable ex, Object details) {
        if (AppContext.devMode)
            Log.error(tag, ex, details);
    }

    public static String getFileUrl(StackTraceElement el) {

        if (el == null || el.getFileName() == null || el.getLineNumber() <= 0)
            return null;
        String pckg = el.getClassName();
        if (pckg.contains("."))
            pckg = pckg.substring(0, pckg.lastIndexOf(".")) + ".";
        else
            pckg = "";
        return pckg.replace(".", "/") + el.getFileName() + ":" + el.getLineNumber();

    }

    public static String getFileUrl(Class cls) {
        return cls.getName().replace(".", "/") + ".java";

    }

}
