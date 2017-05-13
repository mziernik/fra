package com.context;

import com.utils.console.TConsole;
import com.utils.Utils;
import com.utils.Is;
import com.utils.Execute;
import java.lang.management.ManagementFactory;
import java.net.InetAddress;
import java.util.Properties;
import java.util.logging.Level;
import java.util.logging.Logger;

import static com.context.AppContextInitializer.startupInfo;

public class Environment {

    public final static String hostname;
    public final static String osName;
    public final static String country; //PL
    public final static String language; // pl
    public final static String userName;
    public final static String userHome;
    public final static String timeZone; // Europe/Belgrade
    public final static String javaVersion; //1.8.0_65
    public final static String command; //komenda inicjalizująca usługę. 
    //Dla tomcata jest to org.apache.catalina.startup.Bootstrap start.
    // Na jej podstawie można stwierdzić czy jest to serwlet czy aplikacja wbudowana

    public final static boolean isWindows;
    public final static boolean isLinux;
    public final static boolean isJava64bit;
    public final static boolean isCpu64bit;
    public final static String javaClassVersion;  // 52.0
    public static String temp;
    public final static boolean fxSupported;

    static {

        osName = Utils.coalesce(System.getProperty("os.name"), "");
        //   String command = System.getProperty("sun.java.command"); //service.Context
        javaClassVersion = Utils.coalesce(System.getProperty("java.class.version"), ""); // 52.0

        temp = Utils.coalesce(System.getProperty("java.io.tmpdir"), "");
        country = Utils.coalesce(System.getProperty("user.country"), ""); //PL
        language = Utils.coalesce(System.getProperty("user.language"), ""); // pl
        userName = Utils.coalesce(System.getProperty("user.name"), "");
        userHome = Utils.coalesce(System.getProperty("user.home"), "");
        timeZone = Utils.coalesce(System.getProperty("user.timezone"), ""); // Europe/Belgrade
        javaVersion = Utils.coalesce(System.getProperty("java.version"), ""); //1.8.0_65
        command = Utils.coalesce(System.getProperty("sun.java.command"), "");

        isWindows = osName != null && osName.toLowerCase().contains("windows");
        isLinux = osName != null && (osName.toLowerCase().contains("linux")
                || osName.toLowerCase().contains("unix"));
        isJava64bit = Utils.coalesce(System.getProperty("sun.arch.data.model"), "").contains("64");
        isCpu64bit = Utils.coalesce(System.getProperty("sun.cpu.isalist"), "").contains("64");

        boolean fx = false;
        try {
            fx = Class.forName("javafx.application.Application", false, ClassLoader.getSystemClassLoader()) != null;
        } catch (ClassNotFoundException ex) {
        }

        fxSupported = fx;

        String h = Utils.coalesce(System.getProperty("hostname"), "");
        if (Is.empty(h))
            try {
                h = InetAddress.getLocalHost().getHostName();
                // zdarza się, że w linuksie nie pobierze prawidłoo nazwy hosta
                if (Is.empty(h) || h.equalsIgnoreCase("localhost")) {

                    String name = ManagementFactory.getRuntimeMXBean().getName();
                    if (name.contains("@"))
                        h = name.substring(name.indexOf("@") + 1);

                    if (isLinux && (Is.empty(h) || h.equalsIgnoreCase("localhost")))
                        h = new Execute("hostname").run().buffer.trim();

                }
            } catch (Throwable e) {
                TConsole.printErr(e);
            }

        hostname = h;
    }

}
