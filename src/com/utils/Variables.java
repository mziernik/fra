package com.utils;

import java.util.LinkedHashMap;
import java.util.Map;

public class Variables {

    private final static Map<String, String> all = new LinkedHashMap<>();

    public static void put(String name, String value) {
        synchronized (all) {
            all.put(name, value);
        }
    }

    public static void remove(String name) {
        synchronized (all) {
            all.remove(name);
        }
    }

    public static String get(String name) {
        synchronized (all) {

            if (name.startsWith("cfg/")) {

            }

            if (name.startsWith("env/")) {

            }

            if (name.startsWith("prop/")) {

            }

            if (name.startsWith("lang/")) {

            }

            return all.get(name);
        }
    }

    public static void putAll(Map<String, String> vars) {
        synchronized (all) {
            all.putAll(vars);
        }
    }

    /**
     *
     * @param part
     * @param reserved znaki, które nie mogą wystąpić w nazwie zmiennej
     * @return
     */
    public static String processSystemEnvironments(String part, char... reserved) {
        StringBuilder result = new StringBuilder();

        StringBuilder envb = null;
        for (char c : part.toCharArray()) {

            if (reserved != null && reserved.length > 0 && envb != null)
                for (char d : reserved)
                    if (c == d) {
                        result.append('%').append(envb.toString());
                        envb = null;
                        break;
                    }

            if (c == '%') {
                if (envb != null) {
                    String env = envb.toString();
                    String val = System.getenv(env);
                    if (val != null)
                        result.append(val);
                    else
                        result.append('%').append(env).append('%');

                    envb = null;
                    continue;
                }
                envb = new StringBuilder();
                continue;
            }
            if (envb != null)
                envb.append(c);
            else
                result.append(c);
        }

        if (envb != null)
            result.append('%').append(envb.toString());

        return result.toString();
    }

}
