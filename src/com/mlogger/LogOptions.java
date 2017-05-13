package com.mlogger;

import java.util.HashMap;
import java.util.LinkedHashSet;
import java.util.Map;
import java.util.Set;
import java.util.UUID;
import com.mlogger.interfaces.ILogEventsHandler;

public class LogOptions {

    public final Set<ILogEventsHandler> events = new LinkedHashSet<>();
    // elementy stosu wywołań zaczynające się od zawartych w liście zostaną zignorowane
    public final Set<String> knowLoggers = new LinkedHashSet<>();
    public UUID token;
    public boolean priority;
    public String encryptionKey; // opcjonalne szyfrowanie treści (tylko w połączeniu z tokenem)
    public boolean compressed;
    public int maxDatagramSize; // maksymalny rozmiar paczki UDP (wg specyfikacji 512, w przeczywistości 64kB - dane naglowkowe)
    final static Map<Thread, LogOptions> threadData = new HashMap<Thread, LogOptions>();
    public int minDelay = 3; // minimalne opoznienie wysylania logu (ms) (aby mozna bylo zmodyfikowac zawartosc

    public LogOptions(MLogger logger) {
        maxDatagramSize = 65200;
        knowLoggers.add("java.util.logging.");
        knowLoggers.add("sun.util.logging.");
        knowLoggers.add("org.apache.commons.logging.");
        knowLoggers.add("org.slf4j.");
        knowLoggers.add(MHandler.class.getPackage().getName() + ".");
        load(logger != null ? logger.options : MLogger.instance().options);
    }

    public LogOptions setCurrentThreadDefaults() {
        LogOptions opts = new LogOptions(null);
        synchronized (LogOptions.threadData) {
            LogOptions.threadData.put(Thread.currentThread(), opts);
        }
        return opts;
    }

    void load(LogOptions parent) {

        if (parent == null)
            return;

        this.token = parent.token;
        this.events.addAll(parent.events);
        this.knowLoggers.addAll(parent.knowLoggers);
        this.compressed = parent.compressed;
        this.encryptionKey = parent.encryptionKey;
        this.maxDatagramSize = parent.maxDatagramSize;
    }

    public void loadThreadDefaults() {
        MLogger.cleanupThreadsList();
        load(threadData.get(Thread.currentThread()));
    }

    public void addStackTraceTrimName(String value) {
        synchronized (knowLoggers) {
            knowLoggers.add(value);
        }
    }

    public void setCompressed(Boolean value) {
        compressed = value;
    }

    public void setToken(UUID token) {
        token = token;
    }

    public void setEncryptionKey(String key) {
        encryptionKey = key;
    }

    public void setMaxDatagramSize(int size) {
        maxDatagramSize = size;
    }

}
