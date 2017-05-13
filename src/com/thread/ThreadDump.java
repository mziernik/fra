package com.thread;

import com.lang.core.Language;
import com.utils.date.TDate;
import java.util.Arrays;

public class ThreadDump {

    public final Thread currentThread;
    public final StackTraceElement[] stackTrace;
    public final String name;
    public final long id;
    public final ThreadDump parent;
    public final TDate date = new TDate();
    public final Language language;

    public ThreadDump(Thread thread) {
        this.currentThread = thread;
        name = thread.getName();
        id = thread.getId();
        StackTraceElement[] stack = thread.getStackTrace();

        stackTrace = Arrays.copyOfRange(stack, 2, stack.length);
        language = ThreadObject.language.get(thread);
        parent = ThreadObject.parentThread.get(thread);
    }

    public ThreadDump() {
        this(Thread.currentThread());
    }

}
