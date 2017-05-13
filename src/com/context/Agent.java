package com.context;

import com.utils.console.TConsole;
import java.lang.instrument.Instrumentation;

public class Agent {

    static Instrumentation instrumentation;

    public static void premain(String args, Instrumentation instrumentation) {
        Agent.instrumentation = instrumentation;
        TConsole.print("Agent.instrumentation");

//        ClassLogger transformer = new ClassLogger();
//        instrumentation.addTransformer(transformer);
    }

}
