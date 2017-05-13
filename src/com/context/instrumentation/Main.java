package com.context.instrumentation;

import com.utils.console.TConsole;
import java.lang.instrument.UnmodifiableClassException;

public class Main {

    public static void init() {

        try {
            ConstructorInstrumenter.instrumentClass(
                    Thread.class, (Thread t) -> {
                        TConsole.print("Instantiating a thread");

                    });
        } catch (UnmodifiableClassException e) {
            TConsole.print("Class cannot be modified");
        }

        AllocationRecorder.addSampler((int count, String desc, Object newObj, long size) -> {
            TConsole.print("I just allocated the object " + newObj
                    + " of type " + desc + " whose size is " + size);
            if (count != -1)
                TConsole.print("It's an array of size " + count);
        });

    }
}
