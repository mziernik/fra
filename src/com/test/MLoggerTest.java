package com.test;

import com.servlet.handlers.TestClass;
import com.servlet.interfaces.ITestClass;
import com.mlogger.Log;

@ITestClass(name = "MLogger")
public class MLoggerTest extends TestClass {

    public void nestedException() {

        try {

            try {

                try {

                    throw new Exception("Błąd");
                } catch (Exception e) {
                    throw new Exception("Wyjątek 1", e);
                }

            } catch (Exception e) {
                throw new Exception("Wyjątrk 2", e);
            }

        } catch (Exception e) {
            Log.error(e);
        }
    }

}
