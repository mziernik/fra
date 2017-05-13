package com.context.unit_test;

import com.utils.console.TConsole;
import com.mlogger.Log;
import java.util.logging.Level;
import java.util.logging.Logger;
import org.junit.runner.Description;
import org.junit.runner.Result;
import org.junit.runner.notification.Failure;
import org.junit.runner.notification.RunListener;
import org.junit.runner.notification.RunNotifier;
import org.junit.runners.BlockJUnit4ClassRunner;
import org.junit.runners.model.InitializationError;

public class ServiceRunner extends BlockJUnit4ClassRunner {

    public ServiceRunner(Class cls) throws InitializationError {
        super(cls);
    }

    @Override
    public void run(RunNotifier notifier) {
        notifier.addListener(new JUnitExecutionListener());
        super.run(notifier);
    }

}

class JUnitExecutionListener extends RunListener {

    @Override
    public void testRunStarted(Description description) throws Exception {

    }

    @Override
    public void testRunFinished(Result result) throws Exception {

    }

    @Override
    public void testStarted(Description description) throws Exception {

    }

    @Override
    public void testFinished(Description description) throws Exception {

    }

    @Override
    public void testFailure(Failure failure) throws Exception {
        TConsole.printErr(failure.getException());
        Log.error("UnitTest", failure.getException());
        Thread.sleep(10);
    }

    @Override
    public void testAssumptionFailure(Failure failure) {
        TConsole.printErr(failure.getException());
        Log.error("UnitTest", failure.getException());
        try {
            Thread.sleep(10);
        } catch (InterruptedException ex) {
            Logger.getLogger(JUnitExecutionListener.class.getName()).log(Level.SEVERE, null, ex);
        }
    }

    @Override
    public void testIgnored(Description description) throws Exception {

    }
}
