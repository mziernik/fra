package com.utils;

import com.lang.LUtil;
import com.mlogger.Log;
import com.mlogger.LogKind;
import com.utils.date.time.Interval;
import com.utils.date.time.Unit;

/**
 * Wywołanie metody run powoduje oczekiwanie na zwrócenie statusu true przez
 * metodę checkState() Jeśli status nie zostanie zwrócony w czasie @timeout,
 * zostanie wywołana metoda onTimeout(), która może zwrócić wyjątek <E> lub nie
 * zrobić nic.
 *
 * Co okreslony czas (@messageInterval) wywoływana jest metoda onMessage(), w
 * której można wyświetlić informację o oczekiwaniu
 *
 *
 * @author milosz
 * @timeout limit czasu oczekiwania wyrazony [ms]
 * @interval co jaki czas weryfikowany będzie status [ms], domyslnie 1 ms
 * @param <E> Klasa wyjątku
 */
public class WaitFor {

    protected int interval = 1_000;
    protected int timeout = 30_000;
    protected int messageInterval = 1_000;
    protected final long start = System.currentTimeMillis();
    private long lastMsgTS = 0;
    private final Object notifyObject = new Object();
    protected String actionName;
    private String[] tags;

    public static class WaitForTiemoutException extends RuntimeException {

    }

    public static class WaitForTimeoutException extends RuntimeException {

        public WaitForTimeoutException(String message) {
            super(message);
        }

    }

    @FunctionalInterface
    public static interface WaitForChecker {

        boolean checkState();
    }

    public WaitFor(String actionName, Interval timeout, WaitForChecker intf) {
        this(actionName, timeout, null, intf);
    }

    public WaitFor(String actionName, WaitForChecker intf) {
        this(actionName, null, null, intf);
    }

    public WaitFor(String actionName, Interval timeout, Interval interval, WaitForChecker intf) {
        this.actionName = actionName;
        timeout(timeout);
        interval(interval);

        boolean state = intf.checkState();

        while (!state) {
            try {
                if (notifyObject != null)
                    synchronized (notifyObject) {
                        notifyObject.wait(this.interval);
                    }
                else
                    Thread.sleep(this.interval);
            } catch (InterruptedException e) {
                onTimeout();
                return;
            }
            long now = System.currentTimeMillis();

            if (now > start + this.timeout) {
                onTimeout();
                return;
            }

            if (now - lastMsgTS > messageInterval) {
                lastMsgTS = now;
                onMessage();
            }

            state = intf.checkState();
        }
    }

    protected void onTimeout() {
        throw new WaitForTimeoutException(
                actionName + ", " + LUtil.WAITING_TIMEOUT.toString());
    }

    protected void onMessage() {
        Log log = new Log(LogKind.DEBUG)
                .tag("WaitFor")
                .tag(tags)
                .value(actionName + ", oczekiwanie")
                .comment("Pozostało " + getRemainingTime() + "s");
        log.send();
    }

    public int getRemainingTime() {
        return (int) (((start + timeout) - System.currentTimeMillis()) / 1000);
    }

    protected Log log(String message, String... tags) {
        Log log = new Log(LogKind.DEBUG)
                .tag("WaitFor")
                .tag(tags)
                .value(message)
                .comment("Pozostało " + getRemainingTime() + "s");
        log.send();
        return log;
    }

    public WaitFor timeout(Interval timeout) {
        if (timeout != null)
            this.timeout = (int) timeout.getTime(Unit.MILLISECONDS);
        return this;
    }

    public WaitFor timeout(String... tags) {
        this.tags = tags;
        return this;
    }

    public WaitFor interval(Interval interval) {
        if (interval != null)
            this.interval = (int) interval.getTime(Unit.MILLISECONDS);
        return this;
    }

    public WaitFor messageInterval(Interval messageInterval) {
        if (messageInterval != null)
            this.messageInterval = (int) messageInterval.getTime(Unit.MILLISECONDS);
        return this;
    }

}
/*

 new WaitFor<RuntimeException>() {

 @Override
 protected boolean checkState() {
 return isInitialized();
 }

 @Override
 protected void onTimeout() throws RuntimeException {
 throw new RuntimeException("Przekroczono limit czasu inicjalizacji WebService");
 }
 }.run(30000);

 */
