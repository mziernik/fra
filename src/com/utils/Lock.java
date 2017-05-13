package com.utils;

import com.exceptions.ThrowableException;
import com.intf.callable.CallableEx;
import com.intf.runnable.RunnableEx;
import java.util.concurrent.atomic.AtomicInteger;

public class Lock {

    private final AtomicInteger locks = new AtomicInteger(0);

    public Lock read(RunnableEx runnable) {
        try {
            while (locks.get() < 0)
                synchronized (locks) {
                    locks.wait();
                }

            synchronized (locks) {
                locks.incrementAndGet();
            }

            try {
                runnable.run();
            } finally {
                synchronized (locks) {
                    locks.decrementAndGet();
                }
            }
            return this;
        } catch (Throwable e) {
            throw new ThrowableException(e);
        }

    }

    public <T> T readR(CallableEx<T> runnable) {
        try {
            while (locks.get() < 0)
                synchronized (locks) {
                    locks.wait();
                }

            synchronized (locks) {
                locks.incrementAndGet();
            }

            try {
                return runnable.run();
            } finally {
                synchronized (locks) {
                    locks.decrementAndGet();
                }
            }
        } catch (Throwable e) {
            throw new ThrowableException(e);
        }

    }

    public Lock write(RunnableEx runnable) {
        try {
            while (locks.get() != 0)
                synchronized (locks) {
                    locks.wait();
                }

            synchronized (locks) {
                locks.decrementAndGet();
                try {
                    runnable.run();
                } finally {
                    locks.incrementAndGet();
                    locks.notifyAll();
                }
            }

            return this;
        } catch (Throwable e) {
            throw new ThrowableException(e);
        }
    }

}
