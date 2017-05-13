package com.thread;

import com.mlogger.Log;
import com.utils.Utils;
import com.utils.Is;
import java.lang.management.*;
import java.util.*;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicLong;

/**
 * Miłosz Ziernik 2013/11/01
 */
public abstract class MultiThread<T> {

    public static enum QueueExceedAction {

        skip, // pomin dodawany element
        shift, // usun najstarszy element
        wait // czekaj na zwolnienie kolejki
    }

    public abstract void execute(T item) throws Exception;
    private final int threadsCount;
    protected final LinkedList<ItemData> items = new LinkedList<>();
    public int maxQueueSize = 100000;
    public QueueExceedAction queueExceedAction = QueueExceedAction.wait;
    private boolean started = false;
    private boolean interruptOnDone;
    private boolean terminated = false;
    private final AtomicInteger processedCount = new AtomicInteger();
    private final AtomicInteger preProcessedCount = new AtomicInteger();
    private final LinkedList<Throwable> exceptions = new LinkedList<>();

    private final AtomicLong counter = new AtomicLong();

    public enum ItemStage {

        awainting,
        processing,
        queueing
    }

    class ItemData {

        final T item;
        Throwable exception;
        final long order;
        ItemStage stage = ItemStage.awainting;

        public ItemData(T item) {
            this.item = item;
            this.order = counter.incrementAndGet();
        }

    }

    /**
     * Zgłaszanie wyjątków
     *
     * @param e
     * @return Czy kontynuować
     */
    public boolean onException(Throwable e) {
        Log.warning(e);
        return false;
    }

    public LinkedList<Throwable> getExceptions() {
        synchronized (exceptions) {
            return Utils.asList(exceptions);
        }
    }

    /**
     * Metoda wywoływana po zakończeniu przetwarzania danego elementu w
     * kolejności, w której zostały dodane
     *
     * @param item
     * @param ex
     */
    public abstract void postExecuteOrder(T item, long order, Throwable exception);

    /**
     * +
     * Ustaw flagę zakończenia wątków i czekaj aż zakończy się przetwarzanie
     * listy
     *
     * @param timout
     * @throws InterruptedException
     */
    public void waitFor(int timout) throws InterruptedException {
        interruptOnDone = true;

        start(); // na wypadek gdyby nie dodano nic do listy

        synchronized (items) {
            items.notifyAll();
        }

        try {
            for (SingleThread th : threads)
                if (timout > 0)
                    th.join(timout);
                else
                    th.join();
        } finally {
            terminated = true;
        }
    }

    public void waitFor() throws InterruptedException {
        waitFor(0);
    }

    private final List<SingleThread> threads = new LinkedList<>();

    public MultiThread<T> add(T item) {
        synchronized (items) {
            while (maxQueueSize > 0 && items.size() >= maxQueueSize)
                switch (queueExceedAction) {
                    case skip:
                        return this;
                    case shift:
                        items.pollFirst();
                        break;
                    case wait:
                        try {
                            items.wait();
                        } catch (InterruptedException ex) {
                            return this;
                        }
                        break;

                }
            ItemData data = new ItemData(item);
            items.add(data);
            items.notifyAll();
        }
        start();
        return this;
    }

    public MultiThread<T> addAll(Collection<T> items) {
        synchronized (this.items) {
            for (T item : items)
                this.items.add(new ItemData(item));
            this.items.notifyAll();
            start();
        }
        return this;
    }

    public MultiThread(String name) {
        this(name, null, null);
    }

    public MultiThread(String name, Integer threadsCount, Integer priority) {
        if (threadsCount == null || threadsCount <= 0)
            threadsCount = ManagementFactory.getPlatformMXBean(
                    OperatingSystemMXBean.class).getAvailableProcessors();

        this.threadsCount = threadsCount;
        for (int i = 0; i < threadsCount; i++) {
            SingleThread th = new SingleThread(name + " [" + i + "]");
            if (priority != null)
                th.setPriority(priority);
            threads.add(th);
        }

    }

    public void start() {
        if (started)
            return;
        started = true;
        synchronized (threads) {
            for (SingleThread th : threads)
                th.start();
        }
    }

    public void stop(int wait) {
        for (SingleThread th : threads)
            th.interrupt();

        if (wait > 0)
            synchronized (threads) {
                try {
                    for (SingleThread th : threads)
                        th.join(wait);
                } catch (InterruptedException ex) {
                }
            }

    }

    public int getThreadsCount() {
        return threads.size();
    }

    public LinkedList<T> getCurrentlyProcessed() {
        LinkedList<T> currItems = new LinkedList<>();
        for (SingleThread th : threads)
            if (th.currentItem != null)
                currItems.add(th.currentItem.item);
        return currItems;
    }

    public int getQueueSize() {
        return items.size();
    }

    public int getProcessedCount(boolean includeCurrent) {
        if (includeCurrent)
            synchronized (preProcessedCount) {
                return preProcessedCount.get();
            }
        else
            synchronized (processedCount) {
                return processedCount.get();
            }

    }

    //--------------------------------------------------------------------------
    public class SingleThread extends Thread {

        ItemData currentItem;

        public SingleThread(String name) {
            super(name);
        }

        @Override
        public void run() {
            try {
                try {
                    while (!isInterrupted()) {
                        ItemData data;
                        synchronized (items) {
                            data = items.pollFirst();
                        }

                        if (data == null)
                            synchronized (items) {
                                if (interruptOnDone)
                                    return;
                                items.wait();
                                continue;
                            }

                        Throwable err = null;
                        if (!isInterrupted())
                            try {
                                currentItem = data;
                                currentItem.stage = ItemStage.processing;
                                synchronized (preProcessedCount) {
                                    preProcessedCount.incrementAndGet();
                                }
                                MultiThread.this.execute(data.item);
                            } catch (InterruptedException ex) {
                                return;
                            } catch (Throwable ex) {
                                err = ex;
                                synchronized (exceptions) {
                                    exceptions.add(ex);
                                }
                                if (!onException(ex))
                                    MultiThread.this.stop(0);
                            } finally {
                                synchronized (processedCount) {
                                    processedCount.incrementAndGet();
                                }
                                try {
                                    processPostExec(data, err);
                                } catch (Throwable e) {
                                    synchronized (exceptions) {
                                        exceptions.add(e);
                                    }
                                    if (!onException(e))
                                        MultiThread.this.stop(0);
                                }
                                currentItem = null;
                            }

                        synchronized (items) {
                            items.notifyAll();
                        }

                    }
                } catch (InterruptedException ex) {
                    if (interruptOnDone)
                        return;
                }
            } finally {
                interrupt();
            }
        }

    }

    private long expectedItemOrderNumber = 1;
    private final HashMap<Long, ItemData> processed = new HashMap<>();

    void processPostExec(ItemData current, Throwable err) {

        synchronized (processed) {
            processed.put(current.order, current);
        }

        current.exception = err;
        current.stage = ItemStage.processing;

        while (true) {
            ItemData item;

            synchronized (processed) {
                item = processed.get(expectedItemOrderNumber);
                if (item != null) {
                    processed.remove(expectedItemOrderNumber);
                    ++expectedItemOrderNumber;
                }
            }

            if (item == null)
                return;

            postExecuteOrder(item.item, item.order, item.exception);
        }
    }

}
