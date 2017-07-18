package com.thread;

import com.utils.collections.TList;
import java.util.*;

/**
 * Miłosz Ziernik 2013/11/01
 *
 * @param <T> Dowolny obiekt
 */
public abstract class QueueThread<T extends Object> extends TThread {

    private final TList<T> queue = new TList<>();
    /**
     * Minimalne opóźnienie wywołania metody processItem - działa jak frame limiter
     */
    protected int minDelay = 0; // minimalne opóżnienie (ms)
    protected int idleTime = 0;
    protected int maxIdleTime = 0;
    private long lastPeak = System.currentTimeMillis();
    private long lastIdle = System.currentTimeMillis();
    private T currentItem;

    /**
     * Metoda wywoływana w momencie gdy przez czas dłuższy niż %idleTime% nie
     * zostanie wywołana metoda add. %idleTime% musi być większy od 0. Metoda
     * wywoływana jest cyklicznie co %idleTime% milisekund
     */
    protected void onIdle() throws Exception {

    }

    /**
     * Ustawia maksymalny czas bezczynności, po którym zostanie wykonana metoda
     * onIndle
     *
     * @param idleTime
     * @return
     */
    public QueueThread setIdleTime(int idleTime) {
        this.idleTime = idleTime;
        return this;
    }

    /**
     * W połączeniu z setIdleTime() - definiuje maksymalny czas, przez jaki
     * zdarzenie onIdle() może być odraczane. Jeśli parametr ten nie jest
     * zdefiniowany, w przypadku częstych operacji dodawania elementów,
     * wykonanie metody onIdle() może być odraczane w czasie w nieskończoność
     *
     * @param maxIdleTime
     * @return
     */
    public QueueThread setMaxIdleTime(int maxIdleTime) {
        this.maxIdleTime = maxIdleTime;
        return this;
    }

    public QueueThread(final String name) {
        super(name);
    }

    /**
     * Dodaj element do kolejki
     *
     * @param item
     */
    public void add(T item) {
        synchronized (this) {
            queue.add(item);
            lastPeak = new Date().getTime();
            notify();
        }
        start();
    }

    /**
     * Dodaj element na początek kolejki
     *
     * @param item
     */
    public void insert(T item) {
        synchronized (this) {
            queue.add(0, item);
            lastPeak = new Date().getTime();
            notify();
        }
        start();
    }

    /**
     * Dodaj elementy do kolejki
     *
     * @param items
     */
    public void addAll(Collection<T> items) {
        synchronized (this) {
            queue.addAll(items);
            lastPeak = new Date().getTime();
            notify();
        }
        start();
    }

    public boolean remove(T item) {
        synchronized (this) {
            return queue.remove(item);
        }
    }

    public boolean removeAll(Collection<T> items) {
        synchronized (this) {
            return queue.removeAll(items);
        }
    }

    /**
     * @param items Lista elementów do przetworzenia. Jeśli dany element
     * zostanie usunięty z listy, to zostanie ponownie dodoany do kolejki
     * @throws Exception
     */
    protected abstract void processItem(final T item) throws Exception;

    protected void beforeRun() {
        // ------------------------- do przeciazenia ----------------------
    }

    public int getQueueSize() {
        return queue.size();
    }

    protected boolean canProcess(T item) {
        return true;
    }

    public TList<T> getQueue() {
        synchronized (this) {
            return new TList<>(queue);
        }
    }

    /**
     * Pobiera zawartość kolejki oraz ją czyści
     *
     * @param includeCurrent
     * @return
     */
    public TList<T> fetchQueue(boolean includeCurrent) {
        synchronized (this) {
            TList<T> result = new TList<>();
            if (includeCurrent)
                result._add(currentItem);
            result.addAll(queue);
            queue.clear();
            return result;
        }
    }

    @Override
    protected void run() throws Exception {

        beforeRun();

        long lastUpdate = System.currentTimeMillis();
        while (isRunning())
            try {

                if (queue.isEmpty())
                    synchronized (this) {
                        if (idleTime > 0)
                            this.wait(idleTime);
                        else
                            this.wait();
                    }

                long now = System.currentTimeMillis();

                if (idleTime > 0 && ((maxIdleTime > 0 && now - lastIdle > maxIdleTime)
                        || now - lastPeak > idleTime)) {
                    lastPeak = now;
                    lastIdle = now;
                    onIdle();
                }

                if (queue.isEmpty())
                    continue;

                if (minDelay > 0) {
                    long time = System.currentTimeMillis();

                    if (time - lastUpdate < minDelay)
                        try {
                            Thread.sleep(minDelay - (time - lastUpdate));
                        } catch (InterruptedException e) {
                            return;
                        }
                }

                synchronized (this) {
                    currentItem = queue.removeFirst();
                }

                if (isInterrupted())
                    break;

                if (!canProcess(currentItem)) {
                    synchronized (this) {
                        queue.add(0, currentItem);
                        currentItem = null;
                    }
                    Thread.sleep(1);
                }

                try {
                    processItem(currentItem);
                } finally {
                    currentItem = null;
                }
                lastUpdate = System.currentTimeMillis();
            } catch (InterruptedException e) {
                return;
            } catch (Throwable e) {
                onException(e);
            }
    }

}
