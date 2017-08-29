package com.utils;

import com.cron.TTimer;
import com.exceptions.EError;
import com.exceptions.ServiceException;
import com.intf.runnable.RunnableEx1;
import com.service.status.StatusGroup;
import com.service.status.StatusItem;
import com.utils.WaitForItem.ReadyException;
import com.utils.collections.Strings;
import com.utils.collections.TList;
import com.utils.date.time.Interval;
import com.utils.date.time.Unit;
import java.util.*;

public class Ready {

    public final static StatusGroup STATUS = StatusGroup.SERVICE.group("ready", "Gotowość", "Gotowość")
            .devOnly(true);
    public final static StatusGroup STS_CONF = STATUS.group("c", "Gotowe");
    public final static StatusGroup STS_AWT = STATUS.group("w", "Oczekujące");

    final static WaitForItemTimer timer = new WaitForItemTimer();

    final static List<WaitForItem> items = new LinkedList<>();
    public final static Map<Class<?>, Throwable> objects = new LinkedHashMap<>();

    public static boolean on(RunnableEx1<Throwable> runnable, Class<?>... objects) {
        return on(runnable, null, null, objects);
    }

    public static boolean waitFor(Class<?>... objects) throws InterruptedException {
        return waitFor(null, objects);
    }

    public static boolean waitFor(Interval timeout, Class<?>... objects) throws InterruptedException {
        final TObject<Throwable> notifier = new TObject();
        boolean result = on((Throwable err) -> {
            synchronized (notifier) {
                notifier.set(err);
                notifier.notify();
            }
        }, null, null, objects);

        if (!result)
            synchronized (notifier) {
                if (timeout == null) {
                    notifier.wait();
                    return false;
                }
                long t = timeout.getTime(Unit.MILLISECONDS);
                long ts = System.currentTimeMillis();
                notifier.wait(t);

                if (System.currentTimeMillis() >= ts + t)
                    throw new InterruptedException("Operation timeout");
            }

        Throwable err = notifier.get();
        if (err != null)
            throw new ReadyException(err);

        return result;
    }

    public static boolean on(RunnableEx1<Throwable> runnable, Interval timeout,
            Runnable onTimeout, Class<?>... objects) {

        if (objects == null || objects.length == 0)
            return false;

        WaitForItem item = new WaitForItem(timeout, onTimeout, runnable);

        synchronized (Ready.objects) {
            for (Class<?> o : objects)
                if (!Ready.objects.containsKey(Objects.requireNonNull(o)))
                    item.objects.add(o);
                else
                    item.error = Utils.coalesce(Ready.objects.get(o), item.error);
        }

        if (item.objects.isEmpty() || item.error != null)
            try {
                item.runnable.run(item.error);
                return true;
            } catch (RuntimeException | Error ex) {
                throw ex;
            } catch (Throwable ex) {
                throw new ServiceException(ex);
            }

        item.updateStatus();

        synchronized (items) {
            items.add(item);
        }

        return false;
    }

    static String objName(Class<?> object) {
        String name = Utils.cutLongName(Utils.toString(object), 50, false);
        String hash = Integer.toHexString(Objects.hashCode(object));
        if (!name.contains("@" + hash))
            name += "@" + hash;
        return name;
    }

    public static void confirmDepends(Class<?> object, Class<?>... dependencies) {
        if (dependencies == null || dependencies.length == 0) {
            confirm(object, null);
            return;
        }

        on((Throwable error) -> {
            confirm(object, error);
        }, dependencies);
    }

    public static void confirm(Class<?> object) {
        confirm(object, null);
    }

    public static void confirm(Class<?> object, Throwable e) {
        synchronized (Ready.objects) {
            Ready.objects.put(object, e);
        }

        EError error = e != null ? new EError(e) : null;

        STS_CONF.itemStr(Integer.toHexString(Objects.hashCode(object)), objName(object)
                + (error != null ? ", " + error.toString(true) : ""));

        synchronized (items) {
            for (WaitForItem item : new LinkedList<>(items)) {

                boolean removed = false;
                for (Class<?> o : new LinkedList<>(item.objects))
                    if (Objects.equals(o, object)) {
                        removed |= item.objects.remove(o);
                        item.error = e;
                    }

                if (removed)
                    item.updateStatus();

                if (item.objects.isEmpty() || item.error != null) {
                    items.remove(item);
                    STS_AWT.remove(Integer.toHexString(Objects.hashCode(item)));
                    try {
                        item.runnable.run(e);
                    } catch (RuntimeException | Error ex) {
                        throw ex;
                    } catch (Throwable ex) {
                        throw new ServiceException(ex);
                    }
                }
            }
        }

    }

}

class WaitForItemTimer extends TTimer {

    public WaitForItemTimer() {
        super(new Interval(100, Unit.MILLISECONDS));
    }

    @Override
    protected void run() throws Exception {
        if (Ready.items.isEmpty())
            return;

        long now = System.currentTimeMillis();
        synchronized (Ready.items) {
            for (WaitForItem item : new LinkedList<>(Ready.items))
                if (item.timeout != null
                        && now > item.ts + item.timeout.getTime(Unit.MILLISECONDS)) {
                    item.onTimeout.run();
                    Ready.items.remove(item);
                }
        }
    }

}

class WaitForItem {

    final TList<Class<?>> objects = new TList<>();
    final Interval timeout;
    final long ts = System.currentTimeMillis();
    final RunnableEx1<Throwable> runnable;
    final Runnable onTimeout;
    private StatusItem sts;
    Throwable error;

    public WaitForItem(Interval timeout, Runnable onTimeout, RunnableEx1<Throwable> runnable) {
        this.runnable = runnable;
        this.timeout = timeout;
        this.onTimeout = onTimeout;
    }

    void updateStatus() {
        Strings strings = new Strings();
        for (Class<?> obj : objects)
            strings.add(Ready.objName(obj));

        if (sts == null)
            sts = Ready.STS_AWT.itemStr(Integer.toHexString(Objects.hashCode(this)), Utils.toString(runnable));

        sts.comment(strings.toString(", "));
    }

    public static class ReadyException extends RuntimeException {

        public ReadyException(Throwable cause) {
            super(cause);
        }
    }

}
