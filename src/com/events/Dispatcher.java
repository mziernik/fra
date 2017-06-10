package com.events;

import com.events.Dispatcher.ContextRunnable;
import com.intf.callable.CallableEx1;
import com.intf.callable.CallableEx2;
import com.intf.runnable.RunnableEx1;
import com.intf.runnable.RunnableEx2;
import com.intf.runnable.RunnableEx3;
import com.utils.collections.TList;
import com.webapi.core.WebApiRequest;
import java.util.Iterator;
import java.util.LinkedHashSet;
import java.util.Set;
import java.util.stream.Stream;

/**
 * Klasa ułatwiająca obsługę listenerów. Jako [Intf] deklarujemy dowolny
 * interfejs. Może być nim jeden z poniższych.
 *
 * @param <Intf>
 */
public class Dispatcher<Intf> implements Iterable<ContextRunnable<Intf>> {

    private final Set<ContextRunnable<Intf>> listeners = new LinkedHashSet<>();

    @Override
    public Iterator<ContextRunnable<Intf>> iterator() {
        throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
    }

//    @FunctionalInterface
//    public static interface DispatcherRunnable<Intf> {
//
//        void run(Object source, Object context, Intf runnable) throws Exception;
//    }
    public static class ContextRunnable<Intf> {

        public final Object context;
        public final Intf runnable;

        public ContextRunnable(Object context, Intf runnable) {
            this.context = context;
            this.runnable = runnable;
        }

    }

    public Stream<ContextRunnable<Intf>> stream() {
        Set<ContextRunnable<Intf>> list;
        synchronized (listeners) {
            list = new LinkedHashSet<>(listeners);
        }
        return list.stream();
    }

    public TList<Intf> getObservers() {
        TList<Intf> list = new TList<>();
        synchronized (listeners) {
            for (ContextRunnable<Intf> cr : listeners)
                list.add(cr.runnable);
        }
        return list;
    }

    public ContextRunnable<Intf> listen(Object context, Intf runnable) {
        if (runnable == null)
            return null;

        synchronized (listeners) {
            for (ContextRunnable<Intf> cr : listeners)
                if (cr.runnable == runnable && cr.context == context)
                    return cr;

            final ContextRunnable<Intf> cr = new ContextRunnable<>(context, runnable);
            listeners.add(cr);

            if (context instanceof WebApiRequest)
                ((WebApiRequest) context).webSocket.onClose.listen(context, reason -> {
                    synchronized (listeners) {
                        listeners.remove(cr);
                    }
                });
            return cr;
        }

    }

    /**
     * Wywołuje funkcje zwrotne wszystkich obserwatorów. Jeśli któryś z nich
     * zwróci false, cała pętla zostaje przerwana
     *
     * @param sender
     * @param callable
     * @return
     */
    public Boolean dispatchBreakCtx(Object sender, CallableEx2<Boolean, Object, Intf> callable) {
        TList<ContextRunnable<Intf>> list;
        synchronized (listeners) {
            list = new TList<>(listeners);
        }
        for (ContextRunnable<Intf> ctxRun : list)
            try {
                if (Boolean.FALSE.equals(callable.run(ctxRun.context, ctxRun.runnable)))
                    return false;
            } catch (RuntimeException | Error e) {
                throw e;
            } catch (Throwable e) {
                throw new RuntimeException(e);
            }

        return true;
    }

    public Boolean dispatchBreak(Object sender, CallableEx1<Boolean, Intf> callable) {
        TList<ContextRunnable<Intf>> list;
        synchronized (listeners) {
            list = new TList<>(listeners);
        }
        for (ContextRunnable<Intf> ctxRun : list)
            try {
                if (Boolean.FALSE.equals(callable.run(ctxRun.runnable)))
                    return false;
            } catch (RuntimeException | Error e) {
                throw e;
            } catch (Throwable e) {
                throw new RuntimeException(e);
            }

        return true;
    }

    public void dispatch(Object sender, RunnableEx1< Intf> callable) {
        TList<ContextRunnable<Intf>> list;
        synchronized (listeners) {
            list = new TList<>(listeners);
        }
        for (ContextRunnable<Intf> ctxRun : list)
            try {
                callable.run(ctxRun.runnable);
            } catch (RuntimeException | Error e) {
                throw e;
            } catch (Throwable e) {
                throw new RuntimeException(e);
            }
    }

    public void dispatchtx(Object sender, RunnableEx2<Object, Intf> callable) {
        TList<ContextRunnable<Intf>> list;
        synchronized (listeners) {
            list = new TList<>(listeners);
        }
        for (ContextRunnable<Intf> ctxRun : list)
            try {
                callable.run(ctxRun.context, ctxRun.runnable);
            } catch (RuntimeException | Error e) {
                throw e;
            } catch (Throwable e) {
                throw new RuntimeException(e);
            }
    }

    public void removeByRunnable(Intf runnable) {
        synchronized (listeners) {
            for (ContextRunnable<Intf> cr : new TList<>(listeners))
                if (cr.runnable == runnable)
                    listeners.remove(cr);
        }
    }

    public void removeByContext(Object context) {
        synchronized (listeners) {
            for (ContextRunnable<Intf> cr : new TList<>(listeners))
                if (cr.context == context)
                    listeners.remove(cr);
        }
    }

    public boolean isEmpty() {
        return listeners.isEmpty();
    }

    public Dispatcher<Intf> clear() {
        listeners.clear();
        return this;
    }

}
