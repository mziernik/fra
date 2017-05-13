package com.events;

import com.context.AppContext;
import com.context.index.Index;
import com.exceptions.ThrowableException;
import com.lang.LEvents;
import com.mlogger.Log;
import com.servlet.Handlers;
import com.utils.reflections.TClass;

/**
 * @author Miłosz Ziernik
 * @date 08 stycznia 2016
 * @encoding UTF-8
 */
public class Notifier {

    final Object instance;
    boolean throwsExceptions = true;

    public Notifier(Object instance) {
        this.instance = instance;
    }

    public static void register(Class<? extends NotifyListener<? extends Notifier>> listener) {
        TClass<? extends NotifyListener<? extends Notifier>> cls = new TClass<>(listener);
        Class<?>[] types = cls.getClassGenericTypes();

        if (types.length < 1)
            throw new UnsupportedOperationException(
                    LEvents.MISSING_TYPE_DECLATATION_OF_GENERIC_CLASS.toString(listener.getName()));

        NotifyListener<? extends Notifier> instance = cls.newInstance(null);

        Index.events.add((Class<? extends Notifier>) types[0], instance);

    }

    /**
     * Czy w przypadku wystąpienia wyjątku przekazywać go dalej czy wytłumić
     *
     * @param throwsExceptions
     * @return
     */
    public Notifier throwsExceptions(boolean throwsExceptions) {
        this.throwsExceptions = throwsExceptions;
        return this;
    }

    protected void onNotifyError(NotifyListener listener, Throwable e) throws Exception {
        Log.error(e);
    }

    /**
     * Czy można wywołać metodę onNotify danego listenera
     *
     * @param listener
     * @return
     */
    protected boolean canNotify(NotifyListener listener) {
        return true;
    }

    public int execute() {
        int count = 0;
        try {
            if (!Handlers.events.getInstance().onEvent(instance))
                return count;

            for (Class<?> cls : Index.events.keySet())
                if (new TClass(instance.getClass()).instanceOf(cls))

                    for (NotifyListener listener : Index.events.get(cls))
                        try {
                            if (canNotify(listener)) {
                                ++count;
                                listener.onNotify(instance);
                            }
                        } catch (Throwable e) {
                            if (throwsExceptions)
                                throw new NotifierException(e);
                            onNotifyError(listener, e);
                        }

        } catch (Throwable e) {
            if (AppContext.unitTestMode)
                throw new ThrowableException(e);
            Log.error(e);
        }
        return count;
    }

}

class NotifierException extends RuntimeException {

    public NotifierException(Throwable ex) {
        super(ex);
    }

}
