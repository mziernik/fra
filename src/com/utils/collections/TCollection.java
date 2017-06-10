package com.utils.collections;

import com.events.Dispatcher;
import com.intf.callable.Callable1;
import com.utils.TObject;
import java.util.List;
import java.util.function.Predicate;

public abstract class TCollection<T> implements Iterable<T> {

    protected int sizeLimit = -1;
    protected OverloadAction overloadAction = OverloadAction.ERROR;
    protected boolean readOnly = false;
    protected boolean readOnlyException;
    protected boolean allowNulls = true;
    protected boolean nullException = false;
    protected final Dispatcher<ChangeEvent<T>> listeners = new Dispatcher<>();

    public TCollection() {

    }

    /*  protected StatusGroup status;
    protected Runnable2<StatusGroup, T> onAddStatus;

    public TCollection<T> setStatus(StatusGroup status, Runnable2<StatusGroup, T> onAddStatus) {
        this.status = status;
        this.onAddStatus = onAddStatus;
        return this;
    }
     */
    public abstract boolean isEmpty();

    public abstract int size();

    public abstract boolean contains(T key);

    public abstract void clear();

    public abstract T first();

    public abstract T last();

    public abstract T removeFirst();

    public abstract T removeLast();

    public abstract T findFirst(Predicate<? super T> filter);

    public abstract List<T> find(Predicate<? super T> filter);

    public <E> E findFirstNotNull(Class<E> cls, Callable1<E, T> callback) {
        TObject<E> result = new TObject<>();
        findFirst((T t) -> {
            E item = callback.run(t);
            if (item != null)
                result.set(item);
            return item != null;
        });
        return result.get();
    }

    public abstract TList<T> asList();

    public void onChange(Object context, ChangeEvent<T> event) {
        this.listeners.listen(context, event);
    }
}
