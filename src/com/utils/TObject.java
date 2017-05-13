package com.utils;

import com.intf.callable.Callable;
import com.intf.callable.CallableEx;
import com.intf.callable.CallableEx1;
import com.intf.runnable.Runnable1;
import com.intf.runnable.RunnableEx1;
import java.util.*;
import javax.xml.ws.Holder;

public class TObject<T> {

    private final LinkedHashSet<TObjectListener<T>> listeners = new LinkedHashSet<>();
    private String name;
    private boolean notNull = false;

    public final LinkedHashMap<Object, Object> extra = new LinkedHashMap<>(); // pola dodatkowe

    private Holder<T> raw;
    private Callable<T> provider;

    public TObject() {

    }

    public TObject(T value) {
        set(value);
    }

    public TObject(Callable<T> provider) {
        set(provider);
    }

    public boolean clear() {
        return set((T) null);
    }

    public boolean set(Callable<T> provider) {
        if (provider == null && notNull)
            throw new NullPointerException(name);

        boolean result = set(provider.run());
        if (result) {
            this.raw = null;
            this.provider = provider;
        }
        return result;
    }

    public boolean set(T value) {
        if (value == null && notNull)
            throw new NullPointerException(name);

        if (!listeners.isEmpty()) {
            T v = doGet();
            for (TObjectListener<T> listener : new LinkedHashSet<>(listeners))
                if (!listener.onChange(this, v, value))
                    return false;
        }

        this.raw = new Holder<>(value);
        this.provider = null;
        return true;
    }

    @Override
    public String toString() {
        return Utils.toString(doGet());
    }

    public T get(T defaultValue) {
        T result = doGet();
        return result != null ? result : defaultValue;
    }

    private T doGet() {
        return raw != null ? raw.value : provider != null ? provider.run() : null;
    }

    public T get() {
        T result = doGet();
        if (result == null && notNull)
            throw new NullPointerException(name);
        return result;
    }

    public T getF() {
        return Objects.requireNonNull(get());
    }

    public T getF(String message) {
        return Objects.requireNonNull(get(), message);
    }

    public T getIfNotNull(Runnable1<T> onNotNull) {
        T v = doGet();
        if (v != null && onNotNull != null)
            onNotNull.run(v);
        return v;
    }

    @Override
    public boolean equals(Object obj) {
        return Objects.equals(obj, doGet());
    }

    @Override
    public int hashCode() {
        return Is.nullR(doGet(), () -> 0, (T t) -> t.hashCode());
    }

    public TObject<T> extra(Object key, Object value) {
        this.extra.put(key, value);
        return this;
    }

    public Object extra(Object key) {
        return extra.get(key);
    }

    public <O> O extra(Object key, Class<O> cls) {
        return (O) extra.get(key);
    }

    public TObject<T> onChange(TObjectListener<T> listener) {
        this.listeners.add(listener);
        return this;
    }

    public TObject<T> removeListener(TObjectListener<T> listener) {
        this.listeners.remove(listener);
        return this;
    }

    /**
     * Nazwa pola / obiektu (wyswietlana w komunikatach błędów)
     *
     * @param name
     * @return
     */
    public TObject<T> name(String name) {
        this.name = name;
        return this;
    }

    public TObject<T> notNull(boolean notNull) {
        this.notNull = notNull;
        return this;
    }

    public boolean isNull() {
        return doGet() == null;
    }

    public boolean isEmpty() {
        return Is.empty(doGet());
    }

    public T ifNull(CallableEx<T> callback) {
        return Is.nullR(doGet(), callback);
    }

    public T ifNotNull(CallableEx1<T, T> callback) {
        return Is.nonEmptyR(doGet(), callback);
    }

    public T ifEmpty(CallableEx<T> callback) {
        return Is.emptyR(doGet(), callback);
    }

    public T ifNonEmpty(CallableEx1<T, T> callback) {
        return Is.nonEmptyR(doGet(), callback);
    }

    public boolean isRawValue() {
        return raw != null;
    }

    public boolean isProvider() {
        return provider != null;
    }

    @FunctionalInterface
    public static interface TObjectListener<T> {

        public boolean onChange(TObject<T> sender, T currentValue, T newValue);

    }

}
