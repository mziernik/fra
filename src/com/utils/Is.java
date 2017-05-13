package com.utils;

import com.exceptions.ThrowableException;
import com.intf.callable.Callable;
import com.intf.callable.Callable1;
import com.intf.callable.CallableEx;
import com.intf.callable.CallableEx1;
import com.intf.runnable.RunnableEx;
import com.intf.runnable.RunnableEx1;
import java.lang.reflect.Array;
import java.util.Arrays;

/**
 * Klasa zawiera zbiór funkcji warunkowych skracających zapis lub wykonywujących
 * typowe operacje
 *
 * @author user
 */
public interface Is {

    public static boolean empty(Object value) {
        String val = Utils.toString(value);
        return val == null || val.isEmpty() || val.trim().isEmpty();
    }

    @SuppressWarnings("unchecked")
    public static <T> boolean in(T value, T... items) {
        if (value == null || items == null || items.length == 0)
            return false;

        if (items != null && value != null)
            for (T item : items) {
                if (item == null)
                    continue;
                if (value.equals(item))
                    return true;
                if (item.getClass().isArray())
                    for (int i = 0; i < Array.getLength(item); i++)
                        if (value.equals(Array.get(item, i)))
                            return true;

            }
        return false;
    }

    /**
     * Jeśli [value] jest nullem to zwróć rezultat funkcji [onNull]. W
     * przeciwnym razie zwróć rezultat funkcji [onNotNull]
     *
     * @param <T>
     * @param <R>
     * @param value
     * @param onNotNull
     * @param onNull
     * @return
     */
    public static <T, R> R nullR(T value, Callable<R> onNull, CallableEx1<R, T> onNotNull) {
        try {
            return value != null
                    ? onNotNull != null ? onNotNull.run(value) : null
                    : onNull != null ? onNull.run() : null;
        } catch (RuntimeException | Error ex) {
            throw ex;
        } catch (Exception ex) {
            throw new ThrowableException(ex);
        }
    }

    /**
     * Jeśli [value] jest nullem to zwróć rezultat funkcji [onNull]. W
     * przeciwnym razie zwróć rezultat funkcji [onNotNull]
     *
     * @param <T>
     * @param value
     * @param onNotNull
     * @param onNull
     */
    public static <T> void nullV(T value, RunnableEx onNull, RunnableEx1<T> onNotNull) {
        try {
            if (value == null && onNull != null)
                onNull.run();
            if (value != null && onNotNull != null)
                onNotNull.run(value);
        } catch (RuntimeException | Error ex) {
            throw ex;
        } catch (Exception ex) {
            throw new ThrowableException(ex);
        }
    }

    /**
     * Jeśli [value] jest puste to zwróć rezultat funkcji [onNull]. W przeciwnym
     * razie zwróć rezultat funkcji [onNotNull]
     *
     * @param <T>
     * @param <R>
     * @param value
     * @param onNonEmpty
     * @param onEmpty
     * @return
     */
    public static <T, R> R emptyR(T value, Callable<R> onEmpty, CallableEx1<R, T> onNonEmpty) {
        try {
            return !Is.empty(value)
                    ? onNonEmpty != null ? onNonEmpty.run(value) : null
                    : onEmpty != null ? onEmpty.run() : null;
        } catch (RuntimeException | Error ex) {
            throw ex;
        } catch (Exception ex) {
            throw new ThrowableException(ex);
        }
    }

    /**
     * Jeśli [value] jest puste to zwróć rezultat funkcji {onEmpty]. W
     * przeciwnym razie zwróć rezultat funkcji [onNotNull]
     *
     * @param <T>
     * @param value
     * @param onNonEmpty
     * @param onEmpty
     */
    public static <T> void emptyV(T value, RunnableEx onEmpty, RunnableEx1<T> onNonEmpty) {
        try {
            boolean empty = Is.empty(value);
            if (empty && onEmpty != null)
                onEmpty.run();
            if (!empty && onNonEmpty != null)
                onNonEmpty.run(value);
        } catch (RuntimeException | Error ex) {
            throw ex;
        } catch (Exception ex) {
            throw new ThrowableException(ex);
        }
    }

    /**
     * Jeśli [value] jest nullem to zwróć rezultat funkcji [callback]
     *
     * @param <T>
     * @param value
     * @param callback
     * @return
     */
    public static <T> T nullR(T value, CallableEx<T> callback) {
        if (value != null)
            return value;
        try {
            return callback.run();
        } catch (RuntimeException | Error ex) {
            throw ex;
        } catch (Exception ex) {
            throw new ThrowableException(ex);
        }
    }

    /**
     * Jeśli [value] jest nullem to wykonaj funkcję [callback]
     *
     * @param <T>
     * @param value
     * @param callback
     */
    public static <T> void nullV(T value, RunnableEx callback) {
        if (value != null)
            return;
        try {
            callback.run();
        } catch (RuntimeException | Error ex) {
            throw ex;
        } catch (Exception ex) {
            throw new ThrowableException(ex);
        }
    }

    /**
     * Jeśli [value] nie jest nullem to zwróć rezultat funkcji [callback]
     *
     * @param <T>
     * @param value
     * @param callback
     * @return
     */
    public static <T, R> R notNullR(T value, CallableEx1<R, T> callback) {
        try {
            return value != null ? callback.run(value) : null;
        } catch (RuntimeException | Error ex) {
            throw ex;
        } catch (Exception ex) {
            throw new ThrowableException(ex);
        }
    }

    /**
     * Jeśli [value] nie jest nullem to wykonaj funkcję [callback]
     *
     * @param <T>
     * @param value
     * @param callback
     */
    public static <T> void notNullV(T value, RunnableEx1<T> callback) {
        if (value != null)
            try {
                callback.run(value);
            } catch (RuntimeException | Error ex) {
                throw ex;
            } catch (Exception ex) {
                throw new ThrowableException(ex);
            }
    }

    /**
     * Zwraca [value] jeśli nie jest puste lub rezultat funkcji [callback]
     *
     * @param <T>
     * @param value
     * @param callback
     * @return
     */
    public static <T> T nonEmptyR(T value, CallableEx1<T, T> callback) {
        if (Is.empty(value))
            return value;
        try {
            return callback.run(value);
        } catch (RuntimeException | Error ex) {
            throw ex;
        } catch (Exception ex) {
            throw new ThrowableException(ex);
        }
    }

    /**
     * Jeśli [value] nie jest puste to wykonaj funkcję [callback]
     *
     * @param <T>
     * @param value
     * @param callback
     */
    public static <T> void nonEmptyV(T value, RunnableEx1<T> callback) {
        if (Is.empty(value))
            return;
        try {
            callback.run(value);
        } catch (RuntimeException | Error ex) {
            throw ex;
        } catch (Exception ex) {
            throw new ThrowableException(ex);
        }
    }

    /**
     * Jeśli [value] jest nullem to zwróć rezultat [callback]
     *
     * @param <T>
     * @param value
     * @param callback
     * @return
     */
    public static <T> T emptyR(T value, CallableEx<T> callback) {
        if (!Is.empty(value))
            return value;
        try {
            return callback != null ? callback.run() : null;
        } catch (RuntimeException | Error ex) {
            throw ex;
        } catch (Exception ex) {
            throw new ThrowableException(ex);
        }
    }

    /**
     * Jeśli [value] jest puste to wykonaj [callback]
     *
     * @param <T>
     * @param value
     * @param callback
     */
    public static <T> void emptyV(T value, RunnableEx callback) {
        if (!Is.empty(value))
            return;
        try {
            callback.run();
        } catch (RuntimeException | Error ex) {
            throw ex;
        } catch (Exception ex) {
            throw new ThrowableException(ex);
        }
    }

    //--------------------------------------------------------------------------
    /**
     * Czy dana klasa istnieje na liscie wywolan. Procedura pomocna przy
     * eliminacji zapetlen
     */
    public static boolean inStackTrace(Class cls) {
        String clsName = cls.getName();
        for (StackTraceElement ste : Thread.currentThread().getStackTrace())
            if (ste.getClassName().startsWith(clsName))
                return true;

        return false;
    }

    public static boolean inStackTrace(Class<?> cls, String method) {
        if (cls == null || method == null)
            return false;
        for (StackTraceElement ste : Thread.currentThread().getStackTrace())
            if (ste.getClassName().equals(cls.getName())
                    && ste.getMethodName().equals(method))
                return true;
        return false;
    }

    public static boolean inStackTrace(String... methods) {
        return methods != null ? inStackTrace(Arrays.asList(methods)) : null;
    }

    public static boolean inStackTrace(Iterable<String> methods) {
        if (methods == null)
            return false;

        for (StackTraceElement ste : Thread.currentThread().getStackTrace())
            for (String method : methods)
                if (method != null && method.equals(ste.getClassName() + "." + ste.getMethodName()))
                    return true;

        return false;

    }

    public static <T, C> T instanceOf(T object, Class<C> cls, Callable1<T, C> intf) {
        return object == null || (!cls.isAssignableFrom(object.getClass()))
                ? object : intf.run((C) object);
    }

}

/*
    przykład:

        String os = System.getProperty("os");
        if (os != null)
            os = os.trim() + " OK";
        else
            os = "Brak";

      -----------------------------------------------

        os = Is.nullR(System.getProperty("os"), () -> "Brak", (o) -> o.trim() + " OK");
 
 */
