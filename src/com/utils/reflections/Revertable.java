package com.utils.reflections;

import com.exceptions.ServiceException;

public interface Revertable<T extends Revertable<T>> {

    @FunctionalInterface
    public static interface SnapshotRunnable<T> {

        public void run(T object) throws Exception;
    }

    default void transaction(SnapshotRunnable<T> runnable) {
        Snapshot<Revertable<T>> clone = new Snapshot<>(this);
        try {
            runnable.run((T) this);
        } catch (Throwable e) {
            clone.revert();
            throw new ServiceException(e);
        }

    }

}
