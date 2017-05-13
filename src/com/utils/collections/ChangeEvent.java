package com.utils.collections;

import java.util.Collection;

public interface ChangeEvent<T> {

    public boolean onChange(CollectionAction action, Collection<? extends T> items);
}
