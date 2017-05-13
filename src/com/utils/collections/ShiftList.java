package com.utils.collections;

import java.util.*;

@SuppressWarnings("unchecked")
public class ShiftList<T extends Object> implements Iterable<T> {

    @Override
    public Iterator<T> iterator() {
        return new Iterator<T>() {
            public int itr = index;

            @Override
            public boolean hasNext() {
                return itr < total && itr <= index;

            }

            @Override
            public T next() {
                throw new UnsupportedOperationException("Not supported yet.");
            }

            @Override
            public void remove() {
            }
        };
    }

    public static interface IOverrideItem<T extends Object> {

        public void onOverrideItem(T item);
    }
    //-----------------------------------
    public IOverrideItem onOverrideItem;
    private final Object[] array;
    private int index = 0;
    private long total = 0;

    public <T extends Object> ShiftList(int size) {
        array = new Object[size];
    }

    public long size() {
        return total;
    }

    public List<T> getArray() {

        if (total <= array.length) {
            List<T> lst = new ArrayList<>();
            for (int i = 0; i < total; i++)
                lst.add((T) array[i]);
            return lst;
        }

        int len = (int) (total >= array.length ? array.length : index);

        List<T> lst = new ArrayList<>();

        for (int i = index; i < len; i++)
            lst.add((T) array[i]);

        for (int i = 0; i < index; i++)
            lst.add((T) array[i]);

        return lst;
    }

    public void add(T... elements) {
        if (elements == null)
            return;
        for (T el : elements) {
            if (onOverrideItem != null && total > index)
                onOverrideItem.onOverrideItem(array[index]);
            array[index] = el;
            ++index;
            ++total;
            if (index >= array.length)
                index = 0;
        }

    }
}
