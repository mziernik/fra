package com.utils.collections;

import com.utils.Utils;
import com.utils.Is;
import com.utils.collections.CollectionException.CollectionExceptionType;
import java.util.*;
import java.util.function.Consumer;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public class SyncList<T> extends TCollection<T> implements Collection<T> {

    private final Collection<T> raw;
    private final boolean unique;

    public SyncList() {
        this(false);
    }

    public SyncList(boolean unique) {
        this.unique = unique;
        this.raw = unique ? new LinkedHashSet<>() : new TList<>();
    }

    public SyncList<T> readOnly(boolean readOnly, boolean raiseException) {
        this.readOnly = readOnly;
        this.readOnlyException = raiseException;
        return this;
    }

    public SyncList<T> allowNulls(boolean allowNulls, boolean raiseException) {
        this.allowNulls = allowNulls;
        this.nullException = raiseException;
        return this;
    }

    public SyncList<T> sizeLimit(int sizeLimit, OverloadAction overloadAction) {
        this.sizeLimit = sizeLimit;
        this.overloadAction = overloadAction;
        return this;
    }

    @Override
    public int size() {
        return raw.size();
    }

    @Override
    public boolean isEmpty() {
        return raw.isEmpty();
    }

    @Override
    public boolean contains(Object o) {
        return raw.contains(o);
    }

    public void sort(Comparator<T> comparator) {
        synchronized (raw) {
            if (raw instanceof List) {
                ((List) raw).sort(comparator);
                return;
            }
            TList<T> list = new TList<>(raw);
            list.sort(comparator);
            raw.clear();
            raw.addAll(list);
        }
    }

    public boolean replace(T from, T to) {
        if (readOnly) {
            if (readOnlyException)
                throw new CollectionException(this, CollectionExceptionType.READ_ONLY, "Read only");
            return false;
        }

        if (!allowNulls && from == null || to == null) {
            if (nullException)
                throw new CollectionException(this, CollectionExceptionType.NULL, "Null item");
            return false;
        }

        if (!listeners.dispatchBreak(this, intf -> intf.onChange(
                CollectionAction.BEFORE_MODIFY, Arrays.asList((T[]) new Object[]{from}))))
            return false;

        boolean replaced = false;

        synchronized (raw) {
            ArrayList<T> list = new ArrayList<>(raw);
            int idx = list.indexOf(from);

            if (idx < 0)
                return false;

            if (raw instanceof TList) {
                final ListIterator<T> li = ((TList<T>) raw).listIterator(idx);

                if (li.hasNext()) {
                    li.next();
                    li.set(to);
                    replaced = true;
                }
            } else if (raw instanceof LinkedHashSet) {
                List<T> toRemove = list.subList(idx, list.size() - 1);
                ((LinkedHashSet<T>) raw).removeAll(toRemove);
                raw.add(to);
                toRemove.remove(0);
                raw.addAll(toRemove);
                replaced = true;
            }
        }

        listeners.dispatch(this, intf -> intf.onChange(
                CollectionAction.AFTER_MODIFY, Arrays.asList((T[]) new Object[]{to})));

        return replaced;
    }

    @Override
    public TList<T> asList() {
        synchronized (raw) {
            return new TList<>(raw);
        }
    }

    public LinkedHashSet<T> toSet() {
        synchronized (raw) {
            return new LinkedHashSet<>(raw);
        }
    }

    @Override
    public Iterator<T> iterator() {
        return asList().iterator();
    }

    public boolean add(int index, T e) {
        List<T> items = new ArrayList<>(1);
        items.add(e);
        return addAll(items, index);
    }

    @Override
    public boolean add(T e) {
        List<T> items = new ArrayList<>(1);
        items.add(e);
        return addAll(items);
    }

    @Override
    public boolean addAll(Collection<? extends T> items) {
        return addAll(items, null);
    }

    private boolean addAll(Collection<? extends T> items, Integer index) {
        if (items.isEmpty())
            return false;

        if (readOnly) {
            if (readOnlyException)
                throw new CollectionException(this, CollectionExceptionType.READ_ONLY, "Read only");
            return false;
        }

        if (!listeners.dispatchBreak(this, intf -> intf.onChange(
                CollectionAction.BEFORE_ADD, items)))
            return false;

        Collection<T> destination = unique ? new LinkedHashSet<>() : new TList<>();

        for (T item : items) {
            if (!allowNulls && item == null) {
                if (nullException)
                    throw new CollectionException(this, CollectionExceptionType.NULL, "Null item");
                return false;
            }

            destination.add(item);
        }
        destination = new ArrayList(destination);

        boolean added = false;

        synchronized (raw) {
            if (sizeLimit > 0 && destination.size() > sizeLimit)
                throw new CollectionException(this, CollectionExceptionType.OVERLOAD, "Oversize (" + destination.size() + ")");

            if (index != null && (index < 0 || index > raw.size()))
                throw new CollectionException(this, CollectionExceptionType.INDEX_OUT_OF_BOUNDS, "Index:" + index + ", Size: " + raw.size());

            int oversize = sizeLimit > 0 ? raw.size() + destination.size() - sizeLimit : 0;

            if (sizeLimit > 0 && oversize > 0)
                switch (overloadAction) {
                    case SHIFT:
                        removeAll(asList().subList(0, oversize));
                        break;

                    case POP:
                        removeAll(asList().subList(raw.size() - oversize, raw.size()));
                        break;

                    case SKIP:
                        if (oversize == destination.size())
                            return false;
                        ((ArrayList<T>) destination).subList(destination.size() - oversize, destination.size()).clear();
                        break;

                    case ERROR:
                        throw new CollectionException(this, CollectionExceptionType.OVERLOAD, "Overload");
                }

            if (index != null)
                // Obsługa dodawania pojedyńczego elementu do listy (tylko jeśli TList)
                if (raw instanceof TList && destination.size() == 1)
                    ((TList<T>) raw).add(index, ((ArrayList<T>) destination).get(0));
                else {
                    // Obłsuga dodawania elementów do LinkedHashSet i TList (jeśli więcej niż 1 element)
                    boolean shift = raw.size() > index;
                    List<T> shiftList = null;

                    if (shift) {
                        shiftList = asList().subList(index, raw.size());
                        raw.removeAll(shiftList);
                    }

                    raw.addAll(destination);

                    if (shift)
                        raw.addAll(shiftList);
                }
            else
                added = raw.addAll(destination);
        }

        final Collection<T> _destination = destination;
        listeners.dispatch(this, intf -> intf.onChange(
                CollectionAction.AFTER_ADD, _destination));

        return added;
    }

    @Override
    public Object[] toArray() {
        return asList().toArray();
    }

    @Override
    public <T> T[] toArray(T[] a) {
        return asList().toArray(a);
    }

    @Override
    public boolean containsAll(Collection<?> c) {
        synchronized (raw) {
            return raw.containsAll(c);
        }
    }

    @Override
    public boolean remove(Object o) {
        List<Object> items = new ArrayList<>(1);
        items.add(o);
        return removeAll(items);
    }

    @Override
    public boolean removeAll(Collection<?> c) {
        if (c.isEmpty())
            return false;

        if (readOnly) {
            if (readOnlyException)
                throw new CollectionException(this, CollectionExceptionType.READ_ONLY, "Read only");
            return false;
        }

        boolean removed = false;

        if (!listeners.dispatchBreak(this, intf -> intf.onChange(
                CollectionAction.BEFORE_REMOVE, (Collection<? extends T>) c)))
            return false;

        synchronized (raw) {
            removed = raw.removeAll(c);
        }

        listeners.dispatch(this, intf -> intf.onChange(
                CollectionAction.AFTER_REMOVE, (Collection<? extends T>) c));

        return removed;
    }

    @Override
    public boolean removeIf(Predicate<? super T> filter) {
        synchronized (raw) {
            List<T> list = raw.stream()
                    .filter(filter)
                    .collect(Collectors.toList());

            if (list.isEmpty())
                return false;
            return removeAll(list);
        }
    }

    @Override
    public boolean retainAll(Collection<?> c) {
        synchronized (raw) {
            List<T> list = asList();
            list.removeAll(c);
            return removeAll(list);
        }
    }

    @Override
    public void clear() {
        if (readOnly) {
            if (readOnlyException)
                throw new CollectionException(this, CollectionExceptionType.READ_ONLY, "Read only");
            return;
        }

        if (!listeners.dispatchBreak(this, intf
                -> intf.onChange(CollectionAction.BEFORE_CLEAR, null)))
            return;

        synchronized (raw) {
            raw.clear();
        }

        listeners.dispatch(this, intf -> intf.onChange(CollectionAction.AFTER_CLEAR, null));

    }

    @Override
    public Spliterator<T> spliterator() {
        return asList().spliterator();
    }

    @Override
    public Stream<T> stream() {
        return asList().stream();
    }

    @Override
    public Stream<T> parallelStream() {
        return asList().parallelStream();
    }

    @Override
    public void forEach(Consumer<? super T> action) {
        asList().forEach(action);
    }

    @Override
    public T first() {
        synchronized (raw) {
            if (raw instanceof TList)
                return ((TList<T>) raw).first();

            Iterator<T> itr = raw.iterator();
            if (itr.hasNext())
                return itr.next();
            return null;
        }
    }

    @Override
    public T last() {
        synchronized (raw) {
            return raw instanceof TList
                    ? ((TList<T>) raw).last()
                    : new TList<>(raw).last();
        }
    }

    @Override
    public T removeFirst() {
        synchronized (raw) {
            if (raw instanceof TList)
                return ((TList<T>) raw).removeFirst();

            T first = first();
            if (first == null)
                return null;

            List<T> items = new ArrayList<>(1);
            items.add(first);
            removeAll(items);

            return first;
        }
    }

    @Override
    public T removeLast() {
        synchronized (raw) {

            if (raw instanceof TList)
                return ((TList<T>) raw).removeLast();

            T last = last();
            if (last == null)
                return null;

            List<T> items = new ArrayList<>(1);
            items.add(last);
            removeAll(items);

            return last;
        }
    }

    @Override
    public T findFirst(Predicate<? super T> filter) {
        return Utils.findFirst(asList(), filter);
    }

    @Override
    public List<T> find(Predicate<? super T> filter) {
        synchronized (raw) {
            return Utils.find(raw, filter);
        }
    }

    public <X, Y> Map<X, Y> map(Function<? super T, ? extends X> keyMapper,
            Function<? super T, ? extends Y> valueMapper) {
        synchronized (raw) {
            return raw.stream().collect(Collectors.toMap(keyMapper, valueMapper));
        }
    }

    public String toString(String separator) {
        return toString(separator, null);
    }

    public String toString(String separator, Function<T, String> mapper) {
        StringBuilder sw = new StringBuilder();
        for (T t : this) {
            String val = mapper != null ? mapper.apply(t) : Utils.toString(t);
            if (val == null)
                continue;
            if (sw.length() > 0)
                sw.append(separator);
            sw.append(val);
        }
        return sw.toString();
    }
}
