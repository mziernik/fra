package com.utils.collections;

import com.utils.Utils;
import com.utils.collections.CollectionException.CollectionExceptionType;
import java.util.*;
import java.util.Map.Entry;
import java.util.function.*;

public class SyncMap<Key, Value> extends TCollection<Entry<Key, Value>>
        implements Map<Key, Value> {

    private final Map<Key, Value> raw;

    public SyncMap() {
        this.raw = new LinkedHashMap<>();
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
    public TList<Entry<Key, Value>> asList() {
        synchronized (raw) {
            return new TList<>(raw.entrySet());
        }
    }

    @Override
    public boolean contains(Entry<Key, Value> entry) {
        return findFirst((Entry<Key, Value> t) -> Objects.equals(t, entry)) != null;
    }

    @Override
    public boolean containsKey(Object key) {
        return raw.containsKey(key);
    }

    @Override
    public boolean containsValue(Object value) {
        return raw.containsValue(value);
    }

    @Override
    public Value get(Object key) {
        return raw.get(key);
    }

    @Override
    public Value put(Key key, Value value) {
        synchronized (raw) {
            Value prevValue = raw.get(key);
            Map<Key, Value> items = new LinkedHashMap<>();
            items.put(key, value);
            putAll(items);
            return prevValue;
        }
    }

    @Override
    public void putAll(Map<? extends Key, ? extends Value> m) {
        if (m.isEmpty())
            return;

        if (readOnly) {
            if (readOnlyException)
                throw new CollectionException(this, CollectionExceptionType.READ_ONLY, "Read only");
            return;
        }

        Map<Key, Value> destination = new LinkedHashMap<>();

        for (Entry<? extends Key, ? extends Value> entry : m.entrySet()) {
            if (!allowNulls && (entry.getKey() == null || entry.getValue() == null)) {
                if (nullException)
                    throw new CollectionException(this, CollectionExceptionType.NULL, "Null item");
                return;
            }

            destination.put(entry.getKey(), entry.getValue());
        }

        if (!listeners.dispatchBreak(this, intf -> !intf.onChange(
                CollectionAction.BEFORE_ADD,
                (Collection<? extends Entry<Key, Value>>) destination.entrySet())))
            return;

        synchronized (raw) {
            if (sizeLimit > 0 && destination.size() > sizeLimit)
                throw new CollectionException(this, CollectionExceptionType.OVERLOAD, "Oversize (" + destination.size() + ")");

            int oversize = sizeLimit > 0 ? raw.size() + destination.size() - sizeLimit : 0;

            if (sizeLimit > 0 && oversize > 0)
                switch (overloadAction) {
                    case SHIFT:
                        raw.keySet().removeAll(new ArrayList<Key>(raw.keySet()).subList(0, oversize));

                        break;

                    case POP:
                        raw.keySet().removeAll(new ArrayList<Key>(raw.keySet()).subList(raw.size() - oversize, raw.size()));
                        break;

                    case SKIP:
                        if (oversize == destination.size())
                            return;
                        destination.keySet().removeAll(new ArrayList<Key>(destination.keySet()).subList(destination.size() - oversize, destination.size()));
                        break;

                    case ERROR:
                        throw new CollectionException(this, CollectionExceptionType.OVERLOAD, "Overload");
                }

            raw.putAll(m);
        }

        listeners.dispatch(this, intf -> intf.onChange(CollectionAction.AFTER_ADD,
                (Collection<? extends Entry<Key, Value>>) destination.entrySet()));

    }

    @Override
    public Value remove(Object key) {
        synchronized (raw) {
            if (!raw.containsKey(key))
                return null;

            Set<Key> set = new LinkedHashSet<>(1);
            set.add((Key) key);

            Value val = raw.get(key);
            removeAll(set);
            return val;
        }
    }

    @Override
    public boolean remove(Object key, Object value) {
        synchronized (raw) {
            if (Objects.equals(raw.get(key), value) && remove(key) != null)
                return true;
            return false;
        }
    }

    @Override
    public Entry<Key, Value> removeFirst() {
        if (isEmpty())
            return null;

        synchronized (raw) {
            for (Entry<Key, Value> en : raw.entrySet()) {
                remove(en.getKey());
                return en;
            }
        }

        return null;
    }

    @Override
    public Entry<Key, Value> removeLast() {
        if (isEmpty())
            return null;
        Entry<Key, Value> en;

        synchronized (raw) {
            en = new TList<>(raw.entrySet()).last();
            if (en != null)
                remove(en.getKey());
        }

        return en;
    }

    private void removeAll(Set<? extends Key> keys) {
        if (keys.isEmpty())
            return;

        if (readOnly) {
            if (readOnlyException)
                throw new CollectionException(this, CollectionExceptionType.READ_ONLY, "Read only");
            return;
        }

        Map<Key, Value> destination = new LinkedHashMap<>(keys.size());
        synchronized (raw) {
            if (keys.size() == 1) {
                Key k = (Key) keys.toArray()[0];
                if (raw.containsKey(k))
                    destination.put(k, raw.get(k));
            } else
                raw.entrySet().stream()
                        .filter((Entry<Key, Value> t) -> {
                            if (keys.isEmpty())
                                return false;

                            boolean contains = keys.contains(t.getKey());
                            keys.remove(t.getKey());
                            return contains;
                        })
                        .forEach((Entry<Key, Value> t) -> {
                            destination.put(t.getKey(), t.getValue());
                        });

            if (!listeners.dispatchBreak(this, intf -> intf.onChange(
                    CollectionAction.BEFORE_REMOVE,
                    (Collection<? extends Entry<Key, Value>>) destination.entrySet())))
                return;

            raw.keySet().removeAll(destination.keySet());
        }

        listeners.dispatch(this, intf -> intf.onChange(CollectionAction.AFTER_REMOVE,
                (Collection<? extends Entry<Key, Value>>) destination.entrySet()));
    }

    @Override
    public void clear() {
        if (readOnly) {
            if (readOnlyException)
                throw new CollectionException(this, CollectionExceptionType.READ_ONLY, "Read only");
            return;
        }

        if (!listeners.dispatchBreak(this, intf -> intf.onChange(CollectionAction.BEFORE_CLEAR, null)))
            return;

        synchronized (raw) {
            raw.clear();
        }

        listeners.dispatch(this, intf -> intf.onChange(CollectionAction.AFTER_CLEAR, null));
    }

    @Override
    public LinkedHashSet<Key> keySet() {
        synchronized (raw) {
            return new LinkedHashSet<>(raw.keySet());
        }
    }

    @Override
    public TList<Value> values() {
        synchronized (raw) {
            return new TList<>(raw.values());
        }
    }

    @Override
    public HashSet<Entry<Key, Value>> entrySet() {
        synchronized (raw) {
            return new HashSet<>(raw.entrySet());
        }
    }

    @Override
    public Value getOrDefault(Object key, Value defaultValue) {
        synchronized (raw) {
            return raw.getOrDefault(key, defaultValue);
        }
    }

    private HashMap<Key, Value> asMap() {
        return new HashMap<>(raw);
    }

    @Override
    public void forEach(BiConsumer<? super Key, ? super Value> action) {
        synchronized (raw) {
            asMap().forEach(action);
        }
    }

    @Override
    public Value putIfAbsent(Key key, Value value) {
        synchronized (raw) {
            if (raw.containsKey(key))
                return raw.get(key);
            put(key, value);
            return null;
        }
    }

    @Override
    public Value replace(Key key, Value value) {
        if (raw.isEmpty())
            return null;

        if (readOnly) {
            if (readOnlyException)
                throw new CollectionException(this, CollectionExceptionType.READ_ONLY, "Read only");
            return null;
        }

        if (!allowNulls && value == null) {
            if (nullException)
                throw new CollectionException(this, CollectionExceptionType.NULL, "Null item");
            return null;
        }

        synchronized (raw) {
            if (!raw.containsKey(key))
                return null;

            Map<Key, Value> destination = new LinkedHashMap<>(1);
            destination.put(key, raw.get(key));

            if (!listeners.dispatchBreak(this, intf -> intf.onChange(
                    CollectionAction.BEFORE_MODIFY,
                    (Collection<? extends Entry<Key, Value>>) destination.entrySet())))
                return null;

            Value prevValue = raw.replace(key, value);
            destination.clear();
            destination.put(key, raw.get(key));

            listeners.dispatch(this, intf -> intf.onChange(
                    CollectionAction.AFTER_MODIFY,
                    (Collection<? extends Entry<Key, Value>>) destination.entrySet()));

            return prevValue;
        }
    }

    @Override
    public boolean replace(Key key, Value oldValue, Value newValue) {
        if (raw.isEmpty())
            return false;

        if (readOnly) {
            if (readOnlyException)
                throw new CollectionException(this, CollectionExceptionType.READ_ONLY, "Read only");
            return false;
        }

        if (!allowNulls && newValue == null) {
            if (nullException)
                throw new CollectionException(this, CollectionExceptionType.NULL, "Null item");
            return false;
        }

        synchronized (raw) {
            if (!raw.containsKey(key) || !Objects.equals(raw.get(key), oldValue))
                return false;

            Map<Key, Value> destination = new LinkedHashMap<>(1);
            destination.put(key, raw.get(key));

            if (!listeners.dispatchBreak(this, intf -> intf.onChange(
                    CollectionAction.BEFORE_MODIFY,
                    (Collection<? extends Entry<Key, Value>>) destination.entrySet())))
                return false;

            boolean replaced = raw.replace(key, oldValue, newValue);
            destination.clear();
            destination.put(key, raw.get(key));

            listeners.dispatch(this, intf -> intf.onChange(
                    CollectionAction.AFTER_MODIFY,
                    (Collection<? extends Entry<Key, Value>>) destination.entrySet()));
            return replaced;
        }
    }

    @Override
    public void replaceAll(BiFunction<? super Key, ? super Value, ? extends Value> function) {
        if (raw.isEmpty())
            return;

        if (readOnly) {
            if (readOnlyException)
                throw new CollectionException(this, CollectionExceptionType.READ_ONLY, "Read only");
            return;
        }

        Pairs<Entry<Key, Value>, Value> newValues = new Pairs<>();

        if (!listeners.dispatchBreak(this, intf -> intf.onChange(
                CollectionAction.BEFORE_MODIFY,
                (Collection<? extends Entry<Key, Value>>) asMap().entrySet())))
            return;

        synchronized (raw) {
            for (Entry<Key, Value> entry : raw.entrySet())
                newValues.add(entry, function.apply(entry.getKey(), entry.getValue()));

            if (!allowNulls && newValues.secondList().contains(null)) {
                if (nullException)
                    throw new CollectionException(this, CollectionExceptionType.NULL, "Null item");
                return;
            }

            for (Pair<Entry<Key, Value>, Value> newValue : newValues)
                newValue.first.setValue(newValue.second);

            listeners.dispatch(this, intf -> intf.onChange(CollectionAction.AFTER_MODIFY,
                    (Collection<? extends Entry<Key, Value>>) asMap().entrySet()));
        }
    }

    @Override
    public Value computeIfAbsent(Key key, Function<? super Key, ? extends Value> mappingFunction) {
        synchronized (raw) {
            return !raw.containsKey(key) ? put(key, mappingFunction.apply(key)) : null;
        }
    }

    @Override
    public Value computeIfPresent(Key key, BiFunction<? super Key, ? super Value, ? extends Value> remappingFunction) {
        synchronized (raw) {
            return raw.containsKey(key) ? replace(key, remappingFunction.apply(key, raw.get(key))) : null;
        }
    }

    @Override
    public Value compute(Key key, BiFunction<? super Key, ? super Value, ? extends Value> remappingFunction) {
        synchronized (raw) {
            Value value = remappingFunction.apply(key, raw.get(key));
            return raw.containsKey(key) ? replace(key, value) : put(key, value);
        }
    }

    @Override
    public Value merge(Key key, Value value, BiFunction<? super Value, ? super Value, ? extends Value> remappingFunction) {
        synchronized (raw) {
            if (raw.containsKey(key)) {
                Value oldValue = raw.get(key);
                return oldValue == null ? replace(key, value) : replace(key, remappingFunction.apply(oldValue, value));
            } else
                return put(key, value);
        }
    }

    @Override
    public Entry<Key, Value> first() {
        if (isEmpty())
            return null;

        synchronized (raw) {
            for (Entry<Key, Value> en : raw.entrySet())
                return en;
        }

        return null;
    }

    @Override
    public Entry<Key, Value> last() {
        if (isEmpty())
            return null;

        synchronized (raw) {
            return new TList<>(raw.entrySet()).last();
        }
    }

    @Override
    public Entry<Key, Value> findFirst(Predicate<? super Entry<Key, Value>> filter) {
        synchronized (raw) {
            return Utils.findFirst(raw.entrySet(), filter);
        }
    }

    @Override
    public List<Entry<Key, Value>> find(Predicate<? super Entry<Key, Value>> filter) {
        synchronized (raw) {
            return Utils.find(raw.entrySet(), filter);
        }
    }

    @Override
    public Iterator<Entry<Key, Value>> iterator() {
        return asList().iterator();
    }

    @Override
    public void forEach(Consumer<? super Entry<Key, Value>> action) {
        asList().forEach(action);
    }

    @Override
    public Spliterator<Entry<Key, Value>> spliterator() {
        return asList().spliterator();
    }
}
