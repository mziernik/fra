package com.utils.collections;

import com.intf.runnable.Runnable2;
import java.util.*;
import java.util.Map.Entry;

public abstract class MapCollection<Key, Item, Coll extends Collection<Item>>
        implements Iterable<Entry<Key, Coll>> {

    private final Map<Key, Coll> map;

    public MapCollection(Map<Key, Coll> map) {
        this.map = map;
    }

    public Set<Key> keySet() {
        return map.keySet();
    }

    public Collection<Coll> values() {
        return map.values();
    }

    public Entry<Key, Coll> first() {
        return new LinkedList<>(map.entrySet()).peek();
    }

    public TList<Item> allValues() {
        TList<Item> result = new TList<>();
        for (Coll coll : new TList<>(map.values()))
            result.addAll(coll);
        return result;
    }

    public MapCollection<Key, Item, Coll> each(Runnable2<Key, Coll> consumer) {
        forEach(t -> consumer.run(t.getKey(), t.getValue()));
        return this;
    }

    @Override
    public String toString() {
        return map.toString();
    }

    protected abstract Coll getCollectionInstance();

    public Set<Entry<Key, Coll>> entrySet() {
        LinkedHashSet<Entry<Key, Coll>> set = new LinkedHashSet<>();
        set.addAll(map.entrySet());
        return set;
    }

    public void clear() {
        map.clear();
    }

    public MapCollection<Key, Item, Coll> set(Key key, Item item) {
        Coll list = list = getCollectionInstance();
        list.add(item);
        map.put(key, list);
        return this;
    }

    public MapCollection<Key, Item, Coll> addAll(Key key, Collection<Item> items) {
        Coll list = map.get(key);
        if (list == null) {
            list = getCollectionInstance();
            map.put(key, list);
        }
        list.addAll(items);
        return this;
    }

    public MapCollection<Key, Item, Coll> add(Key key, Item item) {
        Coll list = map.get(key);
        if (list == null) {
            list = getCollectionInstance();
            map.put(key, list);
        }
        list.add(item);
        return this;
    }

    public Item getFirst(Key key) {
        Coll col = map.get(key);
        return col != null && !col.isEmpty() ? col.iterator().next() : null;
    }

    public Map<Key, Coll> getMap() {
        return map;
    }

    public MapCollection<Key, Item, Coll> remove(Key key) {
        map.remove(key);
        return this;
    }

    public Collection<Item> get(Key key) {
        Collection<Item> list = map.get(key);
        if (list == null)
            list = getCollectionInstance();
        return list;
    }

    public void addAll(MapCollection<Key, Item, Coll> map) {
        for (Entry<Key, Coll> en : map)
            for (Item it : en.getValue())
                add(en.getKey(), it);
    }

    public void addAll(Map<Key, Iterable<Item>> map) {
        if (map != null)
            for (Entry<Key, Iterable<Item>> en : map.entrySet())
                for (Item it : en.getValue())
                    if (it != null)
                        add(en.getKey(), it);
    }

    public Pairs<Key, Item> getAll() {
        Pairs<Key, Item> pairs = new Pairs<>();

        for (Entry<Key, Coll> en : this)
            for (Item it : en.getValue())
                pairs.add(en.getKey(), it);

        return pairs;
    }

    public boolean contains(Key key) {
        return map.containsKey(key);
    }

    public boolean isEmpty() {
        return map.isEmpty();
    }

    @Override
    public Iterator<Entry<Key, Coll>> iterator() {
        return map.entrySet().iterator();
    }

    public int size() {
        return map.size();
    }
}
