package com.utils.collections;

import java.util.*;

public class MapSet<Key, Item> extends MapCollection<Key, Item, LinkedHashSet<Item>> {

    public MapSet(Map<Key, LinkedHashSet<Item>> map) {
        super(map);
    }

    public MapSet() {
        super(new LinkedHashMap<>());
    }

    @Override
    protected LinkedHashSet<Item> getCollectionInstance() {
        return new LinkedHashSet<>();
    }

    @Override
    public LinkedHashSet<Item> get(Key key) {
        return (LinkedHashSet<Item>) super.get(key);
    }

}
