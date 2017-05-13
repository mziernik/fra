package com.utils.collections;

import java.util.*;

public class MapList<Key, Item> extends MapCollection<Key, Item, LinkedList<Item>> {

    public MapList() {
        super(new LinkedHashMap<>());
    }

    public MapList(Map<Key, LinkedList<Item>> map) {
        super(map);
    }

    @Override
    protected LinkedList<Item> getCollectionInstance() {
        return new LinkedList<>();
    }

    @Override
    public LinkedList<Item> get(Key key) {
        return (LinkedList<Item>) super.get(key);
    }

}
