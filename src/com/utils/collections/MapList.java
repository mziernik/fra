package com.utils.collections;

import java.util.*;

public class MapList<Key, Item> extends MapCollection<Key, Item, TList<Item>> {

    public MapList() {
        super(new LinkedHashMap<>());
    }

    public MapList(Map<Key, TList<Item>> map) {
        super(map);
    }

    @Override
    protected TList<Item> getCollectionInstance() {
        return new TList<>();
    }

    @Override
    public TList<Item> get(Key key) {
        return (TList<Item>) super.get(key);
    }

}
