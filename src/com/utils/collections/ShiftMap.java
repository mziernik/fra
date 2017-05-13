package com.utils.collections;

import java.util.LinkedHashMap;
import com.mlogger.Log;
import java.util.Map;
import java.util.Map.Entry;

/**
 * Klasa bazująca na mapie, ilość elementów jest ograniczona do wartości maxSize
 * Każde odwołanie do elementu przenosi go na górę listy, jesli rozmiar listy
 * przkroczy zadeklarowany rozmiar, to zostaną usunięte najstarsze elementy.
 * Można również zadeklarować czas życia danego elementu. Przydatna jako cache
 *
 * @author milosz
 * @param <Key>
 * @param <Value>
 */
public class ShiftMap<Key extends Object, Value extends Object> {

    public class MapData {

        public int accessCount;
        public long lastAccess = System.currentTimeMillis();
        public final Value value;
        public final long created = System.currentTimeMillis();

        public MapData(Value value) {
            this.value = value;
        }

        private MapData inc() {
            ++accessCount;
            lastAccess = System.currentTimeMillis();
            return this;
        }
    }

    public ShiftMap(int maxSize, int expireTime) {
        this.maxSize = maxSize;
        this.expireTime = expireTime;
    }

    private final LinkedHashMap<Key, MapData> map = new LinkedHashMap<>();
    private int maxSize = 100;
    private int expireTime;
    private Class<Value> autoCreateValue;

    public Map<Key, Value> getMap() {
        Map<Key, Value> result = new LinkedHashMap<>();
        for (Entry<Key, MapData> en : map.entrySet())
            result.put(en.getKey(), en.getValue().value);
        return result;
    }

    /**
     * Utwórz automatycznie instację obiektu, jesli nie istnieje
     *
     * @param autoCreateValue
     */
    public void setAutoCreateValue(Class<Value> autoCreateValue) {
        this.autoCreateValue = autoCreateValue;
    }

    public void setMaxSize(int maxSize) {
        this.maxSize = maxSize;
    }

    public void setExpireTime(int expireTime) {
        this.expireTime = expireTime;
    }

    public boolean containsKey(Key key) {
        return map.containsKey(key);
    }

    public void remove(Key key) {
        map.remove(key);
    }

    public void put(Key key, Value value) {

        MapData data = new MapData(value);
        map.put(key, data);

        while (map.size() > maxSize)
            map.remove(map.keySet().iterator().next());
    }

    private Value autoCreate(Key key) {
        try {
            MapData data = new MapData(autoCreateValue.newInstance());
            map.put(key, data);
            return data.inc().value;
        } catch (Exception e) {
            Log.error(e);
        }
        return null;
    }

    public Value get(Key key) {

        boolean has = map.containsKey(key);

        if (autoCreateValue != null && !has)
            return autoCreate(key);

        Value value = null;
        if (has) {
            MapData data = map.get(key);
            map.remove(key);

            if (expireTime <= 0 || data.lastAccess + expireTime >= System.currentTimeMillis())
                map.put(key, data);

            value = data.inc().value;
        }
        return value;
    }

}
