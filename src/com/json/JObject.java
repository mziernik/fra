package com.json;

import com.json.exceptions.*;
import com.mlogger.Log;
import com.resources.core.ResData;
import com.utils.date.TDate;
import java.io.*;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.*;
import com.utils.IUnquoted;
import com.utils.collections.TList;
import java.net.URL;

/**
 * Miłosz Ziernik 2014/04/15
 */
public class JObject extends JCollection {

    final LinkedHashMap<String, JElement> items = new LinkedHashMap<>();

    public static JObject parse(String source) {
        return JSON.parse(source).asObject();
    }

    public static JObject parse(URL source) throws IOException {
        return JSON.parse(source).asObject();
    }

    public static JObject parse(ResData source) throws IOException {
        return JSON.parse(source).asObject();
    }

    public static JObject parse(File source) throws IOException {
        return JSON.parse(source).asObject();
    }

    public static JObject parse(byte[] source) {
        return JSON.parse(new ByteArrayInputStream(source)).asObject();
    }

    public static JObject parse(InputStream source) {
        JElement el = JSON.parse(source);
        if (el.isValue() && el.isNull())
            return new JObject();
        return el.asObject();
    }

    public static JObject parse(Reader source) {
        return JSON.parse(source).asObject();
    }

    public JObject() {
    }

    public JObject(String name) {
        this.name = name;
    }

    public Set<String> getNames() {
        return items.keySet();
    }

    @Override
    JElement addElement(String name, JElement el, boolean insert) {
        if ((el == null
                || (el.isValue() && el.asValue().isNull())
                || (el instanceof JNull))
                && !options.acceptNulls())
            return null;

        Map<String, JElement> map = new LinkedHashMap<>();
        if (insert) {
            map.putAll(items);
            items.clear();
            map.remove(name);
        }

        if (el != null)
            el.name = name;
        items.put(name, el);

        if (insert)
            items.putAll(map);

        if (el != null)
            el.parent = this;
        return el;
    }

    @Override
    public final Iterator<JElement> iterator() {
        TList<JElement> set = new TList<>();
        set.addAll(items.values());
        return set.iterator();
    }

    public JElement _put(String name, Object object) {
        if (object instanceof IUnquoted)
            return addElement(name, new JValue(object).quote(false), false);

        if (object instanceof JElement && name == null)
            name = ((JElement) object).getName();

        return addElement(name, JSON.serialize(object), false);
    }

    public JObject put(String name, Object object) {
        _put(name, object);
        return this;
    }
//
//    public JObject putUnquoted(String name, String value) {
//        addElement(name, new JValue(value).quote(false), false);
//        return this;
//    }

    public JObject putRaw(String name, String value) {
        addElement(name, new JValue(value).raw(true), false);
        return this;
    }

    public JObject add(String name, Object object) {
        return put(name, object);
    }

    public boolean has(String name) {
        return contains(name);
    }

    public boolean contains(String name) {
        return items.containsKey(name);
    }

    @Override
    public Collection<JElement> asRawCollection() {
        return items.values();
    }

    /**
     * dodaj element na poczatku listy
     */
    public JElement insert(String name, Object object) {
        return addElement(name, JSON.serialize(object), true);
    }

    public String getStr(String name) throws JException {
        return elementF(name).asValue().asString();
    }

    public String getStr(String name, String def) {
        JValue jval = getValue(name, null);
        return jval != null && jval.isString() ? jval.asString() : def;
    }

    public Character getChar(String name) throws JException {
        return elementF(name).asValue().asString().charAt(0);
    }

    public Character getChar(String name, Character def) {
        JValue jval = getValue(name, null);

        if (jval != null && jval.isString() && !jval.asString().isEmpty())
            return jval.asString().charAt(0);

        return def;
    }

    public int getInt(String name) throws JException {
        return elementF(name).asValue().asNumber().intValue();
    }

    public Integer getInt(String name, Integer def) {
        Number number = getNumber(name, null);
        if (number == null)
            return def;
        return number.intValue();
    }

    public long getLong(String name) throws JException {
        return elementF(name).asValue().asNumber().longValue();
    }

    public Long getLong(String name, Long def) {
        Number number = getNumber(name, null);
        if (number == null)
            return def;
        return number.longValue();
    }

    public double getDouble(String name) throws JException {
        return elementF(name).asValue().asNumber().doubleValue();
    }

    public Double getDouble(String name, Double def) {
        Number number = getNumber(name, null);
        if (number == null)
            return def;
        return number.doubleValue();
    }

    public TDate getDate(String name) throws JException, ParseException {
        return getDate(name, (String) null);
    }

    public TDate getDate(String name, String format)
            throws JException, ParseException {
        String str = elementF(name).asValue().asString();
        return format == null ? new TDate(str)
                : new TDate(new SimpleDateFormat(format).parse(str));
    }

    public TDate getDate(String name, TDate def) {
        return getDate(name, null, def);
    }

    public TDate getDate(String name, String format, TDate def) {
        String str = getStr(name, null);
        if (str == null)
            return def;
        try {
            return format == null ? new TDate(str)
                    : new TDate(new SimpleDateFormat(format).parse(str));
        } catch (Throwable e) {
            Log.warning(e);
        }
        return def;
    }

    public Number getNumber(String name, Number def) {
        JValue jval = getValue(name, null);
        return jval != null && jval.isNumber() ? jval.asNumber() : def;
    }

    public Boolean getBool(String name) throws JException {
        return elementF(name).asValue().asBoolean();
    }

    public Boolean getBool(String name, Boolean def) {
        JValue jval = getValue(name, null);
        return jval != null && jval.isBoolean() ? jval.asBoolean() : def;
    }

    public JElement elementF(final String name) throws JException {
        JElement element = element(name);
        if (element == null)
            throw new JNotFound(this, name);
        return element;
    }

    public JElement elementC(final String name, Class<? extends JElement> def) {
        JElement element = element(name);
        if (element != null)
            return element;

        try {
            element = def.newInstance();
        } catch (Exception ex) {
            Log.error(ex);
            return null;
        }

        items.put(name, element);
        element.parent = this;
        return element;
    }

    public JElement element(final String name) {
        if (name == null)
            return this;
        return items.get(name);
    }

    public JElement element(final String name, JElement def) {
        if (name == null)
            return this;
        JElement el = items.get(name);
        return el != null ? el : def;
    }

    public Map<String, JElement> getItems() {
        return Collections.synchronizedMap(items);
    }

    /* public JObject getObject(final JObject def, final String... names) {
     JElement el = getElement(def, names);
     return el != null && el.isObject() ? el.asObject() : def;
     }

     public JArray getArray(final JArray def, final String... names) {
     JElement el = getElement(def, names);
     return el != null && el.isArray() ? el.asArray() : def;
     }
     */
    public JValue getValue(final String name, final Object def) {
        JElement el = element(name);
        return el != null && el.isValue() ? el.asValue() : null;
    }

    public JValue getValue(final String name) throws JException {
        JElement element = elementF(name);
        if (!element.isValue())
            throw new JIncorrectType(name, element, JValue.class);
        return element.asValue();
    }

    public Object getRawValue(final String name, final Object def) {
        JElement el = element(name);
        if (el == null)
            return def;
        return el.isValue() ? el.asValue().value()
                : el.isNull() ? null : el;
    }

    public Object getRawValue(final String name) throws JException {
        JElement el = elementF(name);
        return el.isValue() ? el.asValue().value() : el;
    }

    /**
     *
     * @param includeNonStringValues konwertuje wartości liczbowe i boolean do
     * tekstu
     * @return Zwraca listę wartości
     */
    public TList<String> getStringValues(boolean includeNonStringValues) {
        TList<String> list = new TList<>();
        for (JElement el : items.values())
            if (el != null && el.isValue()) {
                JValue jval = el.asValue();
                if (jval.isNull() && !options.acceptNulls())
                    continue;
                Object val = jval.value();
                if (includeNonStringValues || jval.isString())
                    list.add(val == null ? null : val.toString());
            }
        return list;
    }

    public TList<Number> getNumberValues(boolean includeNonStringValues) {
        TList<Number> list = new TList<>();
        for (JElement el : items.values())
            if (el != null && el.isValue() && el.asValue().isNumber())
                list.add(el.asValue().asNumber());
        return list;
    }

    /**
     * Zwraca obiekt na podstawie nazwy, jesli element nie istnieje, tworzy go
     *
     * @param name Nazwa elementu
     * @return
     */
    public JObject objectC(final String name) {
        return JObject.this.object(name, true);
    }

    /**
     * Zwraca obiekt na podstawie nazwy, jesli element nie istnieje, zwraca
     * zaślepkę
     *
     * @param name Nazwa elementu
     * @return
     */
    public JObject objectD(final String name) {
        return JObject.this.object(name, null);
    }

    /**
     * Zwraca obiekt na podstawie nazwy, jesli element nie istnieje, zwraca
     * NULL-a
     *
     * @param name Nazwa elementu
     * @return
     */
    public JObject object(final String name) {
        return JObject.this.object(name, false);
    }

    /**
     * Zwraca obiekt na podstawie nazwy, weryfikuje jego typ. Jesli element nie
     * istnieje, zwraca wyjątek
     *
     * @param name Nazwa elementu
     * @return
     * @throws com.json.exceptions.JNotFound
     */
    public JObject objectF(final String name) throws JException {
        JElement element = element(name);
        if (element == null)
            throw new JNotFound(this, name);
        if (!element.isObject())
            throw new JIncorrectType(name, element, JObject.class);
        return element.asObject();
    }

    /**
     *
     * @param name Nazwa elementu
     * @param canCreate false - null, null - atrapa, true - dodaje element
     * @return
     */
    public JObject object(final String name, Boolean canCreate) {
        if (name == null)
            return this;

        JObject obj = null;
        JElement element = element(name);

        if (element != null && element.isObject())
            return element.asObject();

        if (Boolean.FALSE.equals(canCreate))
            return null;

        obj = new JObject();
        obj.name = name;

        if (canCreate == null)
            return obj;

        obj.parent = this;
        items.put(name, obj);

        return obj;
    }

    /**
     * Zwraca tablicę na podstawie nazwy, jesli element nie istnieje, tworzy go
     *
     * @param name Nazwa elementu
     * @return
     */
    @Override
    public JArray arrayC(final String name) {
        return JObject.this.array(name, true);
    }

    /**
     * Zwraca tablicę na podstawie nazwy, jesli element nie istnieje, zwraca
     * zaślepkę
     *
     * @param name Nazwa elementu
     * @return
     */
    public JArray arrayD(final String name) {
        return JObject.this.array(name, null);
    }

    /**
     * Zwraca tablicę na podstawie nazwy, jesli element nie istnieje, zwraca
     * NULL-a
     *
     * @param name Nazwa elementu
     * @return
     */
    public JArray array(final String name) {
        return JObject.this.array(name, false);
    }

    /**
     * Zwraca tablicę na podstawie nazwy, jesli element nie istnieje, zwraca
     * wyjątek
     *
     * @param name Nazwa elementu
     * @return
     * @throws com.json.exceptions.JNotFound
     */
    public JArray arrayF(final String name) throws JException {
        JElement element = element(name);
        if (element == null)
            throw new JNotFound(this, name);
        if (!element.isArray())
            throw new JIncorrectType(name, element, JArray.class);
        return element.asArray();
    }

    /**
     *
     * @param name Nazwa elementu
     * @param canCreate false - null, null - atrapa, true - dodaje element
     * @return
     */
    public JArray array(final String name, Boolean canCreate) {
        if (name == null)
            return null;
        JElement element = element(name);
        if (element != null && element.isArray())
            return element.asArray();

        if (Boolean.FALSE.equals(canCreate))
            return null;

        JArray arr = new JArray();
        arr.name = name;

        if (canCreate == null)
            return arr;

        arr.parent = this;
        items.put(name, arr);
        return arr;
    }

    @Override
    public void clear() {
        items.clear();
    }

    @Override
    public int size() {
        return items.size();
    }

    @Override
    public boolean isEmpty() {
        return items.isEmpty();
    }

    @Override
    public boolean remove() {
        return parent != null ? parent.doRemove(name) : false;
    }

    public JObject remove(String key) {
        synchronized (items) {
            items.remove(key);
        }
        return this;
    }

    @Override
    boolean doRemove(Object obj) {
        return items.remove(obj) != null;
    }

    @Override
    public void move(JCollection destination) {
        if (destination == null)
            return;
        remove();
        destination.addElement(this.name, this, false);
    }

    @Override
    public void moveChildren(JCollection destination) {

    }

    @Override
    public void join(JCollection second, boolean left) {
    }

    @Override
    public void sort() {
        Map<String, JElement> map = new TreeMap<>();
        map.putAll(items);
        items.clear();
        items.putAll(map);
    }

    @Override
    public void sort(Comparator<JElement> comparator) {
        List<JElement> list = new TList<>(items.values());
        Collections.sort(list, comparator);
        items.clear();
        for (JElement el : list)
            items.put(el.getName(), el);
    }

    @Override
    public void invert() {
        throw new UnsupportedOperationException("Not supported yet.");
    }

    public JElement first() {
        return items.values().iterator().next();
    }

    /**
     * Wczytuje strukturę obiektu json z pliku. Jesli plik nie istnieje to
     * tworzy nową instancję obiektu. Ustawienia domyslne jak dla pliku
     * konfiguracyjnego
     *
     * @param file
     * @return
     * @throws IOException
     */
    public static JObject load(File file, boolean createNewObject) throws IOException {

        JObject json = null;

        if (file != null && file.exists() && file.length() > 2) {
            JElement je = JSON.parse(file);
            if (je.isObject())
                json = je.asObject();
        }

        if (json == null && createNewObject)
            json = new JObject();

        if (json == null)
            return null;

        json.extra.put("file", file);
        json.options
                .quotaNames(false)
                .acceptNulls(true)
                .singleLine(null)
                .intent("\t");
        return json;
    }

    @Override
    public JObject comment(String commnet) {
        super.comment(commnet);
        return this;
    }

    @Override
    public JObject uncommented(boolean uncommented) {
        super.uncommented(uncommented);
        return this;
    }

    @Override
    public TList<JElement> getElements() {
        return new TList<>(items.values());
    }

    public Map<String, Object> asMap() {

        Map<String, Object> map = new HashMap<>();
        for (JElement el : this) {
            if (el == null || el.isNull()) {
                map.put(el.name, null);
                continue;
            }
            if (el.isValue()) {
                map.put(el.name, el.asValue().value());
                continue;
            }
            if (el.isObject())
                map.put(el.name, el.asObject().asMap());
            if (el.isArray())
                map.put(el.name, el.asArray().asList());

        }
        return map;
    }

}
