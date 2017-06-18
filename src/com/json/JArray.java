package com.json;

import com.json.exceptions.JNotFound;
import com.utils.collections.TList;
import java.io.*;
import java.net.URL;
import java.util.*;

/**
 * Miłosz Ziernik 2014/04/15
 */
public class JArray extends JCollection {

    private final ArrayList<JElement> items = new ArrayList<>();

    public static JArray parse(File source) throws IOException {
        return JSON.parse(source).asArray();
    }

    public static JArray parse(String source) {
        return JSON.parse(source).asArray();
    }

    public static JArray parse(InputStream source) {
        return JSON.parse(source).asArray();
    }

    public static JArray parse(URL source) throws IOException {
        return JSON.parse(source).asArray();
    }

    public static JArray parse(Reader source) {
        return JSON.parse(source).asArray();
    }

    public JArray() {
    }

    public JArray(String name) {
        this.name = name;
    }

    @Override
    public Collection<JElement> asRawCollection() {
        return items;
    }

    @Override
    JElement addElement(String name, JElement el, boolean insert) {
        if (el == null && !options.acceptNulls())
            return null;

        if (insert)
            items.add(0, el);
        else
            items.add(el);
        if (el != null)
            el.parent = this;
        return el;
    }

    public JArray array() {
        JArray arr = new JArray();
        items.add(arr);
        arr.parent = this;
        return arr;
    }

    public JObject object() {
        JObject obj = new JObject();
        items.add(obj);
        obj.parent = this;
        return obj;
    }

    @Override
    public Iterator<JElement> iterator() {
        return new TList<>(items).iterator();
    }

    /**
     * Dodaje element i zwraca siebie
     *
     * @param object
     * @return
     */
    public JArray add(Object object) {
        addElement(null, JSON.serialize(object), false);
        return this;
    }

    public JElement addE(Object object) {
        return addElement(null, JSON.serialize(object), false);
    }

    public JArray insert(Object object) {
        addElement(null, JSON.serialize(object), true);
        return this;
    }

    public JArray addAll(Iterable<?> objects) {
        if (objects instanceof JCollection) {
            add(objects);
            return this;
        }
        if (objects != null)
            for (Object o : objects)
                add(o);
        return this;
    }

    public JArray addAll(Object... objects) {
        if (objects != null)
            for (Object o : objects)
                add(o);
        return this;
    }

    public JElement element(int index) {
        return index >= 0 && index < items.size() ? items.get(index) : null;
    }

    public JElement element(int index, JElement def) {
        JElement element = element(index);
        if (element == null)
            return def;
        return element;
    }

    public JElement elementF(int index) throws JNotFound {
        JElement element = element(index);
        if (element == null)
            throw new JNotFound(this, index);
        return element;
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
    public void sort() {
        throw new UnsupportedOperationException("Not supported yet.");
    }

    @Override
    public void sort(Comparator<JElement> comparator) {
        throw new UnsupportedOperationException("Not supported yet.");
    }

    @Override
    public void join(JCollection second, boolean left) {
        throw new UnsupportedOperationException("Not supported yet.");
    }

    @Override
    public boolean remove() {
        return parent != null ? parent.doRemove(this) : null;
    }

    @Override
    boolean doRemove(Object obj) {
        return items.remove(obj);
    }

    @Override
    public void move(JCollection destination) {
        //ToDo: Do napisania
        throw new UnsupportedOperationException("Not supported yet.");
    }

    @Override
    public void moveChildren(JCollection destination) {
        //ToDo: Do napisania
        throw new UnsupportedOperationException("Not supported yet.");
    }

    protected boolean hasOnlyValues() {

        int length = 0;

        // jeśli tablica zawiera tylko wartości to nie dodawaj entera
        for (JElement el : items) {

            if (el.isArray() || el.isObject())
                return false;

            if (el.isValue() && el.asValue().isString()) {
                if (el.uncommented || (el.comment != null && !el.comment.isEmpty()))
                    return false;
                length += el.asValue().asString().length();
                if (length > 80)
                    return false;
                // jeśli łączna długość stringów w tablicy przekroczy 80 znaków to rozdziel na linie
            }

        }
        return true;
    }

    @Override
    public void invert() {
        throw new UnsupportedOperationException("Not supported yet.");
    }

    public int indexOf(JElement element) {
        return items.indexOf(element);
    }

    /**
     * Przycina tablice do okreslonego rozmiaru
     *
     * @param removeFirst
     * @param maxLength
     * @return
     */
    public JArray trim(boolean removeFirst, int maxLength) {
        if (items.size() > maxLength)
            synchronized (items) {
                while (items.size() > maxLength)
                    items.remove(removeFirst ? 0 : items.size() - 1);
            }
        return this;
    }

    @Override
    public TList<JElement> getElements() {
        return new TList<>(items);
    }

    public TList<Object> asList() {
        TList<Object> list = new TList<>();
        for (JElement el : items) {
            if (el == null || el.isNull()) {
                list.add(null);
                continue;
            }
            if (el.isValue()) {
                list.add(el.asValue().value());
                continue;
            }
            if (el.isObject())
                list.add(el.asObject().asMap());

            if (el.isArray())
                list.add(el.asArray().asList());

        }
        return list;
    }

}
