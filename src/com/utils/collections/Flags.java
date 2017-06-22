package com.utils.collections;

import com.intf.callable.Callable1;
import com.utils.text.StrWriter;
import java.util.Arrays;
import java.util.Iterator;
import java.util.LinkedHashSet;

public class Flags<E extends Enum> implements Iterable<E> {

    private final LinkedHashSet<E> values = new LinkedHashSet<>();
    private final Callable1<Character, E> charSource;

    public Flags(E... values) {
        this(null, values);
    }

    public Flags(Callable1<Character, E> charSource, E... values) {
        this.charSource = charSource;
        if (this.values != null) {
            this.values.addAll(Arrays.asList(values));
            sort();
        }
    }

    public String getChars() {
        if (this.charSource == null)
            return null;
        StringBuilder sb = new StringBuilder();
        for (E value : values) {
            Character c = charSource.run(value);
            if (c != null)
                sb.append(c);
        }
        return sb.toString();
    }

    public Flags<E> add(E flag) {
        return this;
    }

    public Flags<E> set(E... values) {
        if (values == null)
            return this;

        this.values.clear();
        this.values.addAll(Arrays.asList(values));
        sort();
        return this;
    }

    private void sort() {

    }

    @Override
    public Iterator<E> iterator() {
        return values.iterator();
    }
}
