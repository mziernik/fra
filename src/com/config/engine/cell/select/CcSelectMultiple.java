package com.config.engine.cell.select;

import com.json.JArray;
import com.json.JElement;

import com.utils.Utils;
import com.utils.Is;
import com.utils.collections.Strings;
import com.utils.collections.TList;
import java.util.*;

public class CcSelectMultiple<T> extends CcSelect<Collection<T>, T> {

    public CcSelectMultiple(Class<? extends T> clazz, CharSequence name) {
        super(null, name);
    }

    public CcSelectMultiple ordered(boolean ordered) {
        super.ordered = ordered;
        return this;
    }

    public CcSelectMultiple list(Boolean list) {
        this.list = list;
        return this;
    }

    @Override
    public final String getDisplayValue(Object value, boolean publicValue) {
        SelectEntries<T> map = getMapF();
        Strings result = new Strings();
        Collection<T> values = (Collection<T>) value;
        for (T v : values)
            result.add(getEntryByValueF(map, v).name);
        return "[" + result.toString(", ") + "]";
    }

    @Override
    public JElement serialize(Collection<T> value) {
        SelectEntries<T> map = getMapF();
        JArray arr = new JArray();

        for (T t : value)
            arr.add(getEntryByValueF(map, t).key);

        return arr;
    }

    @Override
    protected TList<T> doParse(JElement json) throws Exception {
        TList<T> result = new TList<>();
        SelectEntries<T> map = getMapF();
        for (String key : json.asArray().getValuesStr())
            result.add(map.getF(key));
        return result;
    }
}
