package com.config.engine.cell.select;

import com.config.engine.ConfigCell;
import com.config.engine.ConfigException;
import com.config.engine.DataType;
import com.config.engine.cell.select.SelectEntries.SelectEntry;
import com.intf.callable.Callable;
import com.intf.runnable.Runnable1;
import com.json.JElement;
import com.json.JNull;
import com.json.JValue;

import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Objects;
import com.utils.Is;

public class CcSelect<ROW, RAW> extends ConfigCell<ROW, RAW> {

    protected Callable<SelectEntries<RAW>> enumerate;

    public CcSelect(Class<? extends ROW> clazz, CharSequence name) {
        super(DataType.ENUM, clazz, name);
    }

    public CcSelect<ROW, RAW> enumerate(Runnable1<SelectEntries<RAW>> runnable) {

        this.enumerate = () -> {
            SelectEntries map = new SelectEntries();
            runnable.run(map);
            return map;
        };
        return this;
    }

    public CcSelect<ROW, RAW> enumerate(Callable<SelectEntries<RAW>> enumerate) {
        this.enumerate = enumerate;
        return this;
    }

    public CcSelect<ROW, RAW> enumerateSimple(Runnable1<LinkedHashMap<String, RAW>> enumerate) {
        this.enumerate = () -> {
            LinkedHashMap<String, RAW> map = new LinkedHashMap<>();
            enumerate.run(map);
            return new SelectEntries<>(map);
        };

        return this;
    }

    public CcSelect<ROW, RAW> enumerateSimple(Callable<Map<String, RAW>> enumerate) {
        this.enumerate = () -> new SelectEntries(enumerate.run());
        return this;
    }

    public Callable<SelectEntries<RAW>> enumerate() {
        return enumerate;
    }

    protected SelectEntry<RAW> getEntryByValue(SelectEntries<RAW> map, RAW value) {
        if (map == null)
            map = getMap();

        for (SelectEntry<RAW> en : map)
            if (Objects.equals(en.value, value))
                return en;

        return null;
    }

    protected SelectEntry<RAW> getEntryByValueF(SelectEntries<RAW> map, RAW value) {
        SelectEntry<RAW> result = getEntryByValue(map, value);

        if (result != null)
            return result;

        throw new ConfigException(field, String.format("Entry \"%s\" not found", getKey()));
    }

    @Override
    protected String getDisplayValueRAW(RAW value, boolean publicValue) {
        return Is.nullR(getEntryByValue(null, value), null, (c) -> c.name);
    }

    @Override
    public JElement serialize(ROW value) {
        SelectEntry<RAW> val = getEntryByValue(null, (RAW) value);
        return val == null ? new JNull() : new JValue(val.key);
    }

    @Override
    protected ROW doParse(JElement json) throws Exception {
        return (ROW) getMapF().getF(json.asString());
    }

    public SelectEntries<RAW> getMap() {
        Callable<SelectEntries<RAW>> callable = enumerate();
        if (callable == null)
            return null;

        SelectEntries<RAW> map = callable.run();
        if (map == null)
            return null;

        return map;
    }

    public SelectEntries<RAW> getMapF() {
        Callable<SelectEntries<RAW>> callable = enumerate();
        if (callable == null)
            throw new ConfigException("Enumerate callback is missing");

        SelectEntries<RAW> map = callable.run();
        if (map == null)
            throw new ConfigException("Map is missing");

        return map;
    }
}
