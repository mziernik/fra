package com.config.engine;

import com.config.engine.cell.select.CcSelect;
import com.config.engine.cell.select.CcSelectMultiple;
import com.config.engine.cell.select.CcEnumMultiple;
import com.config.engine.cell.select.SelectEntries;
import com.config.engine.cell.*;
import com.config.engine.cell.select.SelectEntries.SelectEntry;
import com.intf.callable.Callable;
import com.intf.runnable.Runnable1;
import com.json.*;
import static com.lang.LConfig.LACK;

import com.utils.Utils;
import com.utils.collections.TList;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Objects;

public abstract class ConfigCell<ROW, RAW> {

    protected CharSequence name;
    protected ROW defaultValue;
    protected Integer min;
    protected Integer max;
    protected String regExpr;
    protected CharSequence description;
    private final boolean password = this instanceof CcPassword;

    private Callable<Map<String, String>> units;
    protected String selectedUnit;
    protected Boolean list; // ComboBox/Select (lista rozwijalna)

    //-----------------------------
    protected boolean ordered;
    public final boolean multiple = this instanceof CcSelectMultiple
            || this instanceof CcEnumMultiple
            || this instanceof CcStringMultiLine;

    protected ConfigField<?, ?, ?> field;

    public final Class<? extends ROW> cls;
    public final DataType type;

    public ConfigCell(DataType type, Class<? extends ROW> cls, CharSequence name) {
        if (this instanceof CcEnumMultiple || this instanceof CcSelectMultiple)
            cls = (Class<? extends ROW>) TList.class;
        this.cls = Objects.requireNonNull(cls, "ConfigCell class");
        this.type = Objects.requireNonNull(type, "ConfigCell type");
        this.name = name;
    }

    public ConfigCell<ROW, RAW> defaultValue(ROW val) {
        defaultValue = val;
        return this;
    }

    public String getKey() {
        for (int i = 0; i < field.cells.length; i++)
            if (field.cells[i] == this)
                return field.getKey() + "[" + i + "]";
        throw new ConfigException("Key not found");
    }

    protected abstract ROW doParse(JElement json) throws Exception;

    public ROW parse(JElement json) {

        if (field != null && field.isRequired()
                && (json == null || json.isNull() || (json.isValue() && json.asValue().asString().isEmpty())))
            throw new ConfigException("Wartość wymagana");

        try {
            return doParse(json);
        } catch (Error | RuntimeException e) {
            throw e;
        } catch (Exception ex) {
            throw new ConfigException(ex);
        }
    }

    public void validate(ROW item) throws Exception {

    }

    public ConfigCell<ROW, RAW> units(Runnable1<LinkedHashMap<String, String>> runnable) {
        LinkedHashMap<String, String> map = new LinkedHashMap<>();
        runnable.run(map);
        this.units = () -> {
            return map;
        };
        return this;
    }

    public ConfigCell<ROW, RAW> units(Callable<Map<String, String>> units) {
        this.units = units;
        return this;
    }

    public Callable<Map<String, String>> units() {
        return units;
    }

    void validate_(Object item) throws Exception {
        validate((ROW) item);
    }

    public String getClassName() {
        String name = this.getClass().getSimpleName();
        if (name.startsWith("Cc"))
            name = name.substring(2);
        return name;
    }

    public String getDisplayValue(Object value, boolean publicValue) {
        if (value == null)
            return LACK.toString();

        String val = getDisplayValueRAW((RAW) value, publicValue);
        if (publicValue)
            return Utils.cutLongName(val, 150, false);
        return val;
    }

    protected String getDisplayValueRAW(RAW value, boolean publicValue) {
        return Utils.toString(value);
    }

    protected JElement doSerialize(Object value) {
        return value == null ? new JNull() : serialize((ROW) value);
    }

    public JElement serialize(ROW value) {
        return JSON.serialize(value);
    }

    public JObject getStructure() {

        JObject jc = new JObject();
        jc.put("type", type.key);
        jc.put("class", cls.getSimpleName());
        jc.put("min", min);
        jc.put("max", max);
        jc.put("pass", password);
        jc.put("regExpr", regExpr);
        jc.put("multiple", multiple);
        jc.put("name", name);
        jc.put("description", description);
        jc.put("defaultValue", defaultValue != null ? serialize(defaultValue) : null);

        if (this instanceof CcSelect) {

            Callable<SelectEntries<ROW>> callable = ((CcSelect<ROW, ROW>) this).enumerate();

            SelectEntries<ROW> map = callable != null ? callable.run() : null;
            if (map != null && !map.map.isEmpty()) {
                // wyświetlaj jako list
                jc.put("list", Utils.coalesce(list, field.isMultiple() || map.map.size() > 10));
                jc.put("ordered", ordered);
                JObject jenum = jc.objectC("enum");
                for (SelectEntry<ROW> en : map)
                    jenum.put(en.key, en.name);
            }
        }
        Map<String, String> map = units != null ? units.run() : null;
        if (map != null && !map.isEmpty()) {
            jc.put("units", map);
            jc.put("selectedUnit", selectedUnit);
        }

        return jc;
    }

    public ConfigCell<ROW, RAW> min(Integer min) {
        this.min = min;
        return this;
    }

    public ConfigCell<ROW, RAW> name(CharSequence name) {
        this.name = name;
        return this;
    }

    public ConfigCell<ROW, RAW> max(Integer max) {
        this.max = max;
        return this;
    }
}
