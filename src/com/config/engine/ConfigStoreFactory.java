package com.config.engine;

import com.exceptions.EError;
import com.json.*;
import com.mlogger.Log;
import com.utils.Is;
import com.utils.collections.Pair;
import com.utils.collections.Quad;
import com.utils.collections.TList;
import com.utils.collections.Triple;

/**
 * Wszystko co zwązane z zapisem i odczytem konfiguracji
 */
public class ConfigStoreFactory<FIELD extends ConfigField<FIELD, RAW, ROW>, RAW, ROW> {

    final FIELD field;

    public ConfigStoreFactory(FIELD field) {
        this.field = field;
    }

    public ConfigStoreFactory set(boolean isDefault, String variable, JElement value) {
        set(true, isDefault, variable, value);
        return this;
    }

    public ConfigStoreFactory set(JObject data) {
        set(true,
                data.getBool("def", true),
                data.getStr("var", null),
                data.element("val", new JNull()));
        return this;
    }

    public ConfigStoreFactory set(JArray arr) {
        if (arr.size() != 3)
            throw new ConfigException("Invalid elements count");
        set(true,
                arr.element(0).asBoolean(),
                arr.element(1).asString(),
                arr.element(2));
        return this;
    }

    public JArray toArray(ValueSource source) {
        return new JArray().addAll(
                field.isDefaultState(),
                field.variable(),
                getValuesArray(source)
        );
    }

    public JArray getValuesArray(ValueSource source) {
        JArray result = new JArray();
        for (ROW val : field.asList(field.getValue(source))) {
            JArray arr = result.array();
            for (Pair<ConfigCell<?, ?>, ROW> pair : field.getRowCellValues(val))
                arr.add(pair.first.doSerialize(pair.second));
        }
        return result;
    }

    public JObject toObject(ValueSource source) {
        return new JObject(field.getKey())
                .put("def", field.isDefaultState())
                .put("var", field.variable())
                .put("val", getValuesArray(source));
    }

    public void set(boolean isUserValue, boolean isDefault, String variable,
            JElement value) {

        if (!isUserValue && !field.allowChangeDefault())
            throw new ConfigException(field, "Cannot change default value");

        if (!value.isNull()) {
            if (!value.isArray())
                value = new JArray().add(value);

            JArray arr = value.asArray();
            if (!arr.isEmpty()) {
                TList<JElement> elements = arr.getElements();
                arr.clear();
                for (JElement el : elements) {
                    if (!el.isArray())
                        el = new JArray().add(el);
                    arr.add(el);
                }
            }
            value = arr;
        }

        RAW val = null;
        if (Is.empty(variable))
            val = value.isArray() ? parse(value.asArray()) : null;

        boolean def = isDefault;
        String var = variable;

        try {
            field.setVariable(variable);

            setDefaultState(isDefault);
            if (!field.hasVariable() && isUserValue)
                field.setUserValue(val, isUserValue && !isDefault);

            if (!isUserValue)
                field.setDefaultValue(val);
        } catch (Throwable e) {
            setDefaultState(def);
            field.setVariable(var);
            throw e;
        }
    }

    /**
     * Weryfikuje poprawność wiersza
     *
     * @param json tablica lub string (w przypadku zmiennej)
     * @return Dane po korekcie (np trimowane)
     */
    public JObject validateRow(JElement json) {

        boolean variable = json.isValue() && json.asValue().isString();
        String error = null;
        JObject result = new JObject();

        if (variable)
            result.put("cells", json.asString().trim());
        else {

            boolean def = isDefaultState();
            try {
                setDefaultState(false);
                RAW value = parse(json.asArray());
                field.validate(value, true);
                ROW obj = field.asList(value).first();

                JArray jcells = result.arrayC("cells");

                for (Pair<ConfigCell<?, ?>, ROW> pair : field.getRowCellValues(obj)) {
                    JElement val = pair.first.doSerialize(pair.second);
                    // wyjątek dla booleana - musi być przekazany jako string (wartość enumeraty)
                    if (val.isValue() && val.asValue().isBoolean())
                        val = new JValue(Boolean.toString(val.asBoolean()));
                    jcells.add(val);
                }

                result.put("displayValue", field.getDisplayValue(value, true));

            } catch (Throwable ex) {
                Log.warning(ex);
                error = EError.format(ex).message;
            } finally {
                setDefaultState(def);
            }
        }
        result.put("correct", error == null);
        result.put("message", error);

        return result;
    }

    public RAW parse(JArray json) {
        if (isEmptyValue(json))
            return null;

        TList<RAW> result = new TList<>();

        if (!field.multiple && json.size() != 1)
            throw new ConfigException(field, "Incorrect array size");

        for (JElement el : json) {
            if (!el.isArray())
                throw new ConfigException(field, "Array expected");

            JArray arr = el.asArray();

            if (arr.size() != field.cells.length)
                throw new ConfigException(field, "Incorrect elements count.\n"
                        + "Expected " + field.cells.length + ", actual " + arr.size());
            RAW val;
            switch (field.cells.length) {
                case 1:
                    val = (RAW) field.cells[0].parse(arr.element(0));
                    break;
                case 2:
                    val = (RAW) new Pair<>(
                            field.cells[0].parse(arr.element(0)),
                            field.cells[1].parse(arr.element(1))
                    );
                    break;
                case 3:
                    val = (RAW) new Triple<>(
                            field.cells[0].parse(arr.element(0)),
                            field.cells[1].parse(arr.element(1)),
                            field.cells[2].parse(arr.element(2))
                    );
                    break;
                case 4:
                    val = (RAW) new Quad<>(
                            field.cells[0].parse(arr.element(0)),
                            field.cells[1].parse(arr.element(1)),
                            field.cells[2].parse(arr.element(2)),
                            field.cells[3].parse(arr.element(3))
                    );
                    break;
                default:
                    throw new UnsupportedOperationException("Unsupported data format");
            }

            if (!field.multiple)
                return val;

            result.add(val);
        }
        return (RAW) result;
    }

    boolean isEmptyValue(JElement json) {
        if (json == null || json.isEmpty())
            return true;

//        if (json.isValue() && json.asString().trim().isEmpty())
//            return true;
        if (json.isArray() && json.asArray().size() == 1)
            return isEmptyValue(json.asArray().element(0));

        return false;
    }

    boolean isDefaultState() {
        return field.isDefaultState();
    }

    void setDefaultState(boolean state) {
        field.setDefaultState(state);
    }
}
