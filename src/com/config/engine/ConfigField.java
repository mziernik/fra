package com.config.engine;

import com.config.engine.interfaces.AfterChangeListener;
import com.config.engine.interfaces.BeforeChangeListener;
import com.events.Dispatcher;
import com.exceptions.CoreException;
import com.intf.callable.Callable;
import com.intf.callable.Callable1;

import com.utils.Utils;
import com.utils.Is;
import com.utils.collections.*;
import java.util.*;
import java.util.Map.Entry;
import com.utils.reflections.TField;
import com.config.engine.interfaces.Cfg;
import com.events.Dispatcher.ContextRunnable;
import com.intf.runnable.Runnable1;
import com.json.JArray;
import com.json.JObject;
import static com.lang.LConfig.LACK;
import com.utils.TObject;

public class ConfigField<SELF extends ConfigField<SELF, RAW, ROW>, RAW, ROW>
        extends ConfigElement<SELF>
        implements Iterable<ROW>, Cloneable {

    boolean defaultState = true;

    final boolean multiple;
    protected Integer maxItems;
    protected Integer minItems;
    protected boolean unique;
    public final ConfigCell<?, ?>[] cells;
    protected RAW userValue;
    protected final TObject<RAW> defaultValue = new TObject<>();
    private Boolean required = true;
    public boolean readOnly;
    private String variable;
    public final Props extra = new Props();

    //---------------
    Cfg annotation;

    public Callable<Map<String, Pair<String, ConfigField<?, ?, ?>>>> variablesProvider;
    public Callable1<ConfigField<?, ?, ?>, String> variableResolver;

    protected final SELF self = (SELF) this;

    final Dispatcher<ValueGetListener<SELF, RAW, ROW>> onGetValue = new Dispatcher<>();
    final Dispatcher<BeforeChangeListener<SELF, RAW>> onBeforeChange = new Dispatcher<>();
    final Dispatcher<AfterChangeListener<SELF, RAW>> onAfterChange = new Dispatcher<>();
    final Dispatcher<ValueValidatorListener<SELF, RAW, ROW>> validators = new Dispatcher<>();

    protected ConfigField(String key, CharSequence name, RAW defaultValue, ConfigCell<?, ?>... cells) {
        super(key, name);
        if (cells == null || cells.length == 0)
            throw new CoreException("Brak elementów");
        this.cells = cells;
        this.multiple = this instanceof MultipleConfigFields;
        for (ConfigCell<?, ?> cc : cells)
            cc.field = this;
        setDefaultValue(defaultValue);
    }

    public SELF setup(Runnable1<SELF> callback) {
        callback.run(self);
        return self;
    }

    @Override
    protected void setParent(ConfigNode parent, TField field) {
        super.setParent(parent, field);

        if (variablesProvider == null)
            variablesProvider = (Callable) () -> {
                Map<String, Pair<String, ConfigField<?, ?, ?>>> list = new LinkedHashMap<>();
                for (ConfigElement fn : all.values())
                    if (fn != this && fn instanceof ConfigField)
                        list.put(fn.getKey(), new Pair<>(fn.getName().toString(), (ConfigField) fn));
                return list;
            };
    }

    public String getTypeName() {
        Strings strs = new Strings();
        for (ConfigCell<?, ?> cell : cells)
            strs.add(cell.type.key);
        return strs.size() > 2 ? "[" + strs.toString(", ") + "]" : strs.toString(", ");
    }

    public String getClassName() {
        Strings strs = new Strings();
        for (ConfigCell<?, ?> cell : cells)
            strs.add(cell.getClassName());
        return strs.size() > 2 ? "[" + strs.toString(", ") + "]" : strs.toString(", ");
    }

    public SELF onBeforeChange(Object context, BeforeChangeListener<SELF, RAW> listener) {
        onBeforeChange.listen(context, listener);
        return self;
    }

    public SELF onAfterChange(Object context, AfterChangeListener<SELF, RAW> listener) {
        onAfterChange.listen(context, listener);
        return self;
    }

    public SELF onGetValue(Object context, ValueGetListener<SELF, RAW, ROW> listener) {
        onGetValue.listen(context, listener);
        return self;
    }

    public void save() throws Exception {
        parent.save(this);
    }

    public ConfigStoreFactory<SELF, RAW, ROW> store() {
        return new ConfigStoreFactory(this);
    }

    public boolean hasVariable() {
        return !Is.empty(variable);
    }

    public void setVariable(ConfigField<?, ?, ?> field) {
        this.variable = field.getKey();
    }

    public void setVariable(String variable) {
        this.variable = variable;
    }

    public String variable() {
        return variable;
    }

    public String getDisplayValue(ValueSource source, boolean publicValue) {

        return getDisplayValue(getValue(source), publicValue);
    }

    public String getDisplayValue(RAW value, boolean publicValue) {

        if (hasVariable())
            return "var::" + variable;

        Strings strs = new Strings();
        for (ROW obj : asList(value))
            strs.add(getRowDisplayValue(obj, publicValue));

        if (!multiple)
            return strs.toString(", ");

        return "[" + strs.toString(", ") + "]";
    }

    protected String getRowDisplayValue(ROW val, boolean publicValue) {
        if (val == null)
            return LACK.toString();

        Strings strs = new Strings();

        for (Pair<ConfigCell<?, ?>, ROW> pair : getRowCellValues(val))
            strs.add(pair.first.getDisplayValue(pair.second, publicValue));

        if (multiple)
            return "[" + strs.toString() + "]";

        return strs.toString();
    }

    @Override
    public String toString() {
        return getKey();
    }

    private ValueSource currentSource() {
        return defaultState ? ValueSource.DEFAULT : ValueSource.USER;
    }

    public boolean isEmpty() {
        return isEmpty(currentSource());
    }

    public boolean isEmpty(ValueSource source) {
        RAW val = getValue(source);
        return val == null || (val instanceof Collection && ((Collection) val).isEmpty());
    }

    public boolean isMissing() {
        return required && isEmpty(currentSource()) && !hasVariable();
    }

    public boolean hasUserValue() {
        return !isEmpty(ValueSource.USER);
    }

    public boolean hasDefaultValue() {
        return !isEmpty(ValueSource.DEFAULT);
    }

    public JObject getStructure(boolean simple) {
        JObject json = new JObject();

        boolean hasDef = hasDefaultValue();

        json.put("name", name != null ? name.toString() : null);
        json.put("default", defaultState && (!required || hasDef));
        json.put("required", Utils.coalesce(required, !hasDef));
        json.put("disabled", !enabled());
        json.put("missing", isMissing());
        json.put("modified", hasVariable() || !defaultState);

        if (!Is.empty(description))
            json.put("description", description);

        if (simple) {
            json.put("displayValue", getDisplayValue(ValueSource.AUTO, true));
            return json;
        }

        ConfigStoreFactory<SELF, RAW, ROW> store = store();

        JArray usrArr = store.getValuesArray(ValueSource.USER);
        JArray defArr = store.getValuesArray(ValueSource.DEFAULT);

        json.put("userValue", store.isEmptyValue(usrArr) ? defArr : usrArr);
        json.put("defaultValue", store.isEmptyValue(defArr) ? null : defArr);
        json.put("defaultDisplayValue", getDisplayValue(ValueSource.DEFAULT, false));
        json.put("multiple", multiple);
        json.put("readOnly", readOnly);
        json.put("variable", variable);

        if (multiple) {
            json.put("minItems", minItems);
            json.put("maxItems", maxItems);
            json.put("unique", unique);
        }
        JArray jcells = json.arrayC("cells");

        for (ConfigCell<?, ?> c : cells)
            jcells.add(c.getStructure());

        if (variablesProvider != null) {
            Map<String, Pair<String, ConfigField<?, ?, ?>>> vars = variablesProvider.run();
            if (vars != null && !vars.isEmpty()) {
                JObject jVars = json.objectC("variables");
                for (Entry<String, Pair<String, ConfigField<?, ?, ?>>> en : vars.entrySet()) {
                    Pair<String, ConfigField<?, ?, ?>> p = en.getValue();
                    jVars.arrayC(en.getKey())
                            .add(p.first)
                            .add(p.second.getTypeName())
                            .add(p.second.getDisplayValue(null, true));
                }

            }
        }

        return json;
    }

    public SELF readOnly(boolean readOnly) {
        this.readOnly = readOnly;
        return self;
    }

    public boolean isReadOnly() {
        return readOnly;
    }

    public boolean isUnique() {
        return unique;
    }

    public boolean isDefaultState() {
        return defaultState;
    }

    public boolean isMultiple() {
        return multiple;
    }

    protected void _notifyChange(TObject<RAW> result) {
        onGetValue.dispatch(this, callable
                -> result.set(callable.onGet(self, result.get(), ValueSource.VARIABLE)));
    }

    public RAW getValue(ValueSource source) {

        if (source == null)
            source = ValueSource.AUTO;

        ConfigNode root = parent != null ? parent.getRoot() : null;

        if (source == ValueSource.VARIABLE || (source == ValueSource.AUTO && !Is.empty(variable))) {
            if (variableResolver == null)
                throw new NullPointerException("Missing variable resolver");
            ConfigField<? extends ConfigField<?, ?, ?>, ?, ?> val = variableResolver.run(variable);

            if (val.cells.length == cells.length) {
                boolean same = true;
                for (int i = 0; i < cells.length; i++)
                    same &= cells[i].cls == val.cells[i].cls;
                if (same) {
                    final TObject<RAW> result = new TObject<>((RAW) val.value());

                    onGetValue.dispatch(this, callable -> result.set(
                            callable.onGet(self, result.get(),
                                    ValueSource.VARIABLE)));

                    if (parent != null)
                        parent.onGetValue.dispatch(this, callable
                                -> result.set((RAW) callable.onGet(self, result.get(),
                                        ValueSource.VARIABLE)));

                    if (root != null && root != parent)
                        root.onGetValue.dispatch(this, callable
                                -> result.set((RAW) callable.onGet(self, result.get(),
                                        ValueSource.VARIABLE)));

                    return result.get();
                }
            }

//            try {
//                return parse(val.getValuesArray(null));
//            } catch (Error | RuntimeException e) {
//                throw e;
//            } catch (Exception ex) {
//                throw new ConfigException(ex);
//            }
        }

        if (source == ValueSource.AUTO)
            source = defaultState ? ValueSource.DEFAULT : ValueSource.USER;

        final TObject< RAW> result = new TObject<>(source == ValueSource.DEFAULT
                ? getDefaultValue()
                : userValue);

        final ValueSource _source = source;

        onGetValue.dispatch(this, callable -> result.set(
                callable.onGet(self, result.get(), _source)));

        if (parent != null)
            parent.onGetValue.dispatch(this, callable
                    -> result.set((RAW) callable.onGet(self, result.get(), _source)));

        if (root != null && root != parent)
            root.onGetValue.dispatch(this, callable
                    -> result.set((RAW) callable.onGet(self, result.get(), _source)));

        if (result.isNull() && this instanceof MultipleConfigFields)
            return (RAW) new TList<>();

        return result.get();
    }

    public RAW value(RAW def) {
        return Utils.coalesce(getValue(null), def);
    }

    public RAW value() {
        return getValue(null);
    }

    /**
     * Ustaw wartość użytkownika, oraz defaultState = false
     *
     * @param value
     * @return
     */
    public boolean setUserValueAsCurrent(RAW value) {
        boolean state = setUserValue(value, true);
        if (state)
            setDefaultState(false);
        return state;

        //  this.value()
    }

    public boolean setUserValue(RAW value) {
        return setUserValue(value, true);
    }

    protected boolean callOnChange(boolean before, boolean isUserValue, RAW oldValue, TObject<RAW> newValue) throws Exception {
        ConfigNode root = parent != null ? parent.getRoot() : null;

        if (before) {

            for (BeforeChangeListener<SELF, RAW> listener : onBeforeChange.getObservers())
                if (!listener.onChange(self, isUserValue, oldValue, newValue))
                    return false;

            if (parent != null)
                for (BeforeChangeListener<SELF, RAW> listener : parent.onBeforeChange.getObservers())
                    if (!listener.onChange(self, isUserValue, oldValue, newValue))
                        return false;

            if (root != null && root != parent)
                for (BeforeChangeListener<SELF, RAW> listener : root.onBeforeChange.getObservers())
                    if (!listener.onChange(self, isUserValue, oldValue, newValue))
                        return false;

        }
        if (!before) {

            onAfterChange.dispatch(self, callable
                    -> callable.onChange(self, isUserValue, newValue.get())
            );

            if (parent != null)
                parent.onAfterChange.dispatch(self, callable
                        -> callable.onChange(self, isUserValue, newValue.get())
                );

            if (root != null && root != parent)
                root.onAfterChange.dispatch(self, callable
                        -> callable.onChange(self, isUserValue, newValue.get())
                );

        }
        return true;
    }

    boolean setUserValue(RAW value, boolean validate) {
        try {
            if (validate)
                validate(value, true);
        } catch (Throwable e) {
            throw new ConfigException(getKey(), e);
        }

        variable = null;
        RAW oldValue = null;
        try {
            TObject<RAW> val = new TObject<>(value);
            if (!onBeforeChange.isEmpty()) {
                oldValue = this.userValue;
                if (!callOnChange(true, true, oldValue, val))
                    return false;
            }

            this.userValue = val.get();

            callOnChange(false, true, oldValue, val);

        } catch (RuntimeException | Error ex) {
            throw ex;
        } catch (Exception ex) {
            throw new ConfigException(ex);
        }

        return true;
    }

    public Pairs<ConfigCell<?, ?>, ROW> getRowCellValues(ROW value) {
        Pairs<ConfigCell<?, ?>, ROW> pairs = new Pairs<>();
        if (value == null)
            return pairs;

        if (cells.length == 1) {
            pairs.add(cells[0], value);
            return pairs;
        }

        int idx = 0;
        Iterator<ROW> itr = ((Iterable<ROW>) value).iterator();
        // Pair, Triple, Quad implementuja Iterable
        while (itr.hasNext()) {
            pairs.add(cells[idx], itr.next());
            ++idx;
        }
        return pairs;
    }

    public void validate(RAW value, boolean checkRequired) throws Exception {

        if (value == null && checkRequired && isRequired())
            throw new Error("Brak wymaganej wartości elementu \"" + getKey() + "\"");

        for (ROW row : asList(value)) {

            for (Pair<ConfigCell<?, ?>, ROW> pair : getRowCellValues(row))
                pair.first.validate_(pair.second);
            validators.dispatch(this, intf -> intf.validate(self, 1, row));
        }
    }

    public SELF setDefaultState(boolean defaultState) {
        this.defaultState = defaultState;
        return self;
    }

    public boolean isRequired() {
        return required;
    }

    public SELF required(boolean required) {
        this.required = required;
        return self;
    }

    public SELF setDefaultValue(Callable<RAW> provider) {
        this.defaultValue.set(provider);
        return self;
    }

    public SELF setDefaultValue(RAW value) {

        RAW oldValue = null;

        try {
            TObject<RAW> val = new TObject<>(value);
            if (!onBeforeChange.isEmpty()) {
                oldValue = this.getDefaultValue();
                if (!callOnChange(true, false, oldValue, val))
                    return self;
            }

            defaultValue.set(value);

            callOnChange(false, false, oldValue, val);

        } catch (RuntimeException | Error ex) {
            throw ex;
        } catch (Exception ex) {
            throw new ConfigException(ex);
        }

        return self;
    }

    public TList<ROW> asList(RAW value) {
        if (this instanceof MultipleConfigFields)
            return new TList<>((Collection<ROW>) value);
        if (this instanceof SingleConfigField)
            return new TList<>((ROW) value);
        throw new UnsupportedOperationException();
    }

    @Override
    public Iterator<ROW> iterator() {
        return asList(null).iterator();
    }

    @Override
    public SELF clone() throws CloneNotSupportedException {
        SELF clone = (SELF) super.clone();
        clone.onAfterChange.clear();
        clone.onBeforeChange.clear();
        return clone;
    }

    public RAW getDefaultValue() {
        return defaultValue.get();
    }

    public SELF extra(String key, Object value) {
        extra.put(key, value);
        return self;
    }

}
