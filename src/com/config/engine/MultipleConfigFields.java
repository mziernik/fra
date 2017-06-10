package com.config.engine;

import com.utils.TObject;
import com.utils.collections.Pair;
import java.util.*;
import com.utils.collections.TList;

public class MultipleConfigFields<SELF extends MultipleConfigFields<SELF, RAW>, RAW>
        extends ConfigField<SELF, Collection<RAW>, RAW> {

    protected MultipleConfigFields(String key, CharSequence name, Collection<RAW> defaultValues, ConfigCell<?, ?>... cells) {
        super(key, name, defaultValues, cells);
        if (userValue == null)
            userValue = new TList<>();
        if (defaultValue.isNull())
            defaultValue.set(new TList<>());
        required(false);
    }

    protected MultipleConfigFields(String key, CharSequence name, RAW[] defaultValues, ConfigCell<?, ?>... cells) {
        super(key, name, Arrays.asList(defaultValues), cells);
        if (userValue == null)
            userValue = new TList<>();
        if (defaultValue.isNull())
            defaultValue.set(new TList<>());
        required(false);
    }

    @Override
    public TList<RAW> value() {
        return new TList<>(super.value());
    }

    public boolean addValue(RAW value) {
        if (this.userValue == null)
            this.userValue = new TList<>();

        Collection<RAW> oldValue = null, newValue = null;

        try {

            oldValue = new TList<>(this.userValue);
            newValue = new TList<>(this.userValue);
            newValue.add(value);

            TObject<Collection<RAW>> val = new TObject<>(newValue);

            if (!callOnChange(true, true, oldValue, val))
                return false;

            this.userValue.add(value);
            callOnChange(false, true, oldValue, val);

        } catch (RuntimeException | Error ex) {
            throw ex;
        } catch (Exception ex) {
            throw new ConfigException(ex);
        }

        return true;
    }

    public boolean addDefaultValue(RAW value) {
        if (defaultValue.isNull())
            defaultValue.set(new TList<>());

        try {

            TObject<Collection<RAW>> val = new TObject<>((Collection) value);
            Collection<RAW> oldValue = this.defaultValue.get();
            if (!onBeforeChange.isEmpty())
                if (!callOnChange(true, false, oldValue, val))
                    return false;

            defaultValue.get().add(value);

            callOnChange(false, false, oldValue, val);

        } catch (RuntimeException | Error ex) {
            throw ex;
        } catch (Exception ex) {
            throw new ConfigException(ex);
        }

        return true;
    }

    public SELF setDefaultValues(RAW... values) {
        return setDefaultValue(Arrays.asList(values));
    }
//
//    @Override
//    public Self setUserValue(Collection<Raw> value, boolean validate) {
//        try {
//            validate(value, true);
//        } catch (Throwable e) {
//            throw new ConfigException(key, e);
//        }
//        return super.setUserValue(value, validate);
//    }

    public SELF clear(boolean defaults) {
        if (defaults)
            defaultValue.clear();
        else
            userValue.clear();
        return self;
    }

    @Override
    public void validate(Collection<RAW> value, boolean checkRequired) throws Exception {
        if (checkRequired && isRequired() && (value == null || value.isEmpty()))
            throw new Error("Brak wymaganej wartości elementu " + getKey());

        if (value == null)
            return;

        for (RAW val : value)
            for (Pair<ConfigCell<?, ?>, RAW> pair : getRowCellValues(val))
                pair.first.validate_(pair.second);

        validators.dispatch(this, intf -> {
            for (RAW val : value)
                intf.validate(self, 1, val);
        });
    }

}
