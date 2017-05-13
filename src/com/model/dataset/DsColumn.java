package com.model.dataset;

import com.database.model.DbCol;
import com.model.dataset.intf.DataType;
import com.exceptions.ThrowableException;
import com.intf.callable.CallableEx1;
import com.json.Escape;
import com.json.JObject;
import com.lang.core.LStr;
import com.model.dataset.intf.DSColumnAlign;
import com.model.dataset.intf.DataSetException;
import com.utils.Is;
import com.utils.Str;
import com.utils.collections.Props;
import com.utils.text.NameFormat;
import java.lang.reflect.Field;
import java.lang.reflect.Type;
import java.util.Objects;

public abstract class DsColumn<SELF extends DsColumn<SELF, DS, DATA, RAW>, //
        DS extends AbstractDataSet<DS, DATA, ?>, DATA, RAW> {

    protected CallableEx1<RAW, DATA> setter;
    String key;
    CharSequence name;
    private final SELF self = (SELF) this;
    DS parent;
    public final Props extra = new Props();
    protected String dbColumnName;
    protected String dbColumnCast;
//---------------------------

    private Boolean primaryKey;
    private Boolean sortOrder; // true - rosnący, false malejący
    private Boolean unique;
    private Boolean nullable;
    private DSColumnAlign align;

    private CharSequence subtitle;
    private DataType type;
    private String foreignDataSet;
    private String foreignColumn;
    private Boolean disabled; // kolumna niewidoczna, zawiera daodatkowe dane, nie można jej wyświetliś
    private Boolean hidden; // kolumna niewidoczna domyslnie (można wyświetlić z menu kontekstowego)
    private Boolean sortable; // można sortować
    private Boolean editable;
    private Boolean filtered; // można filtrowac po tej kolumnie

    private String dateFormat;
    private String[] dateFormats;
    private Boolean searchable;

    private boolean defined = false;
    private boolean initialized = false;

    int index = -1; // indeks elementu w DsRecord (w tablicy)

    Field field;
    Class<?> clazz;
    Class<? extends AbstractDataSet<?, ?, ?>> foreignClass;

    public Class<?> getRawClass() {
        return clazz;
    }

    public int getIndex() {
        return index;
    }

    public DS getParent() {
        return parent;
    }

    public String getKey() {
        return key;
    }

    public CharSequence getName() {
        return name;
    }

    public Field getField() {
        return field;
    }

    public String getDbColumnCast() {
        return dbColumnCast;
    }

    public String getDbColumnName() {
        return dbColumnName;
    }

//    public SELF dbColumnName(String columnName) {
//        this.dbColumnName = columnName;
//        return (SELF) this;
//    }
    @Override
    public String toString() {
        return key + (parent.record != null ? " = " + Escape.escape(get()) : "");
    }

    public DsColumn(DS parent, String key, CharSequence name, CallableEx1<RAW, DATA> setter) {

        this.key = key;
        this.name = name;
        this.parent = parent;
        this.setter = setter;

        if (parent.immutable)
            throw new DataSetException("DataSet is immutable");

        if (!parent.records.isEmpty())
            throw new DataSetException(String.format("Cannot add column %s until dataSet %s is not emty", key, this.key));

        parent._columns.add(this);
    }

    public SELF primaryKey() {
        parent.primaryKey = (DsColumn) this;
        return self;
    }

    public SELF setter(CallableEx1<RAW, DATA> setter) {
        this.setter = setter;
        return self;
    }

    public SELF clear() {
        defined = false;
        return self;
    }

    /**
     * Czy wartość została zdefiniowana (zmodyfikowana)
     *
     * @return
     */
    public boolean isDefined() {
        return defined;
    }

    public SELF copy(DS parent) {
        try {
            SELF col = (SELF) this.clone();
            col.parent = parent;
            return col;
        } catch (CloneNotSupportedException ex) {
            throw new ThrowableException(ex);
        }
    }

    public RAW get() {
        checkInit(true);

        if (parent.record == null)
            throw new DataSetException("Not iterable");

        if (index < 0 || index >= parent.record.data.length)
            throw new DataSetException(String.format("Incorrect column index (%d) on %s",
                    index, parent.key + "." + key));

        return (RAW) parent.record.data[index];
    }

    public JObject getJson() {
        JObject json = new JObject(key);
        json.options.singleLine(true);
        json.options.acceptNulls(false);

        json.put("primaryKey", primaryKey);
        json.put("hidden", hidden);
        json.put("unique", unique);
        json.put("caption", name);
        json.put("subtitle", subtitle);
        json.put("sortOrder", sortOrder);
        json.put("type", type != null ? type.name().toLowerCase() : null);
        json.put("align", align != null ? align.key : null);
        json.put("editable", editable);
        json.put("searchable", searchable);
        json.put("filtered", filtered);
        json.put("disabled", disabled);
        json.put("nullable", nullable);
        json.put("sortable", sortable);
        if (type == DataType.DATE)
            json.arrayC("dateFormat").add(dateFormat).add(dateFormats);

        return json;
    }

    public SELF hidden(boolean hidden) {
        this.hidden = hidden;
        return self;
    }

    public SELF unique(boolean unique) {
        this.unique = unique;
        return self;
    }

    public SELF align(DSColumnAlign align) {
        this.align = align;
        return self;
    }

    public SELF caption(String caption) {
        this.name = new LStr(caption);
        return self;
    }

    public SELF subtitle(CharSequence subtitle) {
        this.subtitle = subtitle;
        return self;
    }

    public SELF sortOrder(Boolean sortOrder) {
        this.sortOrder = sortOrder;
        return self;
    }

    public SELF dateFormat(String format, String... formats) {
        this.dateFormat = format;
        this.dateFormats = formats;
        return self;
    }

    public SELF type(DataType type) {
        this.type = type;
        return self;
    }

    public SELF editable(boolean editable) {
        this.editable = editable;
        return self;
    }

    public SELF searchable(boolean searchable) {
        this.searchable = searchable;
        return self;
    }

    public SELF filtered(boolean filtered) {
        this.filtered = filtered;
        return self;
    }

    public SELF disabled(boolean disabled) {
        this.disabled = disabled;
        return self;
    }

    public SELF nullable(Boolean nullable) {
        this.nullable = nullable;
        return self;
    }

    public SELF sortable(boolean sortable) {
        this.sortable = sortable;
        return self;
    }

    private void checkInit(boolean state) {
        if (initialized == state)
            return;
        if (!initialized)
            throw new DataSetException(String.format("Column %s.%s is not initialized", parent.key, key));
        throw new DataSetException(String.format("Column %s.%s has been initialized", parent.key, key));
    }

    void init() {
        if (initialized)
            return;
        if (Is.empty(key))
            key = new Str(field.getName())
                    .removePrefix("_")
                    .removeSufix("_")
                    .toString();

        if (parent.columns.containsValue(key))
            throw new DataSetException(String.format("Column %s.%s already exists", parent.key, key));

        index = parent.columns.size();
        parent.columns.put(key, this);

        dbColumnName = NameFormat.camelCaseToUnderscore(key);

        DbCol dbCol = field != null ? field.getAnnotation(DbCol.class) : null;
        if (dbCol != null) {
            dbColumnCast = dbCol.cast();
            if (!Is.empty(dbCol.name()))
                dbColumnName = dbCol.name();
        }
        Objects.requireNonNull(key, "Missing column key in " + parent.key);
        Objects.requireNonNull(clazz, "Missing generic type of " + parent.key + "." + key);

        initialized = true;
    }

}
