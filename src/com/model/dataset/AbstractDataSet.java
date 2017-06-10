package com.model.dataset;

import com.database.model.Repository;
import com.events.Dispatcher;
import com.exceptions.ServiceException;
import com.exceptions.ThrowableException;
import com.intf.callable.CallableEx1;
import com.intf.runnable.RunnableEx;
import com.intf.runnable.RunnableEx1;
import com.intf.runnable.RunnableEx2;
import com.json.Escape;
import com.json.JArray;
import com.json.JObject;
import com.model.dataset.intf.DataSetException;
import com.utils.Lock;
import com.utils.collections.TList;
import com.utils.reflections.DataType;
import com.utils.reflections.TClass;
import com.utils.text.StrWriter;
import java.util.Map.Entry;
import java.util.*;

public abstract class AbstractDataSet<SELF extends AbstractDataSet<SELF, DATA, PRIMARY_KEY>, DATA, PRIMARY_KEY>
        implements Iterable<SELF> {

    protected final Map<PRIMARY_KEY, DsRecord<SELF, DATA, PRIMARY_KEY>> records = new LinkedHashMap<>();
    protected final Map<String, DsColumn<? extends DsColumn, SELF, DATA, ?>> columns = new LinkedHashMap<>();

    TList<DsColumn<? extends DsColumn, SELF, DATA, ?>> _columns = new TList<>(); // tymczasowa lita kolumn (na potrzeby procesu inicjalizacji)

    protected DsRecord<SELF, DATA, PRIMARY_KEY> record; // bieżące dane (na potrzeby metody getMap())

    protected DsColumn<? extends DsColumn, SELF, DATA, PRIMARY_KEY> primaryKey;
    // W tym trybie nie możemy dodawać kolumn
    protected boolean immutable;

    private final SELF self = (SELF) this;
    private final SELF parent = (SELF) this;

    public final String key; // nazwa tabeli
    public final CharSequence name;
    private final Lock lock = new Lock();

    public final Dispatcher<RunnableEx> onInit = new Dispatcher<>();
    public final Dispatcher<RunnableEx2<AbstractDataSet<SELF, DATA, PRIMARY_KEY>, JObject>> onChange = new Dispatcher<>();

    protected AbstractDataSet(String key, CharSequence name) {
        this.key = key;
        this.name = name;
    }

    protected <RAW> RAW onAfterSet(DsColumn<?, SELF, DATA, RAW> col, RAW value) {
        return value;
    }

    /**
     * Zwraca pary: klucz główny - rekord
     *
     * @return
     */
    public LinkedHashMap<PRIMARY_KEY, SELF> asMap() {
        LinkedHashMap<PRIMARY_KEY, SELF> map = new LinkedHashMap<>();
        synchronized (AbstractDataSet.this.records) {
            for (Entry<PRIMARY_KEY, DsRecord<SELF, DATA, PRIMARY_KEY>> en : records.entrySet()) {
                SELF ds = newInstance();
                ds.record = en.getValue();
                map.put(en.getKey(), ds);
            }
        }
        return map;
    }

    public DsColumn<? extends DsColumn, SELF, DATA, PRIMARY_KEY> getPrimaryKeyColumn() {
        return primaryKey;
    }

    public int size() {
        return records.size();
    }

    public DsRecord<SELF, DATA, PRIMARY_KEY> getRecord() {
        return record;
    }

    public TList<SELF> asList() {
        return new TList<>(asMap().values());
    }

    @Override
    public String toString() {
        StrWriter writer = new StrWriter();
        writer.append(key).append(": ");
        boolean first = true;
        for (DsColumn<? extends DsColumn, SELF, DATA, ?> col : columns.values()) {
            if (!first)
                writer.append(", ");
            first = false;
            writer.append(col.key);
            if (primaryKey == col)
                writer.append("[PK]");
            if (record != null)
                writer.append(" = ").escape(col.get());
        }
        return writer.toString();
    }

    public Map<String, DsColumn<? extends DsColumn, SELF, DATA, ?>> getColumns() {
        synchronized (columns) {
            return new HashMap<>(columns);
        }
    }

    public SELF fillRow(DATA data) {
        try {
            PRIMARY_KEY pk = primaryKey.setter.run(data);
            TList<Object> values = new TList<>();
            for (DsColumn<? extends DsColumn, SELF, DATA, ?> col : columns.values())
                values.add(col.setter != null ? col.setter.run(data) : null);
            addRow(pk, values.toArray(new Object[0]));
            return self;
        } catch (Exception ex) {
            throw new DataSetException("Fill rows of " + key, ex);
        }
    }

    public SELF fillRows(Iterable<DATA> data) {
        for (DATA d : data)
            fillRow(d);
        return self;
    }

    @Override
    public Iterator<SELF> iterator() {
        return new DataSetIterator();
    }

//    public <DS, B, C> static DS of(DsRecord<DS, B, C> aa){
//        return null;
//    }
//    
    public SELF clear() {
        records.clear();
        return self;
    }

    public DsRecord<SELF, DATA, PRIMARY_KEY> addRow(PRIMARY_KEY pk, Object... data) {
        if (pk == null)
            throw new NullPointerException(String.format("Primary key of %s cannot be null", key));
        if (records.containsValue(pk))
            throw new ServiceException(String.format("%s = %s already exists", key, Escape.escape(pk)));

        DsRecord<SELF, DATA, PRIMARY_KEY> rec = new DsRecord<>(self, pk, data);
        records.put(pk, rec);
        return rec;
    }

    /**
     * Zwraca nową instancję DataSet, kolumn oraz aktualizuje referencje do
     * elementów dodanych dynamicznie
     *
     * @return
     */
    protected SELF newInstance() {
        SELF ds = (SELF) new TClass<>(AbstractDataSet.this.getClass()).newInstance(null, key, name);
        ds.init();
        return ds;
    }

    public SELF getByKeyF(PRIMARY_KEY key) {
        SELF self = getByKey(key);
        if (self == null)
            throw new DataSetException("Record " + this.key + "." + primaryKey.key
                    + " = " + Escape.escape(key) + " not found");
        return self;
    }

    public SELF getByKey(PRIMARY_KEY key) {
        if (key == null)
            return null;
        DsRecord<SELF, DATA, PRIMARY_KEY> rec = records.get(key);
        if (rec == null)
            return null;

        SELF ds = newInstance();
        ds.record = rec;
        ds.records.put(rec.pk, rec);
        return ds;
    }

//    public SELF getByKey(PRIMARY_KEY key) {
//        if (key == null)
//            return null;
//        
//        
//        return lock.readR(() -> {
//            DsRecord<SELF, DATA, PRIMARY_KEY> rec = records.values()
//                    .parallelStream()
//                    .filter((t) -> {
//                        return key.equals(t.pk);
//                    })
//                    .findFirst()
//                    .get();
//
//            if (rec == null)
//                return null;
//
//            SELF ds = newInstance();
//            ds.record = rec;
//            return ds;
//        });
//    }
    protected boolean initialized;

    protected void init() {
        try {
            if (initialized)
                return;
            initialized = true;
            Objects.requireNonNull(primaryKey, "PrimaryKey of " + getClass().getName());
            DsMetaData.load(this);

            onInit.dispatch(this, intf -> intf.run());

        } catch (Throwable e) {
            throw new ThrowableException(e);
        }
    }

    protected void initCol(DsColumn<?, SELF, DATA, ?> col) {
        col.init();
    }

    public JObject getJson() {
        return getJson(new JObject());
    }

    public JObject getJson(JObject json) {

        json.put("key", key);
        json.put("name", name);
        json.put("pk", primaryKey != null ? primaryKey.key : null);

//        json.put("results", results);
//        json.put("offset", offset);
//        json.put("limit", limit);
//        json.put("updatable", updatable);
//        json.put("selectable", selectable);
//        json.put("sortable", sortable);
        JArray cols = json.arrayC("columns");
        for (DsColumn<? extends DsColumn, SELF, DATA, ?> col : columns.values())
            cols.add(col.getJson());

        JArray jRows = json.arrayC("rows");

        for (DsRecord<SELF, DATA, PRIMARY_KEY> rec : records.values())
            jRows.array().addAll(rec.data).options.compactMode(true);

        return json;
    }

    protected <RAW> Col<RAW> column(Class<RAW> cls,
            DataType<? extends RAW> type, CharSequence name,
            CallableEx1<RAW, DATA> setter) {
        return new Col<>(cls, null, type, name, setter);
    }

    protected <RAW> Col<RAW> column(String key, DataType<? extends RAW> type,
            CharSequence name, CallableEx1<RAW, DATA> setter) {
        return new Col<>(null, key, type, name, setter);
    }

    protected <RAW> Col<RAW> column(Class<RAW> cls, String key, DataType<? extends RAW> type,
            CharSequence name, CallableEx1<RAW, DATA> setter) {
        return new Col<>(cls, key, type, name, setter);
    }

    protected <RAW, DS extends Repository<?, ?>> ColF<RAW, DS> columnF(
            DataType<? extends RAW> type, CharSequence name, CallableEx1<RAW, DATA> setter) {
        return new ColF<>(null, type, name, setter);
    }

    protected <RAW, DS extends Repository<?, ?>> ColF<RAW, DS> columnF(Class<RAW> cls,
            DataType<? extends RAW> type, CharSequence name, CallableEx1<RAW, DATA> setter) {
        return new ColF<>(cls, type, name, setter);
    }

    protected <RAW, DS extends Repository<?, ?>> ColF<RAW, DS> columnF(Class<RAW> cls,
            String key, DataType<? extends RAW> type, CharSequence name, CallableEx1<RAW, DATA> setter) {
        return new ColF<>(cls, key, type, name, setter);
    }

    public class Col<RAW> extends DsColumn<Col<RAW>, SELF, DATA, RAW> {

        Col(Class<RAW> cls, DataType<? extends RAW> type, CharSequence name, CallableEx1<RAW, DATA> setter) {
            this(cls, null, type, name, setter);
        }

        Col(Class<RAW> cls, String key, DataType<? extends RAW> type, CharSequence name, CallableEx1<RAW, DATA> setter) {
            super(self, key, type, name, setter);
            this.clazz = cls;
        }
    }

    public class ColF<RAW, DS extends Repository<?, ?>> extends Col<RAW> {

        ColF(Class<RAW> cls, DataType<? extends RAW> type, CharSequence name, CallableEx1<RAW, DATA> setter) {
            super(cls, type, name, setter);
        }

        ColF(Class<RAW> cls, String key, DataType<? extends RAW> type, CharSequence name, CallableEx1<RAW, DATA> setter) {
            super(cls, key, type, name, setter);
        }

    }

    public class DataSetIterator implements Iterator<SELF> {

        private final Iterator<DsRecord<SELF, DATA, PRIMARY_KEY>> itr;
        private final SELF self = newInstance();

        public DataSetIterator() {
            synchronized (records) {
                itr = new TList<>(records.values()).iterator();
            }
        }

        @Override
        public boolean hasNext() {
            return itr.hasNext();
        }

        @Override
        public SELF next() {
            self.record = itr.next();
            return self;
        }

    }

}
