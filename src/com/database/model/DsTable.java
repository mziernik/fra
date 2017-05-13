package com.database.model;

import com.database.*;
import com.database.queries.InsertOrUpdate;
import com.database.queries.MultipleQuery;
import com.database.queries.builder.QueryObject;
import com.dev.Dev;
import com.exceptions.SQLError;
import com.exceptions.ServiceException;
import com.exceptions.ThrowableException;
import com.intf.callable.Callable;
import com.json.Escape;
import com.model.dataset.AbstractDataSet;
import com.model.dataset.DsColumn;
import com.model.dataset.DsRecord;
import com.model.dataset.intf.DataSetException;
import com.utils.Is;
import com.utils.Ready;
import com.utils.collections.Pairs;
import com.utils.collections.TList;
import com.utils.reflections.TClass;
import com.utils.reflections.TypeAdapter;
import com.utils.text.StrWriter;
import java.lang.reflect.Method;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Map.Entry;

//ToDo: Dodać tryb lazy
public abstract class DsTable<SELF extends DsTable<SELF, PRIMARY_KEY>, PRIMARY_KEY>
        extends AbstractDataSet<SELF, QueryRow, PRIMARY_KEY> {

    final static Map<String, DsTable<?, ?>> tables1 = new LinkedHashMap<>();
    final static Map<Class<? extends DsTable>, DsTable<?, ?>> tables2 = new LinkedHashMap<>();

    protected final Pairs<Col<?>, Boolean> orderColumns = new Pairs<Col<?>, Boolean>(); // kolumna, ASC

    public final String tableName;
    private boolean master;
    protected boolean hasCompareMethod;

    @Override
    public String toString() {
        return (master ? "[M] " : "") + super.toString();
    }

//    public static <PK, TBL extends DsTable<?, PK>> TBL instance(Class<TBL> cls) {
//        TBL tbl = new TClass<>(cls).newInstance(null);
//
//        return tbl;
//    }
//
//    public static <PK, TBL extends DsTable<?, PK>> TBL instance(Class<TBL> cls, PK primaryKey) {
//        TBL tbl = new TClass<>(cls).newInstance(null);
//
//        return tbl;
//    }
    protected DsTable(String key, String tableName, CharSequence title, PRIMARY_KEY pk) {
        super(key, title);
        this.tableName = tableName;

        if (pk == null)
            return;

        SELF tbl = (SELF) tables2.get(getClass());
        if (tbl == null)
            throw new DataSetException(String.format("Table %s not found", tableName));

        synchronized (tbl.records) {
            record = tbl.records.get(pk);
        }
        if (record == null)
            throw new DataSetException(String.format("Item %s, %s not found",
                    name, Escape.escape(pk)));
    }

//    public static <PK, TBL extends DsTable<TBL, PK>> TBL getTable(Class<TBL> cls, PK key) {
//        TBL tbl = (TBL) tables1.get(cls);
//        return tbl.get(key);
//    }
    public static void registerRecords(Class<? extends DsTable<?, ?>>... classes) {
        for (Class<? extends DsTable<?, ?>> cls : classes)
            try {
                DsTable<?, ?> tbl = new TClass<>(cls).newInstance(null);
                tbl.master = true;
                tables1.put(tbl.key, tbl);
                tables2.put(tbl.getClass(), tbl);
                tbl.init();
            } catch (Throwable e) {
                throw new ThrowableException(e).details("Class", cls.getName());
            }
    }

    public static DsTable<?, ?> getTableF(String key) {
        DsTable<? extends DsTable<?, ?>, ?> tbl = getTable(key);
        if (tbl == null)
            throw new ServiceException("Table " + key + " not found");
        return tbl;
    }

    public static DsTable<?, ?> getTableF(Class<? extends DsTable<?, ?>> tableClass) {
        DsTable<? extends DsTable<?, ?>, ?> tbl = getTable(tableClass);
        if (tbl == null)
            throw new ServiceException("Table " + tableClass.getName() + " not found");
        return tbl;
    }

    public static DsTable<?, ?> getTable(Class<? extends DsTable<?, ?>> tableClass) {
        return (DsTable<?, ?>) tables2.get(tableClass);
    }

    public static DsTable<?, ?> getTable(String key) {
        return (DsTable<?, ?>) tables1.get(key);
    }

    public static Map<String, DsTable<?, ?>> getTables() {
        return new LinkedHashMap<>(tables1);
    }

    protected boolean isDbCol(DsColumn<?, SELF, QueryRow, ?> col) {
        return true;
    }

    public static void updateAll(Callable<Database> dbFactory) throws Exception {

        Database db = dbFactory.run();

        MultipleQuery queries = db.multipleQuery();

        for (AbstractDataSet<?, ?, ?> ds : tables1.values()) {
            DsTable<?, ?> tbl = (DsTable<?, ?>) ds;
            //DbRecord<?, ?> rec = tbl.recordClass.newInstance(null);

            MultipleQuery mqry = db.multipleQuery();
            tbl.getSelectQuery(mqry);

            if (!mqry.isEmpty())
                queries.add(DbUtils.addMarker(db, tbl)).add(mqry);

        }

        DbUtils.processMarkers(queries.execute(), tables1.values(), (rows, ds) -> {

            ds.fillRows(rows);
            Ready.confirm(ds.getClass());

        });
    }

    protected void getDeleteQuery(MultipleQuery mqry, DsRecord<SELF, QueryRow, ?> rec) {
        mqry.query("DELETE FROM " + tableName + " WHERE " + primaryKey.getDbColumnName() + " = ?", rec.pk);
    }

    protected void getSelectQuery(MultipleQuery mqry) {

        StrWriter sb = new StrWriter();
        for (DsColumn<?, SELF, QueryRow, ?> col : columns.values())
            if (isDbCol(col))
                sb.append(sb.length() == 0 ? "SELECT " : ", ")
                        .append(col.getDbColumnName());

        sb.append("\nFROM ")
                .append(tableName);

        if (!orderColumns.isEmpty())
            sb.append("\nORDER BY ")
                    .join(orderColumns, ", ", (p) -> p.first.getDbColumnName()
                    + (p.second ? " ASC" : " DESC"));
        mqry.query(sb.toString());
    }

    protected void getUpdateQuery(MultipleQuery mqry,
            Map<DsColumn<?, SELF, QueryRow, ?>, Object> cells) throws SQLError {

        Object pkVal = record == null ? null : primaryKey.get();

        InsertOrUpdate ins = mqry.insertOrUpdate(tableName,
                pkVal != null ? primaryKey.getDbColumnName() + " = ?" : null, pkVal);

        for (Map.Entry< DsColumn<?, SELF, QueryRow, ?>, Object> ve : cells.entrySet()) {
            DsColumn<? extends DsColumn<?, SELF, QueryRow, ?>, SELF, QueryRow, ?> col = ve.getKey();
            QueryObject obj = ins.put(col.getDbColumnName(), ve.getValue()).array(true);
            if (!Is.empty(col.getDbColumnCast()))
                obj.cast(col.getDbColumnCast());
        }
        for (DsColumn<?, SELF, QueryRow, ?> col : this.columns.values())
            if (isDbCol(col))
                ins.addReturningColumn(col.getDbColumnName());
    }

    protected <RAW> Col<RAW> column(CharSequence name) {
        return column(null, name);
    }

    protected <RAW> Col<RAW> column(Class<RAW> cls, CharSequence name) {
        Col<RAW> col = super.column(cls, null, name, null);
        return col.setter(row -> (RAW) row.getObj(col.getDbColumnName(), null));
    }

    protected <RAW, FK extends DsTable<?, ?>> ColF<RAW, FK> columnF(Class<RAW> cls, CharSequence name) {
        ColF<RAW, FK> col = super.columnF(cls, null, name, null);
        col.setter(row -> (RAW) row.getObj(col.getDbColumnName(), null));
        return col;
    }

    protected <RAW, FK extends DsTable<?, ?>> ColF<RAW, FK> columnF(CharSequence name) {
        return columnF(null, name);
    }

    void _removeRecord(DsRecord<?, QueryRow, ?> rec) {
        synchronized (records) {
            records.remove(rec.pk);
        }
    }

    Map<PRIMARY_KEY, DsRecord<SELF, QueryRow, PRIMARY_KEY>> _getRecords() {
        return records;
    }

    static DsTable edit(DsTable tbl, Object primaryKeyObj, Map<String, Object> values, DbRecordTransaction trans) {

        DsColumn primaryKey = tbl.getPrimaryKeyColumn();

        Object pk = primaryKeyObj == null ? null
                : new TypeAdapter<>(primaryKey.getRawClass())
                        .single(primaryKeyObj, null);

        tbl = (DsTable) (pk != null ? tbl.getByKeyF(pk) : tbl.newInstance());

        for (Entry<String, Object> en : values.entrySet()) {
            DsColumn col = (DsColumn) tbl.columns.get(en.getKey());
            if (col == null)
                throw new Error("Column \"" + en.getKey() + "\" not found");
            Object val = new TypeAdapter<>(col.getRawClass()).process(en.getValue());
            trans.set(col, val);
        }

        return tbl;
    }

    static DsTable remove(DsTable tbl, Object primaryKeyObj, DbRecordTransaction trans) {
        DsColumn primaryKey = tbl.getPrimaryKeyColumn();

        Object pk = primaryKeyObj == null ? null
                : new TypeAdapter<>(primaryKey.getRawClass())
                        .single(primaryKeyObj, null);

        tbl = (DsTable) tbl.getByKeyF(pk);
        trans.delete(tbl);
        return tbl;
    }

    void apply(SELF tbl, Map<DsColumn<?, ? extends DsTable<?, ?>, QueryRow, ?>, Object> map) {
        assert master;

        PRIMARY_KEY pk = new TList<>(tbl.records.keySet()).first();
        DsRecord<SELF, QueryRow, PRIMARY_KEY> tmpRec = new TList<>(tbl.records.values()).first();

        DsRecord<SELF, QueryRow, PRIMARY_KEY> rec = records.get(pk);
        if (rec == null)
            rec = addRow(pk, tmpRec.data);

        synchronized (records) {
            for (Entry<DsColumn<?, ? extends DsTable<?, ?>, QueryRow, ?>, Object> e : map.entrySet()) {
                DsColumn col = e.getKey();
                rec.data[col.getIndex()] = e.getValue();
            }
        }
    }

    protected int compare(SELF other) {
        return 0;
    }

    public void sort() {
        if (!hasCompareMethod)
            return;

        TList<DsRecord<SELF, QueryRow, PRIMARY_KEY>> values = new TList<>(records.values());
        SELF t1 = newInstance();
        SELF t2 = newInstance();

        values.sort((o1, o2) -> {
            t1.record = o1;
            t2.record = o2;
            return t1.compare(t2);
        });

        synchronized (records) {
            records.clear();
            for (DsRecord<SELF, QueryRow, PRIMARY_KEY> rec : values)
                records.put(rec.pk, rec);

        }

    }

    @Override
    protected SELF newInstance() {
        SELF self = super.newInstance();
        self.hasCompareMethod = this.hasCompareMethod;
        return self;
    }

    @Override
    protected void init() {
        if (initialized)
            return;

        super.init();
        try {
            Method m = getClass().getDeclaredMethod("compare", DsTable.class);
            hasCompareMethod = m != null;
        } catch (Throwable ex) {
        }

        if (!hasCompareMethod && !orderColumns.isEmpty())
            Dev.warning("Klasa " + getClass().getName() + " posiada zdefiniowane "
                    + "kolumny sortowania ale metoda compare(DsTable) nie jest przeciążona");
    }

}
