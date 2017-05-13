package com.model.dataset;

import com.database.model.DsTable;
import com.intf.callable.CallableEx1;

public class DataSet<DATA, PRIMARY_KEY> extends AbstractDataSet<DataSet<DATA, PRIMARY_KEY>, DATA, PRIMARY_KEY> {

    public Integer offset;
    public Integer limit;
    public Integer results;

    public DataSet(String name, CharSequence title) {
        super(name, title);
    }

    @Override
    public <RAW> Col<RAW> column(Class<RAW> cls, String key, CharSequence name, CallableEx1<RAW, DATA> setter) {
        return super.column(cls, key, name, setter);

    }

    @Override
    public <RAW, DS extends DsTable<?, ?>> ColF<RAW, DS> columnF(Class<RAW> cls, String key, CharSequence name, CallableEx1<RAW, DATA> setter) {
        return super.columnF(cls, key, name, setter);
    }

}
