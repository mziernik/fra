package com.model.dataset;

public class DsRecord<DS extends AbstractDataSet<DS, DATA, PRIMARY_KEY>, DATA, PRIMARY_KEY> {

    public final DS dataSet;
    public final PRIMARY_KEY pk;
    public final Object[] data;

    DsRecord(DS dataSet, PRIMARY_KEY pk, Object[] data) {

        if (data == null)
            data = new Object[dataSet.columns.size()];

        this.dataSet = dataSet;
        this.pk = pk;
        this.data = data;
    }

}
