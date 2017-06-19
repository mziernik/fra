package com.model.repository;

import com.intf.callable.Callable1;
import com.intf.runnable.Runnable1;

import com.model.repository.Column.RepoFieldConfig;
import com.utils.reflections.datatype.DataType;
import java.util.Collection;

public class DynamicRepo<SOURCE, PK> extends Repository<PK> {

    public Integer offset;
    public int limit;

    public DynamicRepo(String key, CharSequence name) {
        super((cfg) -> {
            cfg.key = key;
            cfg.name = name;
        });
    }

    public DynamicRepo(Runnable1<RepoConfig> cfg) {
        super(cfg);
    }

    public <RAW> DynamicColumn<RAW> column(Class<RAW> clazz, Runnable1<RepoFieldConfig> cfg, Callable1<RAW, SOURCE> mapper) {
        DynamicColumn<RAW> column = new DynamicColumn<>((arg) -> {
            cfg.run(arg);
        }, mapper);
        return column;
    }

    public <RAW> DynamicColumn<RAW> column(Class<RAW> clazz, String key,
            DataType<RAW> type, CharSequence name, Callable1<RAW, SOURCE> mapper) {
        DynamicColumn<RAW> column = new DynamicColumn<>((cfg) -> {
            cfg.key = key;
            cfg.name = name;
            cfg.type = type;
        }, mapper);
        return column;
    }

    public void fillRows(Iterable<SOURCE> values) {

    }

    public void fillRow(SOURCE value) {

    }

    public class DynamicColumn<RAW> extends Column<RAW> {

        public DynamicColumn(Runnable1<RepoFieldConfig> cfg, Callable1<RAW, SOURCE> mapper) {
            super(cfg);
        }

        public DynamicColumn<RAW> primaryKey() {
        return this;
        }

    }

}
