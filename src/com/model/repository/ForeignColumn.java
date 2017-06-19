package com.model.repository;

import com.intf.callable.Callable;
import com.intf.runnable.Runnable1;

public class ForeignColumn<RAW, REPO extends Repository<?>> extends Column<RAW> {

    public ForeignColumn(Runnable1<RepoFieldConfig> cfg, Column<RAW> column) {
        super(cfg);
    }

}
