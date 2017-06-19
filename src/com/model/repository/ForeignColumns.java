package com.model.repository;

import com.intf.runnable.Runnable1;

public class ForeignColumns<RAW, REPO extends Repository<?>> extends Column<RAW[]> {

    public ForeignColumns(Runnable1<RepoFieldConfig> cfg, Column<RAW> column) {
        super(cfg);
    }

}
