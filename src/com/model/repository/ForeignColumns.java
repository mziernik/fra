package com.model.repository;

import com.intf.runnable.Runnable1;
import java.util.Objects;

public class ForeignColumns<RAW, REPO extends Repository<?>> extends Column<RAW[]> {

    public final Column<RAW> column;

    public ForeignColumns(Runnable1<RepoFieldConfig> cfg, Column<RAW> column) {
        super(cfg);
        this.column = Objects.requireNonNull(column);
    }

}
