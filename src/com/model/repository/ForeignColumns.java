package com.model.repository;

import com.intf.runnable.Runnable1;
import com.model.repository.intf.IForeignColumn;
import java.util.Objects;

public class ForeignColumns<RAW, REPO extends Repository<?>>
        extends Column<RAW[]> implements IForeignColumn<RAW> {

    private final Column<RAW> column;

    public ForeignColumns(Runnable1<RepoFieldConfig> cfg, Column<RAW> column) {
        super(cfg);
        this.column = Objects.requireNonNull(column);
    }

    @Override
    public Column<RAW> getForeignColumn() {
        return column;
    }

}
