package com.model;

import com.model.dataset.DsColumn;

public class DTO {

    public class Col<RAW> {

        private final DsColumn<?, ?, ?, RAW> dsCol;

        public Col(DsColumn<?, ?, ?, RAW> dsCol) {
            this.dsCol = dsCol;
        }

    }

    protected <RAW> Col<RAW> col(DsColumn<?, ?, ?, RAW> dsCol) {
        return new Col<>(dsCol);
    }

}
