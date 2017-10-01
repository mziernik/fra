package com.model.repository.intf;

import com.model.repository.*;

public interface IForeignColumn<RAW> {

    public Column<RAW> getForeignColumn();

}
