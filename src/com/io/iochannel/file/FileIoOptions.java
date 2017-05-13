package com.io.iochannel.file;

import com.io.iochannel.intfs.Options;

public class FileIoOptions extends Options {

    boolean readOnly = false;

    public boolean isReadOnly() {
        return readOnly;
    }

    public void setReadOnly(boolean readOnly) {
        this.readOnly = readOnly;
    }
}
