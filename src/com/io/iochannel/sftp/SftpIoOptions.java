package com.io.iochannel.sftp;

import com.io.iochannel.intfs.Options;

public class SftpIoOptions extends Options {

    boolean readOnly = false;

    public boolean isReadOnly() {
        return readOnly;
    }

    public void setReadOnly(boolean readOnly) {
        this.readOnly = readOnly;
    }

}
