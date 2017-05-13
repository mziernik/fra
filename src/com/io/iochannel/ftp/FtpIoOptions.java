package com.io.iochannel.ftp;

import com.io.iochannel.intfs.Options;

public class FtpIoOptions extends Options {

    boolean readOnly = false;

    public boolean isReadOnly() {
        return readOnly;
    }

    public void setReadOnly(boolean readOnly) {
        this.readOnly = readOnly;
    }

}
