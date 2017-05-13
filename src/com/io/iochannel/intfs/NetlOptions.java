package com.io.iochannel.intfs;

import java.net.Proxy;

public class NetlOptions extends Options {

    private int connectTimeout = 2000;
    private Proxy proxy = Proxy.NO_PROXY;
    private String proxyUsername;
    private String proxyPassword;

    public void setConnectTimeout(int connectTimeout) {
        this.connectTimeout = connectTimeout;
    }

    public int getConnectTimeout() {
        return connectTimeout;
    }
}
