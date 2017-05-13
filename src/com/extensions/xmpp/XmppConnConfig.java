package com.extensions.xmpp;

import org.jivesoftware.smack.ConnectionConfiguration;

public class XmppConnConfig extends ConnectionConfiguration {

    public String username;
    public String password;

    public XmppConnConfig(String host, int port) {
        super(host, port);
    }

    @Override
    public String toString() {
        return "xmpp://" + getHost();
    }

}
