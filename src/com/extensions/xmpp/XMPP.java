package com.extensions.xmpp;

import com.lang.LExtensions;
import com.servlet.pooling.ConnectionPool;
import com.servlet.pooling.SingleConnection;
import com.utils.WaitFor;
import com.utils.collections.Strings;
import com.utils.date.time.Interval;
import com.utils.date.time.Unit;
import org.jivesoftware.smack.*;

/**
 * Klient XMPP - jabber
 *
 * @author user
 */
public class XMPP {

    public static class XmppCon extends SingleConnection<XMPPConnection, XMPP, XmppConnConfig, XMPPException> {

        public XmppCon(ConnectionPool pool, XMPP sender, XMPPConnection connection,
                XmppConnConfig data, int lockTimeout, String connectionName) {
            super(pool, sender, connection, data, lockTimeout, connectionName);
        }

        @Override
        public boolean isSame(XMPP sender, XmppConnConfig data) {
            return true;
        }

        @Override
        protected void doClose() throws XMPPException {
            connection.disconnect();
        }

    }

    public static class XmppConection extends ConnectionPool<XmppCon, XMPP, XmppConnConfig, XMPPException> {

        public XmppConection() {
            super("XMPP (Jabber)", 30_000, 1, 100);
        }

        @Override
        protected XmppCon createConnection(XMPP sender, final XmppConnConfig data, int lockTimeout) throws XMPPException {

            final XMPPConnection conn = new XMPPConnection(data);

            conn.connect();

            if (!conn.isConnected())
                new WaitFor(LExtensions.XMPP_CONNECTION.toString(),
                        new Interval(5, Unit.SECONDS),
                        () -> conn.isConnected());

            conn.login(data.username, data.password);
            return new XmppCon(this, sender, conn, data, lockTimeout, data.toString());
        }

    }

    public static interface XmppListener extends MessageListener {

    }

    public final static XmppConection connection = new XmppConection();

    final XmppConnConfig config;

    public XMPP(XmppConnConfig config) {
        this.config = config;
    }

    public void message(String recipient, String message, XmppListener listener) throws XMPPException {
        message(new Strings(recipient), message, listener);
    }

    public void message(Iterable<String> recipients, String message, XmppListener listener) throws XMPPException {

        XmppCon lock = connection.lock(this, config, 10000, message);
        try {

            ChatManager chat = lock.connection.getChatManager();
            try {
                for (String recipient : recipients)
                    chat.createChat(recipient, listener).sendMessage(message);

            } catch (Throwable e) {
                throw new XMPPException(e);
            }

        } finally {
            lock.unlock();
        }
    }

}
