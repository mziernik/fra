package com.config;

import com.config.engine.ConfigNode;
import com.config.engine.field.*;
import com.utils.Str;
import com.utils.hashes.Base64;
import java.net.*;
import java.net.URL;
import java.net.URLConnection;
import java.util.Properties;
import com.config.engine.interfaces.Cfg;
import static com.lang.LConfig.*;

public class CProxy extends ConfigNode {

    public CProxy() {
        super(CService.class, "proxy", PROXY__PROXY);

        onAfterChange(this, (item, isUserValue, newValue) -> {
            if (item != enabled
                    && item != host
                    && item != port
                    && item != username
                    && item != password)
                return;

            Properties props = System.getProperties();

            if (!enabled.value()) {
                props.put("proxySet", "false");
                props.remove("proxyHost");
                props.remove("proxyPort");
                props.remove("proxy.host");
                props.remove("proxy.port");
                props.remove("http.proxyHost");
                props.remove("http.proxyPort");
                props.remove("https.proxyHost");
                props.remove("https.proxyPort");
                props.remove("http.proxyUser");
                props.remove("http.proxyPassword");
                props.remove("proxy.authentication.username");
                props.remove("proxy.authentication.password");
                return;
            }

            String h = host.value() != null ? host.value() : "";
            String p = port.value() != null ? port.value().toString() : "";
            props.put("proxySet", enabled.value() ? "true" : "false");
            props.put("proxyHost", h);
            props.put("proxyPort", p);
            props.put("proxy.host", h);
            props.put("proxy.port", p);
            props.put("http.proxyHost", h);
            props.put("http.proxyPort", p);
            props.put("https.proxyHost", h);
            props.put("https.proxyPort", p);

            if (username.value() != null && password.value() != null) {
                props.put("http.proxyUser", username.value());
                props.put("http.proxyPassword", password.value());
                props.put("proxy.authentication.username", username.value());
                props.put("proxy.authentication.password", password.value());

                Authenticator.setDefault(new Authenticator() {
                    @Override
                    protected PasswordAuthentication getPasswordAuthentication() {
                        return new PasswordAuthentication(username.value(),
                                password.value().toCharArray());
                    }
                });
            }
            return;
        });
    }

    @Cfg
    public final static CfBool enabled = new CfBool("active", ACTIVE, false);

    @Cfg
    public final static CfString host = new CfString("host", HOST, "");

    @Cfg
    public final static CfInt port = new CfInt("port", PORT, 8080);

    @Cfg
    public final static CfString username = new CfString("username", USER, "");

    @Cfg
    public final static CfPassword password = new CfPassword("password", USER, "");

    @Cfg
    public final static CfStringList bypass = new CfStringList("bypass",
            PROXY__BYPASS,
            "127.0.0.1", "localhost")
            .description(PROXY__BYPASS__DESCRIPTION);

    // ----------------------------------------
    public static Proxy getProxy(URL address) {
        String shost = address.getHost();

        for (String s : bypass.getValue(null))
            if (Str.matchesMask(s, shost))
                return Proxy.NO_PROXY;

        return enabled.value() ? new Proxy(Proxy.Type.HTTP,
                InetSocketAddress.createUnresolved(
                        host.value(), port.value())) : Proxy.NO_PROXY;
    }

    public static void setAuthorization(URLConnection conn) {
        if (enabled.value() && username.value() != null)
            conn.setRequestProperty("Proxy-Authorization",
                    "Basic " + Base64.encode(username + ":" + password));
    }
}
