package com.servers;

import com.config.CHttp;
import static com.context.AppConfig.server;
import com.json.JObject;
import com.json.exceptions.JException;
import java.util.*;
import com.utils.Is;
import com.utils.Url;
import java.net.URI;

/**
 * @author Miłosz Ziernik
 * @date 21 października 2015
 * @encoding UTF-8
 */
public class ServerConfig {

    public String autoRunBrowserURL;
    public String name;
    public String intf;
    public String fdn;
    public final List<Connector> connectors = new LinkedList<>();
    public final Map<String, String> mimeTypes = new HashMap<>();
    public Boolean initWebSocketServer;
    public Boolean initWebServiceServer;

    public void verify() {
        for (Connector conn : connectors)
            conn.verify();

    }

    @Override
    public ServerConfig clone() throws CloneNotSupportedException {
        return (ServerConfig) super.clone();
    }

    public void parse(JObject json) throws JException, Exception {

        name = json.getStr("name", name);
        intf = json.getStr("interface", intf);
        fdn = json.getStr("fdn", fdn);

        CHttp.fdn.setDefaultValue(fdn);

        if (!Is.empty(json.getStr("url", "")))
            CHttp.url.setDefaultValue(new Url(json.getStr("url")));

        Is.notNullV(json.object("https"), (JObject obj) -> {
            Connector conn = new Connector(this, obj);
            server.connectors.add(conn);
            Integer port = conn.getPublicPort();
            if (CHttp.url.value() == null)
                CHttp.url.setDefaultValue(new Url("https://" + fdn
                        + (port == 443 ? "" : ":" + port)));

        });

        Is.notNullV(json.object("http"), (JObject obj) -> {
            Connector conn = new Connector(this, obj);
            server.connectors.add(conn);
            Integer port = conn.getPublicPort();
            if (CHttp.url.value() == null)
                CHttp.url.setDefaultValue(new Url("http://" + fdn
                        + (port == 80 ? "" : ":" + port)));
        });

        Is.notNullV(json.object("proxy"), (JObject obj) -> {
            server.connectors.add(new Connector(this, obj));
        });

    }

}
