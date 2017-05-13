package com.servers;

import com.utils.Utils;
import com.utils.Is;
import com.context.AppConfig;
import com.context.AppContextInitializer;
import com.json.JObject;
import com.json.exceptions.JException;
import com.lang.LServers;
import com.utils.Url;
import java.util.Objects;

public class Connector implements Cloneable {

    public static Connector get(Boolean ssl) {

        for (Connector c : AppConfig.server.connectors)
            if (ssl != null && ssl.equals(c.isSecure()))
                return c;
        return null;
    }

    public Object serverConnector;
    private String scheme;

    private String localAddress;
    private String fdn;
    private String address;

    private Integer localPort;
    private Integer publicPort;
    private Integer port;

    private Integer maxHeaderCount;
    private Integer maxParameterCount;
    private Integer maxPostSize;
    private String uriEncoding;
    //--------------- SSL ---------------
    private String keystoreFile;
    private String keystorePass;
    private String sslProtocol;

    public Connector() {

    }

    public Connector(ServerConfig cfg, JObject json) throws JException {
        scheme = json.getName();

        localPort = json.getInt("bindPort");
        publicPort = json.getInt("publicPort", localPort);
        localAddress = cfg.intf;
        fdn = cfg.fdn;

        sslProtocol = json.getStr("sslProtocol", null);
        keystoreFile = json.getStr("keystoreFile", null);
        keystorePass = json.getStr("keystorePass", null);

        maxHeaderCount = json.getInt("maxHeaderCount", null);
        maxParameterCount = json.getInt("maxParameterCount", null);
        maxPostSize = json.getInt("maxPostSize", null);
    }

    public void addStartupInfo() {
        AppContextInitializer.startupInfo.put("CONNECTOR " + getLocalPort(),
                scheme.toUpperCase() + " " + getLocalAddress() + ":" + getLocalPort()
                + (!Objects.equals(getLocalAddress(), getPublicAddress())
                || !Objects.equals(getLocalPort(), getPublicPort())
                ? " -> " + getPublicAddress() + ":" + getPublicPort()
                : ""));
    }

    public void verify() {

        if (scheme == null)
            throw new Error(LServers.SCHEME_NOT_DEFINED.toString());

        if (Is.empty(address) && Is.empty(localAddress))
            throw new Error(LServers.PARAMETR_MISSING.toString("localAddress"));

        if (Is.empty(port) && Is.empty(localPort))
            throw new Error(LServers.PARAMETR_MISSING.toString("localPort"));

        if (Is.empty(address) && Is.empty(fdn))
            throw new Error(LServers.PARAMETR_MISSING.toString("publicAddress"));

        if (Is.empty(port) && Is.empty(publicPort))
            throw new Error(LServers.PARAMETR_MISSING.toString("publicPort"));

    }

    public boolean isSecure() {
        return Utils.coalesce(scheme, "").equalsIgnoreCase("https");
    }

    public Integer getLocalPort() {
        return Utils.coalesce(localPort, port, 80);
    }

    public Integer getPublicPort() {
        return Utils.coalesce(publicPort, port, 80);
    }

    public String getLocalAddress() {
        return Utils.coalesce(localAddress, address, "127.0.0.1");
    }

    public String getPublicAddress() {
        return Utils.coalesce(fdn, address, "127.0.0.1");
    }

    public String getKeystoreFile() {
        return keystoreFile;
    }

    public String getKeystorePass() {
        return keystorePass;
    }

    public String getScheme() {
        return Utils.coalesce(scheme, "http");
    }

    public Url getUrl() {
        return new Url(getScheme() + "://" + getPublicAddress() + ":" + getPublicPort());
    }

    @Override
    public String toString() {
        return getScheme() + "://" + getPublicAddress() + ":" + getPublicPort();
    }

    public Integer getMaxPostSize() {
        return Utils.coalesce(maxPostSize, 1024 * 1024 * 100);
    }

    public Integer getMaxHeaderCount() {
        return Utils.coalesce(maxHeaderCount, 1000);
    }

    public Integer getMaxParameterCount() {
        return Utils.coalesce(maxParameterCount, 1000);
    }

    public String getSslProtocol() {
        return Utils.coalesce(sslProtocol, "TLS");
    }

}
