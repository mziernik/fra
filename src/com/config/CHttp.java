package com.config;

import com.config.engine.ConfigNode;
import com.config.engine.field.*;
import com.config.engine.interfaces.*;
import com.utils.Str;
import com.utils.Url;
import com.utils.date.time.Interval;
import com.utils.date.time.Unit;
import static com.lang.LConfig.*;
import com.servlet.requests.HttpRequest;

public class CHttp extends ConfigNode {

    public CHttp() {
        super(CService.class, "http", HTTP__HTTP);
    }

    @Cfg
    public final static CfString fdn = new CfString("fdn", HTTP__FDN, "localhost");

    @Cfg
    public final static CfUrl url = new CfUrl("url",
            HTTP__URL,
            null)
            .description(HTTP__URL__DESCRIPTION);

    @Cfg
    public final static CfInterval sessionMaxInterval = new CfInterval("session_max_interval",
            HTTP__SESSION_MAX_INTERVAL,
            new Interval(15, Unit.MINUTES));

    @Cfg
    public final static CfInt maxRequestCount = new CfInt("max_requests",
            HTTP__MAX_REQUESTS,
            100);

    @Cfg
    public final static CfBool autoDetectlanguage = new CfBool("auto_detect_language",
            HTTP__AUTO_DETECT_LANGUAGE, true);

    @Cfg
    public final static CfBool useLanguageCookie = new CfBool("use_language_cookie",
            HTTP__USE_LANGUAGE_COOKIE, true);

    @Cfg
    public final static CfInt maxRequestQueueSize = new CfInt("max_request_queue",
            HTTP__MAX_REQUEST_QUEUE,
            300);

    @Cfg
    public final static CfInt maxSessionCount = new CfInt("max_sessions",
            HTTP__MAX_SESSIONS,
            1000);

    public static Url url(Url reference, String path, Boolean https) {

        if (reference != null)
            return reference.getAbsolutePath(path, https);

        String surl = CHttp.url.value().toString();
        if (surl == null || surl.trim().isEmpty())
            throw new RuntimeException("URL address is not defined");

        return new Url(new Str(surl).removeSufix("/") + "/" + new Str(path).removePrefix("/"));
    }

    public static Url url(HttpRequest reference, String path, Boolean https) {
        return url(reference != null ? reference.url : null, path, https);
    }

}
