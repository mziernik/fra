package com.servlet.controller;

import com.config.CHttp;
import com.thread.ThreadObject;
import com.user.BaseUserData;
import com.utils.date.time.Unit;
import com.utils.date.time.Interval;
import com.utils.Utils;
import com.utils.Is;
import com.config.CService;
import com.context.AppConfig;
import com.context.AppContext;
import com.cron.TTimer;
import com.mlogger.Log;
import com.database.service.Sessions;
import com.lang.core.Language;
import com.mlogger.status.ServiceMonitor;
import com.mlogger.status.StatusGroup;
import com.mlogger.status.StatusItem;
import com.servlet.Handlers;
import com.servlet.UserAgent;
import com.servlet.requests.HttpRequest;
import com.servlet.websocket.WebSocketConnection;
import com.utils.*;
import com.utils.collections.Strings;
import com.utils.collections.SyncList;
import com.utils.date.*;
import java.lang.reflect.*;
import java.net.InetSocketAddress;
import java.util.*;
import java.util.concurrent.atomic.AtomicInteger;

/**
 * Miłosz Ziernik 2012/10/23
 */
public class BaseSession {

    private final static StatusGroup status = ServiceMonitor.service.group("httpses", "HTTP Sessions");

    public final UserAgent userAgent;
    public final InetSocketAddress remoteAddress;
    public final Url url;

    // aktywne sesje
    private final static LinkedHashSet<BaseSession> sessions = new LinkedHashSet<>();
    private long lastAccessTS = System.currentTimeMillis();
    public final static SessionManager manager = new SessionManager();
    public final AtomicInteger requestCount = new AtomicInteger();
    public long bytesReturned;
    public long bytesReceived;
    public final TDate created = new TDate();
    public final HashMap<String, Object> extra = new HashMap<>();
    public final HashMap<Field, Object> sessionFields = new HashMap<>();
    public final HashMap<Class<? extends Page>, HashMap<String, String>> pageParams = new HashMap<>();
    public final HashMap<String, Object> data = new HashMap<>();
    //  public final Page page;
    public final String id = Utils.randomId(10);
    public BaseUserData user;
    private boolean isNew = true;
    private boolean registered;
    public final TObject<Language> language = new TObject<>(CService.language.value())
            .notNull(true);

    private boolean destroyed;

    private Interval maxInactiveTime = new Interval(10, Unit.MINUTES); // [ms] ważność ciastka sesji: 15 minut
    private static Interval maxCookieExpireTime = new Interval(1, Unit.DAYS);
    private final StatusItem sts;
    public final SyncList<WebSocketConnection> webSocketConnections = new SyncList<>();

    //------------------------------------------
    public Class<? extends Page> forcedPage = null; // wymuszony handler, ktory będzie każdoroazowo wywoływany bez względu na URL

    public static String getSessionCookieName() {
        return AppConfig.getServiceName() + "_sid";
    }

    public static BaseSession getInstance(HttpRequest http) {
        if (http.session != null)
            return http.session.keepAlive();

        String sid = http.getCookie(getSessionCookieName());

        if (!Is.empty(sid))
            synchronized (sessions) {
                for (BaseSession ses : sessions)
                    if (ses.id.equals(sid))
                        return ses.keepAlive();
            }

        return Handlers.session.newInstance(http);
    }

    private BaseSession updateCookie(HttpRequest http) {
        http.setCookie(getSessionCookieName(), id, maxCookieExpireTime);
        return this;
    }

    /**
     * Podtrzymanie sesji
     *
     * @return
     */
    public BaseSession keepAlive() {
        lastAccessTS = System.currentTimeMillis();
        return this;
    }

    public void newRequest(ControllerEndpoint<?> ctrl, HttpRequest http) {
        synchronized (requestCount) {
            requestCount.incrementAndGet();
        }

        updateCookie(http);
        keepAlive();

        if (registered) {
            isNew = false;
            return;
        }

        if (ctrl.session())
            synchronized (sessions) {
                sessions.add(this);
            }

        registered = true;
    }

    @Override
    public String toString() {
        return id + " (" + new Strings(user.username, remoteAddress.getHostString(), userAgent.getShortUA())
                .toString(", ") + ")";
    }

    public static BaseSession getInstance() {
        HttpRequest http = HttpRequest.getInstance();

        if (http != null)
            return http.session;

        WebSocketConnection ws = ThreadObject.wsConn.get();

        if (ws != null)
            return ws.httpSession;

        return null;
    }

    public BaseSession() {
        remoteAddress = null;
        userAgent = null;
        url = null;
        sts = null;
    }

    public BaseSession(WebSocketConnection wsConn) {

        remoteAddress = wsConn.remoteAddress;
        userAgent = new UserAgent(wsConn.headers.get("User-Agent"));
        url = wsConn.requestUrl;

        user = BaseUserData.newInstance();
        maxInactiveTime = CHttp.sessionMaxInterval.value();
        sts = status.item(id)
                .onUpdate((StatusItem sts) -> {
                    sts.value(this.user.username + ", " + remoteAddress.getHostString());
                    sts.comment(userAgent.getShortUA());
                });
    }

    private BaseSession(HttpRequest http) {
        url = http.url;
        remoteAddress = InetSocketAddress.createUnresolved(http.request.getRemoteAddr(),
                http.request.getRemotePort());
        userAgent = http.userAgent;

        user = BaseUserData.newInstance();
        maxInactiveTime = CHttp.sessionMaxInterval.value();
        sts = status.item(id)
                .onUpdate((StatusItem sts) -> {
                    sts.value(this.user.username + ", " + remoteAddress.getHostString());
                    sts.comment(userAgent.getShortUA());
                });
    }

    public boolean isDestroyed() {
        return destroyed;
    }

    public void invalidate() {
        if (!registered)
            return;

        destroyed = true;
        synchronized (sessions) {
            sessions.remove(this);
        }

        sts.remove();
        Sessions.sessionDestroyed(this);
        Log.event("Session", "Destroy: " + id + ", " + remoteAddress.getHostString());
    }

    public void loadConfig() {

    }

    /**
     * @return Zwraca czas pozostały do zakończenia sesji (ms)
     */
    public long getRemainingTime() {
        if (!webSocketConnections.isEmpty())
            return maxInactiveTime.getTime(Unit.MILLISECONDS);

        return lastAccessTS + maxInactiveTime.getTime(Unit.MILLISECONDS)
                - System.currentTimeMillis();
    }

    public long getLastAccessedTime() {
        return lastAccessTS;
    }

    public static BaseSession get(String getSessionById) {
        synchronized (sessions) {
            for (BaseSession ss : sessions)
                if (ss.id.equals(getSessionById))
                    return ss;
        }
        return null;
    }

    public static LinkedList<BaseSession> getSessions() {
        synchronized (sessions) {
            return Utils.asList(sessions);
        }
    }

    public Interval getMaxInactiveInterval() {
        return maxInactiveTime;
    }

    public boolean isNew() {
        return isNew;
    }

    public static boolean invalidateSession(String getSessionById) {

        BaseSession session = get(getSessionById);
        if (session != null)
            session.invalidate();

        return false;
    }

    public int getRequestCount() {
        return requestCount.get();
    }

    public void logout() {
        Sessions.logout(this);
        invalidate();
    }

}

class SessionManager extends TTimer {

    public SessionManager() {
        super(new Interval(1, Unit.SECONDS));
    }

    @Override
    public void run() throws Exception {
        for (BaseSession ses : BaseSession.getSessions())
            if (ses.getRemainingTime() < 0)
                ses.invalidate();

    }

}
