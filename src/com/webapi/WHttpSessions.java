package com.webapi;

import com.lang.LWebapi;
import com.model.dataset.DataSet;
import com.servlet.controller.BaseSession;
import com.utils.date.TDate;
import com.utils.date.time.Interval;
import com.utils.date.time.Unit;
import com.webapi.core.*;

public class WHttpSessions implements WebApi {

    public WHttpSessions() {

    }

    @WebApiEndpoint()
    public DataSet getAll() {

        DataSet<BaseSession, String> ds = new DataSet<>("sessions", LWebapi.SESSIONS);

        ds.column(String.class, "id", LWebapi.ID,
                ses -> ses.id).primaryKey();

        ds.column(TDate.class, "created", LWebapi.CREATED,
                ses -> ses.created);

        ds.column(Interval.class, "lastAccess", LWebapi.LAST_REQUESTR,
                ses -> new Interval(ses.getLastAccessedTime(), Unit.MILLISECONDS));

        ds.column(Interval.class, "maxInactive", LWebapi.TIME_LIMIT,
                ses -> ses.getMaxInactiveInterval());

        ds.column(Interval.class, "remTime", LWebapi.REMAINING_TIME,
                ses -> new Interval(ses.getRemainingTime(), Unit.MILLISECONDS));

        ds.column(Integer.class, "wsConn", LWebapi.WEBSOCKET_CONNECTIONS,
                ses -> ses.webSocketConnections.size());

        ds.column(String.class, "host", LWebapi.HOST,
                ses -> ses.remoteAddress.getHostString());

        ds.column(String.class, "ua", LWebapi.USER_AGENT,
                ses -> ses.userAgent.toString());

        ds.column(String.class, "lang", LWebapi.LANGUAGE,
                ses -> ses.language.get().name);

        ds.column(String.class, "user", LWebapi.USER,
                ses -> ses.user != null ? ses.user.username : null);

        ds.column(Long.class, "recB", LWebapi.RECEIVED,
                ses -> ses.bytesReceived);

        ds.column(Long.class, "sendB", LWebapi.SENT,
                ses -> ses.bytesReturned);

        ds.column(Integer.class, "reqC", LWebapi.REQUEST_COUNT,
                ses -> ses.requestCount.get());

        ds.fillRows(BaseSession.getSessions());

        return ds;

    }

}
