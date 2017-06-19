package com.webapi;

import com.lang.LWebapi;
import com.model.repository.DynamicRepo;
import com.model.repository.Repository;
import com.servlet.controller.BaseSession;
import com.utils.date.TDate;
import com.utils.date.time.Interval;
import com.utils.date.time.Unit;
import com.utils.reflections.datatype.DataType;
import com.webapi.core.*;

public class WHttpSessions implements WebApi {

    public WHttpSessions() {

    }

    @WebApiEndpoint()
    public Repository getAll() {

        DynamicRepo<BaseSession, String> ds = new DynamicRepo<>("sessions", LWebapi.SESSIONS);

        ds.column(String.class, "id", DataType.KEY, LWebapi.ID,
                ses -> ses.id).primaryKey();

        ds.column(TDate.class, "created", DataType.TIMESTAMP, LWebapi.CREATED,
                ses -> ses.created);

        ds.column(Interval.class, "lastAccess", DataType.DURATION, LWebapi.LAST_REQUESTR,
                ses -> new Interval(ses.getLastAccessedTime(), Unit.MILLISECONDS));

        ds.column(Interval.class, "maxInactive", DataType.DURATION, LWebapi.TIME_LIMIT,
                ses -> ses.getMaxInactiveInterval());

        ds.column(Interval.class, "remTime", DataType.DURATION, LWebapi.REMAINING_TIME,
                ses -> new Interval(ses.getRemainingTime(), Unit.MILLISECONDS));

        ds.column(Integer.class, "wsConn", DataType.INT, LWebapi.WEBSOCKET_CONNECTIONS,
                ses -> ses.webSocketConnections.size());

        ds.column(String.class, "host", DataType.STRING, LWebapi.HOST,
                ses -> ses.remoteAddress.getHostString());

        ds.column(String.class, "ua", DataType.STRING, LWebapi.USER_AGENT,
                ses -> ses.userAgent.toString());

        ds.column(String.class, "lang", DataType.STRING, LWebapi.LANGUAGE,
                ses -> ses.language.get().name);

        ds.column(String.class, "user", DataType.STRING, LWebapi.USER,
                ses -> ses.user != null ? ses.user.username : null);

        ds.column(Long.class, "recB", DataType.SIZE, LWebapi.RECEIVED,
                ses -> ses.bytesReceived);

        ds.column(Long.class, "sendB", DataType.SIZE, LWebapi.SENT,
                ses -> ses.bytesReturned);

        ds.column(Integer.class, "reqC", DataType.INT, LWebapi.REQUEST_COUNT,
                ses -> ses.requestCount.get());

        ds.fillRows(BaseSession.getSessions());

        return ds;

    }

}
