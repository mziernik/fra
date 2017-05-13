package com.database.service;

import com.utils.date.time.Unit;
import com.utils.date.time.Interval;
import com.utils.Utils;
import com.utils.Is;
import com.dev.Dev;
import com.database.Database;
import com.database.QueryRow;
import com.database.queries.MultipleQuery;
import com.exceptions.SQLError;
import com.mlogger.Log;
import com.servlet.controller.BaseSession;
import com.servlet.requests.HttpRequest;
import com.utils.Undefined;
import com.utils.Unquoted;
import com.utils.date.*;
import com.utils.hashes.*;
import java.sql.SQLException;
import java.util.LinkedList;
import java.util.List;
import org.slf4j.helpers.Util;

/**
 * @author Miłosz Ziernik
 * @date 07 grudnia 2015
 * @encoding UTF-8
 */
public class Sessions {

    // maksymalny czas życia sesji w bazie (musi być większy od czasu życia w usłudze)
    public static Interval maxSessionTime = new Interval(1, Unit.DAYS);
    //częstotliwość aktualizacji danych sesji
    final static Interval sessionsUpdateInterval = new Interval(1, Unit.MINUTES);

    private static long lastAliveUpdate = System.currentTimeMillis();

    public static void getSessionData(BaseSession session, HttpRequest http) {
        String id = session.id;
        if (id == null || id.isEmpty())
            return;

        try {
            String prevId = http.getCookie(BaseSession.getSessionCookieName());

            if (!Is.empty(prevId)) {
                QueryRow row = new ServiceDB().execute("SELECT * FROM sessions\n"
                        + "WHERE session_id = ?\n"
                        + "ORDER BY alive DESC, created desc\n"
                        + "LIMIT 1",
                        prevId).first();

                if (row != null) {
                    String rsid = row.getStr("session_id");
                    String raddr = http.request.getRemoteAddr();
                    String dbaddr = row.getStr("address", "");

                    if (raddr.equals("0:0:0:0:0:0:0:1"))
                        raddr = "127.0.0.1";

                    if (dbaddr.equals("0:0:0:0:0:0:0:1"))
                        dbaddr = "127.0.0.1";

                    if (!dbaddr.isEmpty() && !dbaddr.equals(raddr))
                        Log.warning("Zminana adresu IP dla sesji " + rsid + " ("
                                + dbaddr + " -> " + raddr + ")");

                    if (raddr.equals(dbaddr)) {
                        // session.user.id = row.getInt("user_id", session.user.id);
                        session.user.username = row.getStr("user_name", session.user.username);
                        String pass = row.getStr("user_pass", null);

                        if (!Is.empty(pass)) {
                            byte[] data = Crypto.decrypt(Hex.toBytes(pass));
                            pass = data != null ? new String(data, Utils.UTF8) : null;
                        }

                        if (!Is.empty(session.user.username) && !Is.empty(pass))
                            Dev.info("Session", "Loguję użytkownika " + session.user.username);

                        session.user.passPlain = pass;
                    }

                    // usun wpis starej sesji przed dodaniem nowej
//                    if (!rsid.equals(id))
//                        execute("DELETE FROM sessions WHERE session_id = ?", rsid);
                }
            }

        } catch (Throwable e) {
            Log.error(e);
        }

    }

    public static void updateSession(BaseSession session, HttpRequest http) {
        String id = session.id;
        if (id == null || id.isEmpty())
            return;

        ServiceDB db = new ServiceDB();
        db.logsEnabled = false;

        String pass = null;
        if (!Is.empty(session.user.passPlain))
            pass = Hex.toString(Crypto.encrypt(session.user.passPlain.getBytes(Utils.UTF8)));

        try {
            db.queue(db.merge("sessions", "session_id")
                    .arg("session_id", id)
                    .arg("alive", new TDate().toString(true))
                    .arg("user_agent", http.userAgent.toString())
                    .arg("address", http.request.getRemoteAddr())
                    .arg("user_id", session.user.id)
                    .arg("user_name", session.user.username)
                    .arg("user_pass", pass != null ? pass : Undefined.TYPE)
                    .arg("expires", session.getMaxInactiveInterval().getTime(Unit.SECONDS))
            );

        } catch (Throwable e) {
            Log.error(e);
        }

    }

    public static void logout(BaseSession session) {
        String id = session.id;
        if (id == null || id.isEmpty())
            return;

        ServiceDB db = new ServiceDB();

        try {
            db.queue(db.update("sessions", "session_id = ?", id)
                    //    .arg("user_name", null)
                    .arg("user_id", null)
                    .arg("user_pass", null));

        } catch (Throwable e) {
            Log.error(e);
        }
    }

    public static void sessionDestroyed(BaseSession session) {
        String id = session.id;
        if (id == null || id.isEmpty())
            return;

        ServiceDB db = new ServiceDB();

        try {
            db.queue(db.update("sessions", "session_id = ?", id)
                    .arg("session_id", id)
                    .arg("alive", new TDate().toString(true))
                    .arg("destroyed", new TDate().toString(true)));

        } catch (Throwable e) {
            Log.error(e);
        }
    }

    public static void updateSessions() throws SQLError, SQLException {

        if (System.currentTimeMillis() - lastAliveUpdate < Sessions.sessionsUpdateInterval.getTime(Unit.MILLISECONDS))
            return;

        lastAliveUpdate = System.currentTimeMillis();

        MultipleQuery mqry = new ServiceDB().multipleQuery();

        List<String> sessions = new LinkedList<>();
        for (BaseSession ses : BaseSession.getSessions())
            sessions.add(ses.id);

        String now = new TDate().toString(true);

        // zaktualizuj bieżące sesje (keep alive) 
        mqry.update("sessions", "session_id IN (?)", sessions)
                .arg("alive", now);

        // oznacz nieistniejące sesje jako wygasłe
        mqry.update("sessions", "(alive IS NULL) AND (NOT session_id IN (?))", sessions)
                .arg("destroyed", new Unquoted("DATEADD('MILLISECOND', "
                        + sessionsUpdateInterval.getTime(Unit.MILLISECONDS)
                        + ", COALESCE(alive, created))"));

        // usuń wygasłe sesje starsze niż ServiceDB.maxSessionTime
        mqry.query("DELETE FROM sessions WHERE destroyed < ?", new TDate()
                .addSeconds((int) (maxSessionTime.getTime(Unit.SECONDS) * -1))
                .toString(false));
        if (!mqry.isEmpty())
            mqry.execute();
    }

}
