package com.events;

import com.utils.Utils;
import com.context.AppContext;
import com.database.*;
import com.events.ServiceEvent.ServiceEventAttribute;
import com.exceptions.ThrowableException;
import com.json.Escape;
import com.lang.LEvents;
import com.mlogger.Log;
import com.servlet.Handlers;
import com.servlet.controller.Controller;
import com.thread.ThreadObject;
import com.utils.collections.*;
import com.webapi.core.WebApiRequest;
import java.sql.SQLException;
import java.util.*;

/**
 *
 * @author milosz
 */
public class ServiceEvent implements Iterable<ServiceEventAttribute> {

    public static enum EventType {

        debug('D'),
        info('I'),
        warning('W'),
        error('E');

        public final char key;

        private EventType(char key) {
            this.key = key;
        }

        public static EventType getByKey(char key) {
            for (EventType et : values())
                if (et.key == key)
                    return et;

            return null;
        }
    }

    public class ServiceEventAttribute {

        public final String[] tags;
        public final String displayName;
        public final String value;
        public final boolean details;

        public ServiceEventAttribute(boolean details, String[] tags, String displayName, String value) {
            this.details = details;

            List<String> lst = new LinkedList<>();

            if (tags != null)
                for (String s : tags)
                    if (s != null && !s.trim().isEmpty())
                        lst.add(s.trim().toLowerCase());

            this.tags = lst.toArray(new String[lst.size()]);
            this.displayName = displayName;
            this.value = value;
        }

    }

    private final List<String> tags = new LinkedList<>();
    private String value;
    private final Strings result = new Strings();
    private Object source;
    private String address;
    private EventType type = EventType.info;
    public final Date date = new Date();
    // public final List<Pair<String, String>> details = new LinkedList<>();
    public final List<ServiceEventAttribute> attributes = new LinkedList<>();
    private Integer userId;
    private String username;
    private Integer id; // identyfikator zdarzenia z bazy, pomocny przy uaktualnianiu
    public final Map<String, Object> extra = new LinkedHashMap<>(); // mapa wewnętznych właściowości "extra"
    private boolean cancelled;
    private final MapList<String, Integer> keys = new MapList<>();

    public ServiceEvent(final String source, final Integer userId, final String value) {
        this(source, EventType.info, userId, value);
    }

    public ServiceEvent(final String source, final EventType type, final Integer userId, final String value) {
        this.value = value;
        this.type = type;
        this.userId = userId;
        this.source = source;
    }

    public ServiceEvent(final Object source, final String value) {
        this(EventType.info, source, value);
    }

    public ServiceEvent(final EventType type, final Object source, final String value) {
        this.value = value;
        this.type = type;
        this.source = source;
        Controller page = Controller.getInstance(false);

        if (page != null && page.session() != null && page.session().user != null) {
            userId = page.session().user.id;
            username = page.session().user.username;
        } else {
            WebApiRequest request = ThreadObject.webApiReq.get();

            if (request != null
                    && request.session != null
                    && request.session.user != null) {
                userId = request.session.user.id;
                username = request.session.user.username;
            }
        }
    }

    public ServiceEvent type(EventType type) {
        if (type != null)
            this.type = type;
        return this;
    }

    public Integer id() {
        return id;
    }

    public Integer userId() {
        return userId;
    }

    public String username() {
        return username;
    }

    public EventType type() {
        return type;
    }

    public List<String> tags() {
        return tags;
    }

    public String value() {
        return value;
    }

    public Object source() {
        return source;
    }

    public boolean cancelled() {
        return cancelled;
    }

    public ServiceEvent id(Integer id) {
        this.id = id;
        return this;
    }

    public ServiceEvent username(String username) {
        this.username = username;
        return this;
    }

    public ServiceEvent setResult(String result) {
        this.result.clear();
        this.result.add(result);
        return this;
    }

    public ServiceEvent result(String result) {
        this.result.add(result);
        return this;
    }

    public ServiceEvent source(String source) {
        this.source = source;
        return this;
    }

    public ServiceEvent value(String value) {
        this.value = value;
        return this;
    }

    public ServiceEvent tag(String tag) {
        this.tags.add(tag);
        return this;
    }

    public ServiceEvent cancelled(boolean cancelled) {
        this.cancelled = cancelled;
        return this;
    }

    public ServiceEvent extra(String name, Object value) {
        this.extra.put(name, value);
        return this;
    }

    public ServiceEvent details(String[] tags, String displayName, Object value) {
        this.attributes.add(new ServiceEventAttribute(true, tags, displayName,
                Utils.toString(value)));
        return this;
    }

    public ServiceEvent details(String tags, String displayName, Object value) {
        return details(tags.replace("|", ",").split(","), displayName, value);
    }

    public ServiceEvent removeDetails(String displayName) {
        for (ServiceEventAttribute sea : attributes)
            if (sea.details && Objects.equals(sea.displayName, displayName)) {
                attributes.remove(sea);
                break;
            }
        return this;
    }

    public ServiceEvent detailsOverride(String tag, String displayName, Object value) {
        removeDetails(displayName);
        return details(tag, displayName, value);
    }

    public ServiceEvent attribute(String[] tags, String displayName, Object value) {
        if (tags != null && tags.length > 0)
            this.attributes.add(new ServiceEventAttribute(false, tags, displayName,
                    Utils.toString(value)));
        return this;
    }

    public ServiceEvent attribute(String tags, String displayName, Object value) {
        return attribute(tags.replace("|", ",").split(","), displayName, value);
    }

    public LinkedList<ServiceEventAttribute> getAttributes(String key) {
        LinkedList<ServiceEventAttribute> list = new LinkedList<>();
        for (ServiceEventAttribute sea : attributes)
            for (String s : sea.tags)
                if (s != null && s.equals(key))
                    list.add(sea);
        return list;
    }

    public void execute() {
        if (!cancelled)
            try {
                Handlers.events.getInstance().onServiceEvent(this);
            } catch (Throwable e) {
                if (AppContext.unitTestMode)
                    throw new ThrowableException(e);
                Log.error(e);
            }
    }

    @Override
    public Iterator<ServiceEventAttribute> iterator() {
        return attributes.iterator();
    }

    public ServiceEvent key(String columnName, Integer value) {
        if (columnName != null && !columnName.isEmpty() && value != null)
            keys.add(columnName, value);
        return this;
    }

    public Map<String, TList<Integer>> getKeys() {
        return keys.getMap();
    }

    public int dbDiffRow(String tag, QueryRow prevRow, QueryRow currRow, String... excludeColumns) {

        int mods = 0;

        LinkedList<String> excludes = Utils.asList(excludeColumns);

        if (prevRow == null || currRow == null)
            return mods;

        LinkedList<QueryCell> pp = Utils.asList(prevRow.getValues());
        LinkedList<QueryCell> cc = Utils.asList(currRow.getValues());

        for (QueryCell p : Utils.asList(pp))

            for (QueryCell cell : Utils.asList(cc))
                if (p != null
                        && cell != null
                        && p.column.index == cell.column.index) {
                    pp.remove(p);
                    cc.remove(cell);
                    if (!Objects.equals(p.value, cell.value)) {

                        boolean excluded = false;
                        for (String s : excludes)
                            excluded |= p.column.name.equalsIgnoreCase(s);

                        if (!excluded)
                            attribute(new String[]{tag, "modified"},
                                    LEvents.MODIFIED.toString(),
                                    p.column.name + ": "
                                    + Escape.escape(p.value) + " -> "
                                    + Escape.escape(cell.value));
                        ++mods;
                    }
                    break;
                }

        for (QueryCell p : pp)
            if (p != null) {

                boolean excluded = false;
                for (String s : excludes)
                    excluded |= p.column.name.equalsIgnoreCase(s);

                if (!excluded)
                    attribute(new String[]{tag, "removed"},
                            LEvents.REMOVED.toString(),
                            p.column.name + ": "
                            + Escape.escape(p.value));
                ++mods;
            }

        for (QueryCell c : cc)
            if (c != null) {

                boolean excluded = false;
                for (String s : excludes)
                    excluded |= c.column.name.equalsIgnoreCase(s);

                if (!excluded)
                    attribute(new String[]{tag, "created"},
                            LEvents.CREATED.toString(),
                            c.column.name + ": "
                            + Escape.escape(c.value));
                ++mods;
            }

        return mods;
    }

    /**
     * Metoda wyświetla zestawienie różnic pomiędzy dwoma rezultatami zapytania
     * ( naogół przed i po aktualziacji danych ). Tag jest opcjonalny
     */
    public int attribfDbDiff(String tag,
            QueryRows previous, QueryRows current, String primaryKeyColumn,
            String nameColumn, String valueColumn,
            String... foreignKeysColumns) throws SQLException {

        int mods = 0;

        if (previous == null || current == null)
            return mods;

        int keyColumnIdx = previous.getColumn(primaryKeyColumn).index;

        LinkedList<QueryRow> prev = Utils.asList(previous);
        LinkedList<QueryRow> curr = Utils.asList(current);

        for (QueryRow rp : Utils.asList(prev)) {
            Object pkValue = rp.values[keyColumnIdx];

            for (QueryRow cr : Utils.asList(curr))
                if (Objects.equals(cr.values[keyColumnIdx], pkValue)) {
                    prev.remove(rp);
                    curr.remove(cr);

                    Object pValue = rp.getObj(valueColumn, null);
                    Object cValue = cr.getObj(valueColumn, null);

                    if (Objects.equals(pValue, cValue))
                        break;

                    ++mods;

                    attribute(new String[]{tag, "modified"},
                            LEvents.MODIFIED.toString(),
                            rp.getStr(nameColumn) + ": "
                            + Escape.escape(pValue) + " -> " + Escape.escape(cValue));

                    for (String s : foreignKeysColumns)
                        key(s, rp.getInt(s, null));

                    break;
                }
        }

        for (QueryRow rp : prev)
            attribute(new String[]{tag, "removed"},
                    LEvents.REMOVED.toString(),
                    rp.getStr(nameColumn) + ": "
                    + Escape.escape(rp.getObj(valueColumn, null)));

        for (QueryRow rc : curr)
            attribute(new String[]{tag, "created"},
                    LEvents.CREATED.toString(),
                    rc.getStr(nameColumn) + ": "
                    + Escape.escape(rc.getObj(valueColumn, null)));

        return mods + prev.size() + curr.size();

    }

}
