package com.service.events.filter;

import static com.filter.SqlConditionBuilder.buildCondition;
import java.util.*;
import java.util.stream.Collectors;
import com.database.Database;
import com.database.QueryRow;
import com.database.QueryRows;
import com.events.ServiceEvent.EventType;
import com.filter.FCondition;
import com.filter.AbstractFilterHandler;
import com.filter.data.FilterData;
import com.filter.data.FilterGroupData;
import com.json.JArray;
import com.json.JObject;
import com.utils.Is;
import com.utils.collections.Pair;
import com.utils.collections.Strings;
import com.service.events.filter.EventFilter.TableSearchType;
import com.lang.LService;
import com.model.repository.DynamicRepo;
import com.model.repository.Repository;
import com.utils.reflections.datatype.DataType;

/**
 * @author Błażej Palmąka 2017/01/26
 */
public abstract class BaseEventsFilterHandler<TSelf extends BaseEventsFilterHandler> extends AbstractFilterHandler<Integer, EventFilter, Repository> {

    public final Class<? extends Database> dbClass;
    public final Map<EventFilter, ForeignKeyMapping> fkMappings = new LinkedHashMap<>();

    public BaseEventsFilterHandler(Class<? extends Database> dbClass) {
        this.dbClass = dbClass;
    }

    @Override
    protected void doFilter(JObject params) throws Exception {
        Database db = dbClass.newInstance();

        // Obsługa pustych filtrów
        if (filtersData.isEmpty())
            filtersData.addNewGroup();

        String orderQry = getOrderQry(params.getStr("order", null), params.getStr("orderColumn", null));
        if (Is.empty(orderQry))
            orderQry = "ORDER BY id DESC\n";

        // Budowanie zapytań dla poszczególnych grup ---------
        StringBuilder groupQuerys = new StringBuilder();

        for (FilterGroupData<EventFilter> group : filtersData) {
            Pair<Strings, Strings> condition = buildQuery(group); // Pair<joinQuerys, whereQuerys>

            String grpQuery = (""
                    + "#UNION"
                    + "   SELECT e.* FROM (\n"
                    + "      SELECT\n"
                    + "         e.id,\n"
                    + "         e.date,\n"
                    + "         e.type,\n"
                    + "         e.tags,\n"
                    + "         e.source,\n"
                    + "         e.event,\n"
                    + "         e.username,\n"
                    + "         e.address\n"
                    + "      FROM events.events e\n"
                    + "      LIMIT 100000\n"
                    + "   ) AS e\n"
                    + "   #JOIN_QUERYS\n"
                    + "   #WHERE_QUERYS\n")
                    .replace("#UNION", filtersData.indexOf(group) != 0 ? "   UNION\n" : "")
                    .replace("#JOIN_QUERYS", condition.first.isEmpty() ? "" : condition.first.toString())
                    .replace("#WHERE_QUERYS", condition.second.isEmpty()
                            ? (condition.first.isEmpty() ? "WHERE TRUE\n" : "WHERE FALSE\n") // Obsługa pustych filtrów
                            : "WHERE TRUE\n" + condition.second.toString());

            groupQuerys.append(grpQuery);
        }

        // Filtrowanie danych --------------------------------
        String filterQuery = (""
                + "SELECT id FROM (\n"
                + "#GROUP_QUERYS"
                + ") AS result\n"
                + "#ORDER"
                + "LIMIT #RES_LIMIT;\n\n")
                .replace("#GROUP_QUERYS", groupQuerys.toString())
                .replace("#ORDER", orderQry)
                .replace("#RES_LIMIT", Integer.toString(resultLimit));

        // Zapisanie identyfikatorów rezultatów --------------
        resultsIds.addAll(db.execute(filterQuery).getContentNumber("id").stream()
                .map(Number::intValue)
                .collect(Collectors.toList()));
    }

    @Override
    public Repository getDataPage(int page, int entries) throws Exception {
        page = page < 1 ? 1 : page;
        entries = entries < 1 ? 1 : entries;
        int offset = (page - 1) * entries;

        return getData(offset, entries);
    }

    @Override
    public Repository getData(List<Integer> dataIds) throws Exception {
        throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
    }

    @Override
    public Repository getData(int offset, int entries) throws Exception {
        /*
        DynamicRepo<QueryRow, Integer> data = new DynamicRepo<>("events", LService.EVENTS);

        data.offset = offset;
        data.limit = entries;
        data.results = resultsIds.size();

        // Pobranie odpowiednich identyfikatorów ------------
        List<Integer> ids = resultsIds.stream()
                .skip((long) offset)
                .limit((long) entries)
                .collect(Collectors.toList());

        // Jeśli nie ma rezultatów zwróć pustą tablicę ------
        if (ids.isEmpty())
            return data;

        // Pobieranie rezultatów z bazy ---------------------
        Database db = dbClass.newInstance();
        QueryRows rows = db.execute(""
                + "SELECT \n"
                + "   t.sort_order,\n"
                + "   e.id,\n"
                + "   e.date,\n"
                + "   e.type,\n"
                + "   e.tags,\n"
                + "   e.source,\n"
                + "   e.event,\n"
                + "   e.username,\n"
                + "   e.address\n"
                + "FROM events.events e\n"
                + "JOIN UNNEST(ARRAY" + ids.toString() + "::integer[]) WITH ORDINALITY t(id, sort_order) ON (t.id = e.id)\n"
                + "ORDER BY sort_order ASC");

        data.column(Integer.class, "id", DataType.INT, LService.ID, row -> row.getInt("id"))
                .primaryKey();

        data.column(Date.class, "date", DataType.TIMESTAMP, LService.DATA, row -> row.getDate("date"))
                .searchable(true)
                .dateFormat("yyyy-MM-dd HH:mm:ss", "yyyy-MM-dd HH:mm:ss");

        data.column(String.class, "type", DataType.STRING, LService.TYPE, row -> EventType.getByKey(row.getChar("type")).name())
                .searchable(true);

        data.column(String[].class, "tags", null, LService.TAGS, row -> row.getArray("tags", false).toArray())
                .searchable(true);

        data.column(String.class, "source", DataType.STRING, LService.SOURCE, row -> row.getStr("source", null))
                .searchable(true);

        data.column(String.class, "event", DataType.STRING, LService.EVENT, row -> row.getStr("event", null))
                .searchable(true);

        data.column(String.class, "username", DataType.STRING, LService.USER, row -> row.getStr("username", null))
                .searchable(true);

        data.column(String.class, "address", DataType.STRING, LService.ADDRESS, row -> row.getStr("address", null))
                .searchable(true);

        data.fillRows(rows);

        return data;
         */

        return null;
    }

    @Override
    public JObject getDetails(Integer id) throws Exception {
        Database db = dbClass.newInstance();
        JObject json = new JObject();

        QueryRows rows = db.execute(""
                + "SELECT id, date, type, tags, source, event, username, address, url\n"
                + "FROM events.events WHERE id = " + id + ";\n\n"
                + ""
                + "SELECT id, tags, name, value\n"
                + "FROM events.attributes WHERE event_id = " + id + ";\n\n"
                + ""
                + "SELECT column_name, keys\n"
                + "FROM events.foreign_keys WHERE event_id = " + id + ";\n\n"
                + ""
                + "SELECT id, tags, name, value, content_type\n"
                + "FROM events.details WHERE event_id = " + id + ";\n\n");

        // Zdarzenia ------
        QueryRow eventRow = rows.first();
        json.objectC("event")
                .add("id", eventRow.getInt("id"))
                .add("date", eventRow.getDate("date").toString())
                .add("type", EventType.getByKey(eventRow.getChar("type")).name())
                .add("tags", eventRow.getArray("tags", false))
                .add("source", eventRow.getStr("source", null))
                .add("event", eventRow.getStr("event", null))
                .add("username", eventRow.getStr("username", null))
                .add("address", eventRow.getStr("address", null))
                .add("url", eventRow.getStr("url", null));

        // Atrybuty ------
        QueryRows attribRows = rows.nextResults();
        JArray attribs = json.arrayC("attributes");
        attribs.array().addAll("*id", "tags", "name", "value");

        for (QueryRow row : attribRows)
            attribs.array().addAll(
                    row.getInt("id"),
                    row.getArray("tags", false),
                    row.getStr("name", null),
                    row.getStr("value", null)
            );

        // Klucze obce ------
        QueryRows foreignKeysRows = attribRows.nextResults();
        JArray foreignKeys = json.arrayC("foreignKeys");
        foreignKeys.array().addAll("*columnName", "keys");

        for (QueryRow row : foreignKeysRows)
            foreignKeys.array().addAll(
                    row.getStr("column_name"),
                    row.getArrayInt("keys", false)
            );

        // Szczegóły ------
        QueryRows detailsRows = foreignKeysRows.nextResults();
        JArray details = json.arrayC("details");
        details.array().addAll("*id", "tags", "name", "value", "contentType");

        for (QueryRow row : detailsRows)
            details.array().addAll(
                    row.getInt("id"),
                    row.getArray("tags", false),
                    row.getStr("name", null),
                    row.getStr("value", null),
                    row.getStr("content_type", null)
            );

        return json;
    }

    //##########################################################################
    private String getOrderQry(String column, String order) {
        // Brak sortowania
        if (Is.empty(column))
            return "";

        String sortOrder = null;

        // Określenie kierunku sortowania
        if (Is.empty(order))
            sortOrder = "";
        else if (order.toLowerCase().startsWith("a"))
            sortOrder = "ASC"; // rosnąco
        else if (order.toLowerCase().startsWith("d"))
            sortOrder = "DESC"; // malejąco

        return "ORDER BY " + column + " " + sortOrder + "\n";
    }

    public TSelf addForeignKeyMapping(String filterName, String mapTblName, String mapKeyColumn, String mapValueColumn) {
        return addForeignKeyMapping(filters.getF(filterName), mapTblName, mapKeyColumn, mapValueColumn);
    }

    public TSelf addForeignKeyMapping(EventFilter filter, String mapTblName, String mapKeyColumn, String mapValueColumn) {
        if (filter == null
                || filter.tblSearchType != TableSearchType.FOREIGN_KEY
                || Is.empty(mapTblName)
                || Is.empty(mapKeyColumn)
                || Is.empty(mapValueColumn))
            throw new Error(LService.LACK_OF_NEEDED_DATA.toString());

        fkMappings.put(filter, new ForeignKeyMapping(filter, mapTblName, mapKeyColumn, mapValueColumn));
        return (TSelf) this;
    }

    /**
     * @param conditionQuerys Pair<Strings, Strings>(joinQuerys, whereQuerys>)
     */
    protected void addAnyQuerys(EventFilter filter, FCondition cond, List<String> values, Pair<Strings, Strings> conditionQuerys) {
        int count = 0;
        String qry = null;
        String eventColumns[] = {"date", "tags", "source", "event", "username", "address"};

        StringBuilder sb = new StringBuilder();
        sb.append("(FALSE\n");

        for (String column : eventColumns) {
            qry = buildCondition("e." + column, filter.type, cond, values);
            if (qry == null)
                continue;

            sb.append("      OR (" + qry + ")\n");
            count++;
        }

        sb.append("   )");
        if (count > 0)
            conditionQuerys.second.add("   AND " + sb.toString() + "\n");
    }

    /**
     * @param conditionQuerys Pair<Strings, String>(joinQuerys, whereQuerys>)
     */
    protected void addForeignKeyQuerys(EventFilter filter, FCondition cond, List<String> values, Pair<Strings, Strings> conditionQuerys) {
        ForeignKeyMapping map = fkMappings.get(filter);

        if (map == null)
            throw new Error(LService.NO_MAPPING_KEY_FOR_FILTER.toString(filter.caption));

        String tbl = "efk_" + UUID.randomUUID().toString().substring(0, 6);
        String condition = buildCondition(map.mapValueColumn, filter.type, cond, values);

        if (condition == null)
            return;

        String joinQuery = ""
                + "JOIN events.foreign_keys " + tbl + " ON " + tbl + ".event_id = e.id\n"
                + "   AND " + tbl + ".column_name = '" + filter.key + "'\n"
                + "   AND " + tbl + ".keys <@ (\n"
                + "      SELECT array_agg(" + map.mapKeyColumn + ")\n"
                + "      FROM " + map.mapTblName + "\n"
                + "      WHERE " + condition + "\n"
                + "   )";

        conditionQuerys.first.add(joinQuery);
    }

    /**
     * @param conditionQuerys Pair<Strings, String>(joinQuerys, whereQuerys>)
     */
    protected void addCustomQuerys(EventFilter filter, FCondition cond, List<String> values, Pair<Strings, Strings> conditionQuerys) {
        throw new UnsupportedOperationException("Not supported yet.");
    }

    protected Pair<Strings, Strings> buildQuery(FilterGroupData<EventFilter> groupData) {
        Strings joinQuerys = new Strings();
        Strings whereQuerys = new Strings();

        joinQuerys.separator("");
        whereQuerys.separator("");

        Pair<Strings, Strings> result = new Pair<>(joinQuerys, whereQuerys);

        for (FilterData<EventFilter> data : groupData)
            for (FCondition cond : data.keySet()) {
                EventFilter filter = data.filter;
                LinkedList<String> values = data.get(cond);
                String qry = null;

                switch (filter.tblSearchType) {
                    case EVENT:
                        qry = buildCondition("e." + filter.key, filter.type, cond, values);
                        if (qry == null)
                            continue;

                        whereQuerys.add("   AND " + qry + "\n");
                        break;

                    case ATTRIB:
                        String tbl = "ea_" + UUID.randomUUID().toString().substring(0, 6);

                        qry = buildCondition(tbl + ".value", filter.type, cond, values);
                        if (qry == null)
                            continue;

                        String joinQry = ""
                                + "JOIN events.attributes " + tbl + " ON (e.event_id = " + tbl + ".event_id) AND (\n"
                                + "   " + tbl + ".name = '" + filter.key + "'\n"
                                + "   AND (" + qry + ")\n"
                                + ")\n";

                        joinQuerys.add(joinQry);
                        break;

                    case FOREIGN_KEY:
                        addForeignKeyQuerys(filter, cond, values, result);
                        break;

                    case ANY:
                        addAnyQuerys(filter, cond, values, result);
                        break;

                    case CUSTOM:
                        addCustomQuerys(filter, cond, values, result);
                        break;
                }
            }

        return result;
    }

    //##########################################################################
    public static class ForeignKeyMapping {

        public final EventFilter filter;
        public final String mapTblName;
        public final String mapKeyColumn;
        public final String mapValueColumn;

        public ForeignKeyMapping(EventFilter filter, String mapTblName, String mapKeyColumn, String mapValueColumn) {
            this.filter = filter;
            this.mapTblName = mapTblName;
            this.mapKeyColumn = mapKeyColumn;
            this.mapValueColumn = mapValueColumn;
        }
    }
}
