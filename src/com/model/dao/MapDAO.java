package com.model.dao;

import com.json.JObject;
import com.model.dao.MapDAO.MapDaoRows;
import com.model.dao.core.DAO;
import com.model.dao.core.DAOQuery;
import com.model.dao.core.DAORow;
import com.model.dao.core.DAORows;
import com.utils.collections.TList;
import java.util.Map;
import java.util.Map.Entry;

public class MapDAO implements DAO<MapDaoRows> {

    private final Map<String, Object> map;
    public final TList<MapDaoRow> rows = new TList<>();

    public MapDAO(Map<String, ?> map) {
        this.map = (Map<String, Object>) map;
    }

    public MapDAO(JObject json) {
        this(json.asMap());
    }

    @Override
    public TList<MapDaoRows> process(TList<? extends DAOQuery> queries) throws Exception {

        TList<MapDaoRows> results = new TList<>();
        for (DAOQuery query : queries)
            results.add(new MapDaoRows(query));

        return results;
    }

    public class MapDaoRow extends DAORow {

        private MapDaoRow(DAORows rows) {
            super(rows);
        }

        @Override
        protected Object getDAOValue(String name) {
            return map.get(name);
        }

        @Override
        protected Object getDAOValue(int index) {
            int idx = 0;
            for (Entry<String, Object> en : map.entrySet())
                if (idx++ == index)
                    return en.getValue();
            return null;
        }

        @Override
        protected TList<String> getDAONames() {
            return new TList<>(map.keySet());
        }

    }

    public class MapDaoRows extends DAORows<MapDaoRow> {

        private MapDaoRows(DAOQuery query) {
            super(query);
            rows.add(new MapDaoRow(this));
        }

        @Override
        protected TList<MapDaoRow> getRows() {
            return rows;
        }

        public MapDaoRow add() {
            MapDaoRow row = new MapDaoRow(this);
            rows.add(row);
            return row;
        }

    }
}
