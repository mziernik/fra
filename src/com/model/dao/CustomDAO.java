package com.model.dao;

import com.intf.runnable.Runnable1;
import com.model.dao.CustomDAO.CustomDaoRows;
import com.model.dao.core.DAO;
import com.model.dao.core.DAOQuery;
import com.model.dao.core.DAORow;
import com.model.dao.core.DAORows;
import com.utils.collections.TList;
import java.util.LinkedHashMap;
import java.util.Map.Entry;

public class CustomDAO implements DAO<CustomDaoRows> {

    private final Runnable1< CustomDaoRows> runnable;

    public CustomDAO(Runnable1<CustomDaoRows> runnable) {
        this.runnable = runnable;
    }

    @Override
    public TList<CustomDaoRows> process(TList<? extends DAOQuery> queries) throws Exception {

        TList<CustomDaoRows> results = new TList<>();
        for (DAOQuery query : queries) {
            CustomDaoRows rows = new CustomDaoRows(query);
            this.runnable.run(rows);
            results.add(rows);
        }

        return results;
    }

    public static class CustomDaoRow extends DAORow {

        private final LinkedHashMap<String, Object> map = new LinkedHashMap<>();

        public CustomDaoRow(DAORows rows) {
            super(rows);
        }

        public CustomDaoRow cell(String name, Object value) {
            map.put(name, value);
            return this;
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
            throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
        }

    }

    public static class CustomDaoRows extends DAORows<CustomDaoRow> {

        public final TList<CustomDaoRow> rows = new TList<>();

        public CustomDaoRows(DAOQuery query) {
            super(query);
        }

        @Override
        protected TList<CustomDaoRow> getRows() {
            return rows;
        }

        public CustomDaoRow add() {
            CustomDaoRow row = new CustomDaoRow(this);
            rows.add(row);
            return row;
        }

    }

}
