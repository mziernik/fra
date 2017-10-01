package com.model.repository;

import com.model.dao.CustomDAO;
import com.model.dao.CustomDAO.CustomDaoRows;
import com.model.dao.core.DAOQuery;
import com.model.dao.core.DAORows;

import com.model.repository.intf.CRUDE;
import com.utils.Utils;
import com.utils.collections.TList;
import com.utils.reflections.datatype.DataType;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import org.junit.Test;

public class RepositoryTest {
/*
    @Test
    public void test1() throws Exception {

        TestRepo repo = new TestRepo();

        TList<Record> records = new TList<>();

        records.add(repo.create(), rec -> {

            assertNull(rec.get(ID));
            rec.set(ID, 1);
            assertEquals(1, (int) rec.get(ID));

            assertNull(rec.get(LOGIN));
            rec.set(LOGIN, "admin");
            assertEquals("admin", rec.get(LOGIN));
        });

        records.add(repo.create(), rec -> {
            rec.set(ID, 2);
            rec.set(LOGIN, "user");
        });

        Repository.commit(records);

        //---------------------------------------------------
        Record rec = repo.read(1);
        assertEquals("admin", repo.read(1).get(LOGIN));
        rec.set(LOGIN, "root");
        assertEquals("root", rec.get(LOGIN));
        Repository.commit(rec);
        assertEquals("root", repo.read(1).get(LOGIN));
    }

    @Test
    public void test2() throws Exception {

        TestRepo repo = new TestRepo();

        CustomDAO dao = new CustomDAO(rows -> {
            rows.add().cell("id", 5).cell("login", "jasiu");
            rows.add().cell("id", 10).cell("login", "halina");
            rows.add().cell("id", 15).cell("login", "mietek");
        });

        DAOQuery qry = repo.fillQuery(new DAOQuery(repo, dao, CRUDE.CREATE));
        DAORows rows = dao.process(qry);
        repo.load(rows);

        Utils.with(repo.read(5), rec -> {
            assertEquals(5, (int) rec.get(ID));
            assertEquals("jasiu", rec.get(LOGIN));
        });

        Utils.with(repo.read(10), rec -> {
            assertEquals(10, (int) rec.get(ID));
            assertEquals("halina", rec.get(LOGIN));
        });

        Utils.with(repo.read(15), rec -> {
            assertEquals(15, (int) rec.get(ID));
            assertEquals("mietek", rec.get(LOGIN));
        });

    }
}

class TestRepo extends Repository<Integer> {

    public final static Column<Integer> ID = new Column<>(c -> {
        c.type = DataType.INT;
        c.key = "id";
    });
    public final static Column<String> LOGIN = new Column<>(c -> {
        c.type = DataType.STRING;
        c.key = "login";
    });

    public TestRepo() throws Exception {
        super(c -> {
            c.key = "users";
            c.name = "Użytkownicy";
            c.primaryKey = ID;
        });

    }
*/
}
