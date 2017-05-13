/*
 */
package com.database;

import com.utils.Utils;
import com.database.drivers.h2.H2;
import com.database.drivers.postgresql.PostgreSQL;
import com.exceptions.SQLError;
import org.junit.After;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

import static org.junit.Assert.*;

import com.utils.Unquoted;
import java.sql.SQLException;

/**
 *
 * @author user
 */
public class DatabaseTest {

    public DatabaseTest() {
    }

    @BeforeClass
    public static void setUpClass() {
    }

    @AfterClass
    public static void tearDownClass() {
    }

    @Before
    public void setUp() {
    }

    @After
    public void tearDown() {
    }

    private H2 h2() {
        return new H2(new DBConnectionData(H2.class, "H2:mem", "serviceDbTest"));
    }

    @Test
    public void format() throws Exception {

        Database db = h2();

        assertEquals("SELECT 5-10-15", db.processParams("SELECT ?-?-?", 5, 10, 15));

        assertEquals("SELECT true : 3.14 : 'Abc' : 1, 2, 3",
                db.processParams("SELECT ? : ? : ? : ?", true, 3.14, "Abc", new int[]{1, 2, 3}));

        assertEquals("SELECT (5, 10, 15)", db.processParams("SELECT (?)",
                new Object[]{new int[]{5, 10, 15}}));

        assertEquals("SELECT (5, 10, 15)", db.processParams("SELECT (?)",
                new Object[]{new Integer[]{5, 10, 15}}));

        assertEquals("SELECT (5, 10, 15)", db.processParams("SELECT (?)",
                Utils.asList(5, 10, 15)));

    }

    @Test
    public void escape() throws Exception {
        Database db = h2();

        assertEquals("SELECT 'ala'", db.processParams("SELECT ?", "ala"));

        assertEquals("SELECT ala", db.processParams("SELECT ?", new Unquoted("ala")));

        assertEquals("SELECT 'a''la'", db.processParams("SELECT ?", "a'la"));

        assertEquals("a'la", db.execute("SELECT ?", "a'la").first().getStr(0));
    }

    @Test
    public void insert() throws Exception {
        assertEquals("INSERT INTO tabela (tablica)\n"
                + "VALUES (\n"
                + "	ARRAY['a', 'b', 'c']::int[]	-- tablica\n"
                + ")",
                new PostgreSQL().insert("tabela")
                        .arg("tablica::int[]", Utils.asList("a", "b", "c"))
                        .toString());
    }

    @Test
    public void cast() throws Exception {
        Database db = new PostgreSQL();

        final String RESULT = "INSERT INTO tabelka (user_name, date, order, status, flags)\n"
                + "VALUES (\n"
                + "	'Użytkownik',	-- user_name\n"
                + "	'2016-01-01 10:10:10'::timestamp,	-- date\n"
                + "	'4'::int,	-- order\n"
                + "	'true'::bool,	-- status\n"
                + "	ARRAY[1, 5, 10]	-- flags\n"
                + ")";

        assertEquals(RESULT,
                db.insert("tabelka")
                        .arg("user_name", "Użytkownik")
                        .arg("date::timestamp", "2016-01-01 10:10:10")
                        .arg("order::int", "4")
                        .arg("status::bool", "true")
                        .arg("flags", Utils.asList(1, 5, 10))
                        .toString());

        assertEquals(RESULT,
                db.insert("tabelka")
                        .arg("user_name", "Użytkownik")
                        .arg("date", new Unquoted("'2016-01-01 10:10:10'::timestamp"))
                        .arg("order", new Unquoted("'4'::int"))
                        .arg("status", new Unquoted("'true'::bool"))
                        .arg("flags", Utils.asList(1, 5, 10))
                        .toString());
    }

    @Test
    public void query() throws Exception {
        Database db = h2();

        assertEquals("SELECT * FROM users WHERE id = 10 AND staus IN (5)",
                db.query("SELECT * FROM users WHERE id = ? AND staus IN (:status)", 10)
                        .arg("status", 5)
                        .toString());

        assertEquals("SELECT * FROM users WHERE id = 10 AND staus IN (5, 10, 15)",
                db.query("SELECT * FROM users WHERE id = ? AND staus IN (:status)", 10)
                        .arg("status", new int[]{5, 10, 15})
                        .toString());

        assertEquals("SELECT * FROM users WHERE id = 10 AND staus IN (5, 10, 15)",
                db.query("SELECT * FROM users WHERE id = ? AND staus IN (:status)", 10)
                        .arg("status", Utils.asList(5, 10, 15))
                        .toString());

    }

    @Test
    public void testSelect() throws Exception {

        Database db = h2();

        QueryRows rows = db.execute("SELECT 'Hello' AS tekst, true AS bool, 12345 AS int");
        QueryRow row = rows.first();

        assertEquals("Hello", row.getStr(0));
        assertEquals("Hello", row.getStr("tekst"));

        assertEquals(true, row.getBool(1));
        assertEquals(true, row.getBool("bool"));

        assertEquals(12345, row.getInt(2));
        assertEquals(12345, row.getInt("int"));

        assertEquals(1, row.getArray("int", false).size());

    }

    @Test
    public void array() throws Exception {
        H2 db = h2();

        db.execute("CREATE TABLE test\n"
                + "(\n"
                + "    id INT NOT NULL AUTO_INCREMENT,\n"
                + "    array ARRAY NOT NULL,\n"
                + "    name VARCHAR\n"
                + ");");

        QueryRows rows = db.insert("test")
                .arg("name", "nazwa")
                .arg("array", Utils.asList(1, 2, 3, 4))
                .execute();

        int id = rows.generatedKeys.first().getInt(0);

        rows = db.execute("SELECT * FROM test WHERE id = ?", id);

        QueryRow row = rows.first();
        assertEquals("nazwa", row.getStr("name"));
        assertArrayEquals(new String[]{"1", "2", "3", "4"}, row.getArray("array", false).toArray());
        assertArrayEquals(new Integer[]{1, 2, 3, 4}, row.getArrayInt("array", false).toArray());
    }

}
