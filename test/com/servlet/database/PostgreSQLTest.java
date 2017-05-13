/*
 */
package com.servlet.database;

import com.database.DBConnectionData;
import com.database.drivers.postgresql.PostgreSQL;
import org.junit.Test;

import static org.junit.Assert.*;

/**
 *
 * @author User
 */
public class PostgreSQLTest {

    @Test
    public void processParams() throws Exception {

        PostgreSQL db = new PostgreSQL(new DBConnectionData(null, null, null));

        db.eEscape = null;
        assertEquals("SELECT ('abc')", db.processParams("SELECT (?)", "abc"));
        assertEquals("SELECT ('a\nbc')", db.processParams("SELECT (?)", "a\nbc"));
        assertEquals("SELECT (E'a''b\\\\bc')", db.processParams("SELECT (?)", "a'b\bc"));
        assertEquals("SELECT (E'a\\\\\"bc')", db.processParams("SELECT (?)", "a\"bc"));

        db.eEscape = false;
        assertEquals("SELECT ('abc')", db.processParams("SELECT (?)", "abc"));
        assertEquals("SELECT ('a\nbc')", db.processParams("SELECT (?)", "a\nbc"));
        assertEquals("SELECT ('a''b\rc')", db.processParams("SELECT (?)", "a'b\rc"));
        assertEquals("SELECT ('a\"bc')", db.processParams("SELECT (?)", "a\"bc"));

        db.eEscape = true;
        assertEquals("SELECT (E'abc')", db.processParams("SELECT (?)", "abc"));
        assertEquals("SELECT (E'a\\\\nbc')", db.processParams("SELECT (?)", "a\nbc"));
        assertEquals("SELECT (E'a''b\\\\rc')", db.processParams("SELECT (?)", "a'b\rc"));
        assertEquals("SELECT (E'a\\\\\"bc')", db.processParams("SELECT (?)", "a\"bc"));
    }

}
