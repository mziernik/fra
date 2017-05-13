/*
 */
package com.database.queries;

import com.database.drivers.postgresql.PostgreSQL;
import java.sql.SQLException;
import org.junit.Test;

/**
 *
 * @author user
 */
public class UpdateMultipleTest {

    @Test
    public void testAdd() throws SQLException {
        String qry = new UpdateMultiple(
                new PostgreSQL(), "users", "id::int", "user_name::text[]", "user_pass::text[]")
                .add(1, "admin", new String[]{"asdasf", "dsfdsf"})
                .add(2, "user", new String[]{"2232", "4444444"})
                .toString();

        System.out.println(qry);

    }

}
