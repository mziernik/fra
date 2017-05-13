/*
 */
package com.database;

import org.junit.After;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;
import static org.junit.Assert.*;

/**
 *
 * @author user
 */
public class SqlConditionTest {

    public SqlConditionTest() {
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

    @Test
    public void testAdd() {

        SqlCondition cond = new SqlCondition(true);
        cond.compactMode = true;

        cond.addCondition(false).add("user = 'aaaa'").add("user = 'bbbb'").addCondition(true).add("x").add("y");

        cond.addCondition(false).add("pid = 11").add("pid = 222");
        cond.add("TRUE");
        System.out.println(cond.toString());
    }

}
