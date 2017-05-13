/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package com.context.tools;

import com.context.AppContext;
import org.junit.After;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;
import static org.junit.Assert.*;

/**
 *
 * @author milosz
 */
public class ORMTest {

    public ORMTest() {
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

    /**
     * Test of main method, of class ORM.
     */
    @Test
    public void testMain() {

        AppContext.sourcesPath.set("X:/ormtest");
        ORM.main(new String[]{
            "orm-build",
            "database.Db",
            "postgres:postgres@127.0.0.1/lincall_manager",
            "orm"
        });

    }

}
