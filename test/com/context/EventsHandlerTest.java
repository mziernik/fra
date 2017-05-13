/*
 */
package com.context;

import com.events.ServiceEvent;
import context.ServiceTest;
import org.junit.After;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

/**
 *
 * @author user
 */
public class EventsHandlerTest extends ServiceTest {

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
     * Test of onServiceVersionUpdate method, of class EventsHandler.
     */
    @Test
    public void testOnServiceVersionUpdate() {

        ServiceEvent event = new ServiceEvent("źródło", "Wartość");
        event.tag("t1");
        event.tag("t2");
        event.execute();
    }

}
