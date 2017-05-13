package com.context.unit_test;

import com.utils.reflections.TClass;
import junit.framework.JUnit4TestAdapter;
import junit.framework.TestSuite;

public class GroupTest {

    private final static TestSuite suite = new TestSuite();

    public static void add(Class<?> cls) {
        TClass cc = new TClass(cls);
        if (cc.instanceOf(GroupTest.class)) {
            AbstractServiceTest.require(cls);
            return;
        }
        suite.addTest(new JUnit4TestAdapter(cls));
        AbstractServiceTest.webAppServerRequired
                |= cc.instanceOf(WebAppServerRequired.class);
    }

    public static TestSuite suite() {
        return suite;
    }
}
