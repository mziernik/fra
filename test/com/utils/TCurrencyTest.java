/*
 */
package com.utils;

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
public class TCurrencyTest {

    @Test
    public void testParse_Double_TCurrency() {
        String s1 = TCurrency.parse("1.20").toString();
        String s2 = TCurrency.parse("1,20").toString();

        String s3 = TCurrency.parse("1.2").toString();
        String s4 = TCurrency.parse("1,2").toString();
        String s5 = new TCurrency(1.2d).toString();

    }

}
