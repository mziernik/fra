/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package com.utils.reflections;

import static com.mlogger.DataType.XML;
import com.utils.Execute;
import com.xml.XML;
import java.lang.reflect.Field;
import java.lang.reflect.Type;
import java.util.Collection;
import java.util.Date;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;
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
public class TypeAdapterTest {

    public TypeAdapterTest() {
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

    public Map<String, Integer> map;

    public List<Date> dateList;
    public Date[] dateArr;

    public List<String>[] listArr;

    @Test
    public void testIsSupporterd() throws NoSuchFieldException {

        map = (Map<String, Integer>) new TypeAdapter(new TClass<>(getClass()
                .getField("map")).raw).collection("klucz1:1", "klucz2:2");

        listArr = (List<String>[]) new TypeAdapter(new TClass<>(getClass().getField("listArr")).raw)
                .collection("2016-01-01 20:20:20.222", "2012-12-12 12:12:12.111");

        dateList = (List<Date>) new TypeAdapter(new TClass<>(getClass().getField("dateList")).raw)
                .collection("2016-01-01 20:20:20.222", "2012-12-12 12:12:12.111");

        dateArr = (Date[]) new TypeAdapter(new TClass<>(getClass().getField("dateArr")).raw).collection("2016-01-01 20:20:20.222", "2012-12-12 12:12:12.111");

        assertEquals(true, TypeAdapter.isSupporterd(Boolean.class));
        assertEquals(true, TypeAdapter.isSupporterd(XML.class));
        assertEquals(true, TypeAdapter.isSupporterd(TClass.class));

        assertEquals(true, TypeAdapter.isSupporterd(Execute.class));

        assertEquals(true, TypeAdapter.isSupporterd(Collection.class));
        assertEquals(true, TypeAdapter.isSupporterd(List.class));
        assertEquals(true, TypeAdapter.isSupporterd(Set.class));

    }

}
