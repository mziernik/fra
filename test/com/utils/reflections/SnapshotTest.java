/*
 */
package com.utils.reflections;

import com.json.JSON;
import com.utils.collections.Strings;
import com.utils.date.TDate;
import com.utils.text.StrWriter;
import java.io.Serializable;
import java.text.ParseException;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeSet;
import org.junit.Assert;
import org.junit.Test;

/**
 *
 * @author user
 */
public class SnapshotTest {

    public SnapshotTest() {
    }

    @Test
    public void testSomeMethod() throws ParseException {

        RefObj src = new RefObj();
        src.setBaseDate(new TDate("2016-01-01 10:11:12"));
        src.i1 = 1234;
        src.baseMap.put(100, 100d);
        src.list1.add("aaaaaaaaaaaaaaaaaaaaaa");

        String jSrc = JSON.serialize(src).toString();
        System.out.println(jSrc);

        Snapshot clone = new Snapshot(src);

        src.longSet.clear();
        src.map.clear();
        src.lista.clear();

        RefObj dst = new RefObj();
        int hashCode2 = dst.object.hashCode();
        clone.revert();
        clone.export(dst);

        int hashCode3 = dst.object.hashCode();

        String jSrc2 = JSON.serialize(src).toString();
        System.out.println(jSrc);

        String jDst = JSON.serialize(dst).toString();
        System.out.println(jDst);

        Assert.assertEquals(jSrc, jSrc2);
        Assert.assertEquals(jSrc, jDst);

    }

}

class BaseObj {

    public final Map<Integer, Double> baseMap = new HashMap<>();
    private Date baseDate;

    public void setBaseDate(Date baseDate) {
        this.baseDate = baseDate;
    }

}

class RefObj extends BaseObj {

    public int i1;
    private boolean bool = false;
    final Object object = new Object();

//    public final StrWriter writer = new StrWriter();
//
//    public final Strings strings = new Strings("s1");
    public static String ignore1;
    public final String ignore2 = "";
    public transient String ignore3;

    public List<String> lista = new ArrayList<>();
    public final Set<Long> longSet = new TreeSet<>();

    final Map<String, Integer> map = new LinkedHashMap<>();

    public final List<String> list1 = new LinkedList<>();
    public List<String> list2 = new LinkedList<>();

    public int[] arr1 = new int[]{1, 2, 3};

    public RefObj() {
        map.put("raz", 1);
        map.put("dwa", 2);

        longSet.add(1234567890l);
        longSet.add(112233445566778899l);

        lista.add("aaa");
        lista.add("bbb");
    }

}
