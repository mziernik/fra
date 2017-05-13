package com.test;

import com.utils.collections.HashList;
import com.mlogger.Log;
import com.utils.Utils;
import com.utils.Is;
import com.servlet.handlers.*;
import com.utils.*;
import java.util.*;

public class HashListTest extends TestClass {

    public void test() {

        List<String> strings = new LinkedList<>();
        HashList<String> list = new HashList<>();

        for (int i = 0; i < 100000; i++)
            strings.add(Utils.randomId(10));

        for (int i = 0; i < 1000; i++)
            strings.add("aaaa");

        long ts = System.nanoTime();

        list.addAll(strings);

        HashList<String> copy = list.getCopy();

        copy.clear();

    }

}
