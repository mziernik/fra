package com.utils.text;

import com.utils.Utils;
import com.utils.Is;
import com.utils.collections.Strings;
import java.util.*;

/**
 * @author Miłosz Ziernik
 * @date 11 września 2015
 * @encoding UTF-8
 */
public class Similarity {

    /**
     * Zwraca frazy podobne do podanej
     *
     * @param name
     * @param list
     * @param threshold - próg prawdopodobieństwa 0..1
     * @param resultsLimit
     * @return
     */
    public static Strings getSimilars(String name, Iterator<String> list,
            double threshold, int resultsLimit) {
        // sugerowanie literowek
        Strings lst = new Strings();

        for (Map.Entry<String, Double> en : getMap(name, list).entrySet()) {
            Double val = en.getValue();
            if (val < threshold || lst.size() == resultsLimit)
                break;
            lst.add(en.getKey());
        }
        return lst;
    }

    public static LinkedHashMap<String, Double> getMap(String name, Iterator<String> list) {
        LinkedHashMap<String, Double> map = new LinkedHashMap<>();

        while (list.hasNext()) {
            String s = list.next();
            map.put(s, Similarity.levenshteinDistance(s, name));
        }

        Utils.sortMap(map, new Comparator<Map.Entry<String, Double>>() {

            @Override
            public int compare(Map.Entry<String, Double> o1, Map.Entry<String, Double> o2) {
                return Double.compare(o2.getValue(), o1.getValue());
            }
        });
        return map;
    }

    public static double levenshteinDistance(String s1, String s2) {
        if (s1 == null || s2 == null)
            return 0;

        if (s1.length() < s2.length()) {
            String swap = s1;
            s1 = s2;
            s2 = swap;
        }

        int bigLen = s1.length();

        s1 = s1.toLowerCase();
        s2 = s2.toLowerCase();

        int[] costs = new int[s2.length() + 1];
        for (int i = 0; i <= s1.length(); i++) {
            int lastValue = i;
            for (int j = 0; j <= s2.length(); j++)
                if (i == 0)
                    costs[j] = j;
                else if (j > 0) {
                    int newValue = costs[j - 1];
                    if (s1.charAt(i - 1) != s2.charAt(j - 1))
                        newValue = Math.min(Math.min(newValue, lastValue),
                                costs[j]) + 1;
                    costs[j - 1] = lastValue;
                    lastValue = newValue;
                }
            if (i > 0)
                costs[s2.length()] = lastValue;
        }
        int editDistance = costs[s2.length()];

        if (bigLen == 0)
            return 1f;

        return (double) (bigLen - editDistance) / (double) bigLen;

    }

}
