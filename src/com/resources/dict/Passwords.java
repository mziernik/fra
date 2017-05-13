package com.resources.dict;

import com.utils.StrUtils;
import java.io.*;
import java.util.*;
import java.util.zip.ZipEntry;
import java.util.zip.ZipInputStream;

public class Passwords {

    public static List<String> generate(int count, int maxLength) throws IOException {

        final Random rand = new Random();

        ArrayList<String> adjectives = new ArrayList<>();
        ArrayList<String> nouns = new ArrayList<>();
        ArrayList<String> numerals = new ArrayList<>();

        List<String> list = new LinkedList<>();

        try (ZipInputStream zis = new ZipInputStream(
                new BufferedInputStream(
                        Passwords.class.getResourceAsStream("dict.zip"))
        )) {
            ZipEntry entry;
            while ((entry = zis.getNextEntry()) != null) {
                BufferedReader reader = new BufferedReader(
                        new InputStreamReader(zis, "Windows-1250"));
                ArrayList<String> arr = null;
                switch (entry.getName()) {
                    case "a":
                        arr = adjectives;
                        break;
                    case "b":
                        arr = nouns;
                        break;
                    case "n":
                        arr = numerals;
                        break;
                }

                if (arr == null)
                    continue;

                String line;
                while ((line = reader.readLine()) != null)
                    arr.add(line.trim());
            }
        }

        int retry = 0;
        while (list.size() < count && retry < 1000000) {
            ++retry;
            String a = adjectives.get(rand.nextInt(adjectives.size())); // przymiotniki
            String b = nouns.get(rand.nextInt(nouns.size()));  // rzeczowniki
            String n = numerals.get(rand.nextInt(numerals.size())); // liczebniki

            String val = null;
            if (rand.nextInt() % 3 == 0)
                val = (rand.nextBoolean() ? a : b) + " " + n;

            if (val == null || val.length() > maxLength) {
                val = formatAdj(a, b) + " " + b;

                if (val.length() + n.length() < 25)
                    val += " " + n;
            }

            String[] words = val.split(" ");
            val = "";
            for (String s : words) {
                s = StrUtils.convertPolishChars(s.trim().toLowerCase());
                val += s.substring(0, 1).toUpperCase() + s.substring(1);
            }

            if (val.length() <= maxLength) {
                if (!val.endsWith(n) && (val.length() < 13 || (val.length() < 20 && rand.nextBoolean())))
                    val += (1 + rand.nextInt(30));
                list.add(val);
            }

        }

        return list;

    }

    private static String formatAdj(String adj, String noun) {
        for (char[] arr : REPL)
            if (noun.endsWith(Character.toString(arr[2]))
                    && adj.endsWith(Character.toString(arr[0])))
                return adj.substring(0, adj.length() - 1) + arr[1];
        return adj;
    }

    private final static char[][] REPL = new char[][]{
        {'y', 'a', 'ć'},
        {'y', 'a', 'a'}, // salonowy wyparka
        {'i', 'e', 'o'},//brzydki garncarstwo
        {'i', 'a', 'a'},//rusznikarski podłogówka
        {'y', 'e', 'o'},//felietonowy tyraństwo
        {'y', 'e', 'e'}, //estetyczny złoże
        {'i', 'a', 'ć'}, //fabrykancki niezbędność
        {'y', 'e', 'i'} // neutralny widełki
    };
}
