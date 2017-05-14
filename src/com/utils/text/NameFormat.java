package com.utils.text;

import com.utils.StrUtils;
import com.utils.Utils;
import java.util.Arrays;
import java.util.HashSet;
import java.util.Set;

public class NameFormat {

    public final Set<String> reserved = new HashSet<>();
    public boolean convertPolishChars = true;
    public CaseConvert caseConvert = CaseConvert.CAMEL;
    public CaseConvert firstChar = CaseConvert.NONE;

    public Set<Character> breakChars = new HashSet<>();
    public Set<Character> allow = new HashSet<>();
    public String separator = "_";

    public NameFormat() {
        breakChars.addAll(Arrays.asList(new Character[]{
            '_', '-', ' ', '.', ',', ';'
        }));
        for (char c = 'a'; c <= 'z'; c++)
            allow.add(c);
        for (char c = 'A'; c <= 'Z'; c++)
            allow.add(c);
        for (char c = '0'; c <= '9'; c++)
            allow.add(c);
    }

    public static enum CaseConvert {
        NONE, CAMEL, LOWER, UPPER
    }

    public NameFormat caseConvert(CaseConvert caseConvert) {
        this.caseConvert = Utils.coalesce(caseConvert, CaseConvert.NONE);
        return this;
    }

    public NameFormat firstUpper() {
        this.firstChar = CaseConvert.UPPER;
        return this;
    }

    public NameFormat firstLower() {
        this.firstChar = CaseConvert.LOWER;
        return this;
    }

    public NameFormat separator(String separator) {
        this.separator = separator;
        return this;
    }

    public NameFormat convertPolishChars() {
        convertPolishChars = true;
        return this;
    }

    public NameFormat reserveJavaKeywords() {
        reserved.addAll(Arrays.asList(new String[]{
            "abstract", "continue", "for", "new", "switch",
            "assert", "default", "goto", "package", "synchronized",
            "boolean", "do", "if", "private", "this", "break", "double",
            "implements", "protected", "throw", "byte", "else", "import",
            "public", "throws", "case", "enum", "instanceof", "return", "transient",
            "catch", "extends", "int", "short", "try", "char", "final",
            "interface", "static", "void", "class", "finally", "long", "strictfp",
            "volatile", "const", "float", "native", "super", "while"
        }));

        return this;
    }

    public NameFormat lowerCase() {
        caseConvert = CaseConvert.LOWER;
        return this;
    }

    public NameFormat upperCase() {
        caseConvert = CaseConvert.UPPER;
        return this;
    }

    public NameFormat camelCase() {
        caseConvert = CaseConvert.CAMEL;
        return this;
    }

    public String format(String text) {
        if (text == null)
            return null;

        if (convertPolishChars)
            text = StrUtils.convertPolishChars(text);

        StrWriter out = new StrWriter();

        boolean conv = false;

        for (int i = 0; i < text.length(); i++) {
            char c = text.charAt(i);

            boolean cv = false;
            for (Character cc : breakChars) {
                cv = cc == c;
                conv |= cv;
                if (conv)
                    break;
            }

            if (cv)
                continue;

            CaseConvert caseConvert = this.caseConvert;

            if (i == 0 && (firstChar == CaseConvert.LOWER || firstChar == CaseConvert.UPPER))
                caseConvert = firstChar;

            if (caseConvert == CaseConvert.LOWER)
                c = Character.toLowerCase(c);

            if (caseConvert == CaseConvert.UPPER)
                c = Character.toUpperCase(c);

            if (conv)
                switch (caseConvert) {
                    case CAMEL:
                        c = Character.toUpperCase(c);
                        break;

                    case LOWER:
                    case UPPER:
                        out.append(separator);
                        break;
                }

            if (allow.contains(c))
                out.append(c);
            else
                out.append(separator);

            conv = false;
        }

        String result = out.toString();

        if (reserved.contains(result))
            return "_" + result;

        return result;
    }

    public static String camelCaseToUnderscore(String name) {
        StrWriter str = new StrWriter();
        for (char c : name.toCharArray())
            if (Character.isUpperCase(c))
                str.append("_").append(Character.toLowerCase(c));
            else
                str.write(c);
        return str.toString();
    }

}
