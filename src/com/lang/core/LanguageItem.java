package com.lang.core;

import com.dev.Dev;
import com.lang.core.Language.LangEntry;
import com.mlogger.Log;
import com.utils.collections.Strings;
import com.utils.hashes.Hashes;
import com.utils.text.NameFormat;
import com.utils.text.NameFormat.CaseConvert;
import java.util.*;

public class LanguageItem implements Iterable<LangEntry> {

    // public final LString lstr;
    public final String group;
    public final String key;
    private final Map<Language, LangEntry> entries = new HashMap<>();
    private Enum field;
    public final String id;

    @Override
    public String toString() {
        return group + "." + key;
    }

    public String getEntry(Language lang) {
        LangEntry entry = entries.get(lang);
        if (entry == null)
            Dev.warning("Missing translate, " + lang.key + ", " + group + ", " + key);
        return entry != null ? entry.value : null;
    }

    public LangEntry get(Language lang) {
        return entries.get(lang);
    }

    public LanguageItem set(Language lang, String value) {
        LangEntry en = lang.new LangEntry(this, value, true);
        entries.put(lang, en);
        return this;
    }

    LanguageItem(String group, String key) {
        field = null;
        this.group = group;
        this.key = key;
        this.id = Hashes.idHash12(group + "/" + key);
    }

    public LanguageItem(Enum e, String group) {

        field = e;
        //   this.lstr = (LString) e;
        NameFormat nf = new NameFormat().caseConvert(CaseConvert.CAMEL);

        Strings strs = new Strings();
        for (String s : e.name().toLowerCase().split("__"))
            strs.add(nf.format(s));

        key = strs.toString(".");
        this.group = group;
        this.id = Hashes.idHash12(group + "/" + key);
    }

    public void add(LangEntry le) {
        entries.put(le.language, le);

        Strings lst = new Strings();
        Set<Integer> cnt = new HashSet<>();
        for (LangEntry en : entries.values()) {
            lst.add(en.language.key + ": " + en.paramsCount);
            cnt.add(en.paramsCount);
        }

        if (cnt.size() > 1)
            Log.warning("Language", "Incorrect arguments count in \"" + toString() + "\" ("
                    + lst.toString(", ") + ")").comment(field != null
                    ? field.getDeclaringClass().getName() + "." + field.name() : null);
    }

    public LangEntry remove(Language lang) {
        return entries.remove(lang);
    }

    public boolean isEmpty() {
        return entries.isEmpty();
    }

    @Override
    public Iterator<LangEntry> iterator() {
        return entries.values().iterator();
    }

}
