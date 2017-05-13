package com.lang.core;

import com.context.index.Index;
import com.exceptions.CoreException;
import com.exceptions.ServiceException;
import com.intf.callable.CallableEx1;
import com.lang.core.Language.LangEntry;
import com.resources.core.ResData;
import com.utils.collections.SyncMap;
import com.utils.collections.TList;
import com.utils.reflections.TClass;
import java.util.Map;

public class Languages {

    public final static SyncMap<String, Language> allLanguages = new SyncMap<>();
    final static SyncMap<String, LanguageItem> allItems = new SyncMap<>();
    final static SyncMap<LString, LanguageItem> lstrItems = new SyncMap<>();

    public final static Language pl = new Language("pl", "Polski");
    public final static Language en = new Language("en", "English");

    static {
        allLanguages.put(pl.key, pl);
        allLanguages.put(en.key, en);
    }

    public static LanguageItem getItem(String id) {
        return allItems.get(id);
    }

    public static LanguageItem getItemF(String id) {
        LanguageItem item = allItems.get(id);
        if (item == null)
            throw new Error(String.format("Language item \"%s\" not found", id));
        return item;
    }

    public static Language get(String id) {
        if (id == null)
            return null;

        id = id.toLowerCase();

        if (id.contains("_"))
            id = id.substring(0, id.indexOf("_"));

        if (id.contains("-"))
            id = id.substring(0, id.indexOf("-"));

        return allLanguages.get(id);

    }

    public static Language getF(String id) {
        Language lang = get(id);
        if (lang == null)
            throw new Error(String.format("Language \"%s\" not found", id));
        return lang;
    }

    public static synchronized void refreshExternals() {
        for (ExternalLangFile elf : externals)
            elf.load();
    }

    private final static TList<ExternalLangFile> externals = new TList<>();

    public static void addExternal(Language lang, String group, ResData res,
            CallableEx1<Map<String, String>, ResData> valuesProvider) {
        externals.add(new ExternalLangFile(lang, group, res, valuesProvider));
    }

    public static <T extends Enum<?> & LString> void addLstringEntry(T lstr, Language lang, String value) {

        LanguageItem item = lstrItems.get(lstr);
        if (item == null) {
            LangDict ann = lstr.getClass().getAnnotation(LangDict.class);
            if (ann == null)
                throw new CoreException("Missing LangDict annotation for "
                        + lstr.getClass().getName());

            item = new LanguageItem(lstr, ann.name());
            allItems.put(item.id, item);
            lstrItems.put(lstr, item);
        }

        LangEntry le = lang.new LangEntry(item, value, true);
        item.add(le);
    }

    public static void load() {

        for (TClass<? extends Enum> cls : Index.getEntries("lang_dict", Enum.class)) {
            LangDict ann = cls.getAnnotation(LangDict.class);
            Enum[] item = cls.raw.getEnumConstants();
        }

    }

}
