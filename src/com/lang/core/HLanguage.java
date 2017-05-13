package com.lang.core;

import com.database.Database;
import com.database.QueryRows;
import com.database.service.ServiceDB;
import com.lang.core.Language.LangEntry;
import com.utils.Unquoted;
import com.utils.Utils;
import com.utils.Is;
import java.util.Collection;

/**
 * Handler obsługujący tłumaczenia
 *
 * @author user
 */
public class HLanguage {

    private final static String KEY = "H2_KEY";

    protected Database getDb() {
        return new ServiceDB();
    }

    public Language create(String key, String name) throws Exception {
        Utils.checkId(key, true);
        key = key.trim().toLowerCase();

        if (Languages.get(key) != null)
            throw new Error(String.format("Language %s already exists", key));

        Language lang = new Language(key, name);
        doCreate(getDb(), lang);
        Languages.allLanguages.put(lang.key, lang);
        return lang;
    }

    public void remove(Language lang) throws Exception {
//        Integer key = entry._priv.getInt(KEY, null);
//        if (key == null)
//            return;
//        new ServiceDB().execute("DELETE FROM translation_language WHERE id = ?", key);
    }

    protected void doCreate(Database db, Language lang) throws Exception {
        db.insert("translation_language")
                .arg("key", lang.key)
                .arg("name", lang.name)
                .execute();

        lang._priv.put(KEY, lang.key);
    }

    public void editEntry(LangEntry entry) throws Exception {
        Language lang = entry.language;

        getDb().transaction((db) -> {

            if (lang._priv.isNull(KEY))
                doCreate(db, lang);

            QueryRows rows = db.insertOrUpdate("translation_entry",
                    entry._priv.isNull(KEY) ? null : "id = ?",
                    entry._priv.getInt(KEY, null))
                    .arg("item", entry.item.id)
                    .arg("language", lang._priv.get(KEY))
                    .arg("complete", entry.complete)
                    .arg("value", entry.value)
                    .arg("changed", new Unquoted("CURRENT_TIME"))
                    .execute();

            entry._priv.put(KEY, rows.generatedKeys.first().getInt(0));
            entry.item.add(entry);
        });

    }

}
