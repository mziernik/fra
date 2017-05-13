package com.lang.core;

import com.config.CService;
import com.exceptions.ServiceException;
import com.intf.callable.CallableEx1;
import com.lang.core.Language.LangEntry;
import com.resources.core.ResData;
import com.utils.hashes.Hashes;
import java.util.HashSet;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;

class ExternalLangFile {

    final Language lang;
    final String group;
    final ResData res;
    final CallableEx1<Map<String, String>, ResData> valuesProvider;

    private boolean loaded;
    private final Set<LanguageItem> entries = new HashSet<>();

    ExternalLangFile(Language lang, String group, ResData res, CallableEx1<Map<String, String>, ResData> valuesProvider) {
        this.lang = lang;
        this.group = group;
        this.res = res;
        this.valuesProvider = valuesProvider;
    }

    public void load() {

        if (loaded && CService.devMode() && !res.isFileChanged())
            return;

        loaded = true;

        for (LanguageItem lse : entries)
            lse.remove(lang);

        entries.clear();

        try {
            Map<String, String> map = valuesProvider.run(res);

            for (Entry<String, String> en : map.entrySet()) {

                String key = Hashes.idHash12(group + "/" + en.getKey());
                LanguageItem item = Languages.allItems.get(key);
                if (item == null) {
                    item = new LanguageItem(group, en.getKey());
                    Languages.allItems.put(key, item);
                }

                LangEntry le = lang.new LangEntry(item, en.getValue(), true);

                entries.add(item);
                item.add(le);
            }

        } catch (Throwable e) {
            throw new ServiceException(e);
        }
    }

}
