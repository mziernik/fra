package com.lang.core;

import com.utils.collections.Props;
import com.utils.collections.TList;
import java.util.Objects;

public class Language {

    public final String key;
    public final String name;
    public final TList<LangEntry> entries = new TList<>();
    public final Props _priv = new Props();

    public Language(String key, String name) {
        this.key = key;
        this.name = name;
    }

    @Override
    public String toString() {
        return key + " (" + name + ")";
    }

    public class LangEntry {

        public final Language language = Language.this;
        public final LanguageItem item;
        public String value;
        public boolean complete;
        public final Props _priv = new Props();
        ExternalLangFile external;

        public final int paramsCount;

        public LangEntry(LanguageItem item, String value, boolean complete) {
            this.item = item;
            int cnt = 0;
            Objects.requireNonNull(value, "Log entry \"" + toString() + "\" value");
            char[] chars = value.toCharArray();

            for (int i = 0; i < chars.length; i++) {
                char curr = chars[i];
                char next = i < chars.length - 1 ? chars[i + 1] : 0;
                if (curr == '%' && next >= '1' && next <= '9')
                    ++cnt;
            }

            paramsCount = cnt;

            this.value = value;
            this.complete = complete;
            entries.add(this);
        }

        @Override
        public String toString() {
            return language.key + ": \"" + value + "\"";
        }

    }
}
