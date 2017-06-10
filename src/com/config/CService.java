package com.config;

import com.config.engine.ConfigNode;
import com.config.engine.HConfig;
import com.config.engine.cell.select.SelectEntries;
import com.config.engine.field.*;
import com.config.engine.interfaces.*;
import com.context.AppContext;
import com.exceptions.ServiceException;
import com.mlogger.MLogger;
import java.net.InetSocketAddress;
import com.lang.core.Languages;
import com.lang.core.Language;
import java.util.Locale;
import com.lang.core.LString;
import static com.lang.LConfig.*;

public class CService extends ConfigNode {

    public CService() {
        super(HConfig.class, "service", SERVICE__SERVICE);
    }

    @Cfg
    public final static CfEnum<ServiceMode> mode = new CfEnum<>("mode",
            SERVICE__MODE, ServiceMode.class, ServiceMode.DEV)
            .onBeforeChange(CService.class, (item, isUserValue, oldValue, newValue) -> {
                MLogger.instance().setMode(newValue.get());
                return true;
            });

    @Cfg
    public final static CfSelect<Language> language = new CfSelect<Language>(
            "language", SERVICE__LANGUAGE, Language.class, Languages.en, () -> {
                SelectEntries<Language> entries = new SelectEntries<>();
                for (Language t : Languages.allLanguages.values())
                    entries.put(t.key, t.name, t);
                return entries;
            });

    @Cfg
    public final static CfSelect<Locale> locale = new CfSelect<Locale>(
            "locale", SERVICE__LOCALE, Locale.class, null, () -> {
                SelectEntries<Locale> entries = new SelectEntries<>();
                for (Locale l : Locale.getAvailableLocales())
                    entries.put(l.toString(), l.getDisplayName(l), l);
                return entries;
            });

    static {
        Locale loc = Locale.getDefault();
        if (loc != null) {
            locale.setDefaultValue(loc);
            Language lang = Languages.allLanguages.get(loc.getLanguage());
            if (lang != null)
                language.setDefaultValue(lang);
        }
    }

    public static enum ServiceMode {
        DEV(SERVICE_MODE__DEV, 'D'),
        TEST(SERVICE_MODE__TEST, 'T'),
        RELEASE(SERVICE_MODE__RELEASE, 'R');

        public final LString displayName;
        public final char key;

        @Override
        public String toString() {
            return displayName.toString();
        }

        public static ServiceMode get(char key) {
            key = Character.toUpperCase(key);
            for (ServiceMode sm : values())
                if (sm.key == key)
                    return sm;
            throw new ServiceException("Unknown service mode: " + key);
        }

        private ServiceMode(LString displayName, char key) {
            this.displayName = displayName;
            this.key = key;
        }

    }

    public static boolean devMode() {
        return AppContext.devMode || mode.value() == ServiceMode.DEV;
    }

    public static boolean testMode() {
        return mode.value() == ServiceMode.TEST;
    }

    public static boolean releaseMode() {
        return mode.value() == ServiceMode.RELEASE;
    }

    public static class CTechnicalBreak extends ConfigNode {

        public CTechnicalBreak() {
            super(CService.class, "t_break", SERVICE_BREAK__SERVICE_BREAK);
        }

        @Cfg
        public final static CfBool enabled = new CfBool("enabled",
                SERVICE_BREAK__ENABLED,
                false);

        @Cfg
        public final static CfMultiLineText message = new CfMultiLineText("message",
                SERVICE_BREAK__MESSAGE,
                null)
                .setDefaultValue(() -> SERVICE_BREAK__SERVICE_BREAK__DEF_VALUE.toString());

        @Cfg()
        public final static CfMultiLineText details = new CfMultiLineText("details",
                SERVICE_BREAK__DETAILS,
                null)
                .required(false);

        @Cfg
        public final static CfStringList allowed = new CfStringList("allowed_ip",
                SERVICE_BREAK__EXCEPTION_LIST,
                "127.0.0.1", "0:0:0:0:0:0:0:1")
                .description(SERVICE_BREAK__EXCEPTION_LIST__DESCRIPTION);

        @Cfg
        public final static CfBool allowLocal = new CfBool("allow_local",
                SERVICE_BREAK__ALLOW_LOCAL,
                true);

        public static boolean checkAccess(InetSocketAddress address) {
            if (!enabled.value())
                return true;

            if (address == null)
                return false;

            String url = address.getHostName();

            if (allowLocal.value())
                switch (url) {
                    case "127.0.0.1":
                    case "localhost":
                    case "0:0:0:0:0:0:0:1":
                        return true;
                }

            for (String s : allowed.getValue(null))
                if (url.contains(s.toLowerCase()))
                    return true;

            return false;
        }
    }
}
