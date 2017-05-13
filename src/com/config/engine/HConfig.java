package com.config.engine;

import com.config.engine.interfaces.Cfg;
import com.context.AppConfig;
import com.context.index.Index;
import com.database.QueryRow;
import com.database.QueryRows;
import com.database.service.ServiceDB;
import com.events.ServiceEvent;
import com.exceptions.ServiceException;
import com.json.*;
import com.lang.core.LStr;
import com.mlogger.Log;
import com.servlet.Handlers;
import com.utils.Unquoted;
import com.utils.collections.TList;
import com.utils.reflections.TClass;
import com.utils.reflections.TField;
import java.lang.reflect.Modifier;
import java.util.HashMap;
import java.util.Map;
import java.util.Objects;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * @author Miłosz Ziernik
 * @date 05 stycznia 2016
 * @encoding UTF-8
 */
public class HConfig extends ConfigNode {

    static Map<Class<? extends ConfigNode>, ConfigNode> groups = new HashMap<>();

    private static ConfigNode instance;

    public static ConfigNode instance() {
        if (instance == null)
            Handlers.config.getInstance();
        return Objects.requireNonNull(instance, "Missing HConfig instance");
    }

    public HConfig() {
        super((Class) null, null, new LStr("ROOT"));
        if (instance != null)
            throw new ServiceException("HConfig instance already exists");
        instance = this;
    }

    protected void loadExternalDbConfig() throws Exception {
        throw new UnsupportedOperationException("Not implemented");
    }

    protected boolean hasExternalDb() {
        return false;
    }

    protected boolean externalDB(ConfigField field) {
        return field.externalDB();
    }

    public void load() throws Exception {

        for (JElement el : AppConfig.json.objectD("config"))
            try {
                ConfigField<?, ?, ?> field = getFieldF(el.getName());
                if (field.isDefaultState())
                    field.store().set(false, true, null, el);

            } catch (Exception e) {
                Log.error(e);
                Logger.getLogger("").log(Level.SEVERE, null,
                        new ConfigException(el.getName(), e));
            }

        QueryRows rows = new ServiceDB().execute("SELECT * FROM config");

        boolean external = hasExternalDb();

        // wczytaj na początku konfigurację elementów, które są zapisywale lokalnie
        for (QueryRow row : rows)
            try {
                ConfigField<?, ?, ?> field = getFieldF(row.getStr("key"));

                // pomiń elementy, które mają być wczytywane tylko z zewnętrzbej bazy
                if (external && field.externalDB())
                    continue;

                JElement el = JSON.parse(row.getStr("value", null));

                field.store().set(true,
                        row.getBool("default"),
                        row.getStr("variable", null),
                        el);

            } catch (Exception e) {
                Log.error(e);
                Logger.getLogger("").log(Level.SEVERE, null,
                        new ConfigException(row.getStr("key"), e));
            }

        if (external)
            loadExternalDbConfig();

        for (ConfigNode node : groups.values())
            node.onInitialize();

    }

    @Override
    public void save(ConfigField field) throws Exception {

        if (field == null)
            return;

        ServiceDB db = new ServiceDB();

        QueryRow row = db.execute("SELECT * FROM config WHERE key = ?", field.getKey()).first();

        JArray jvalue = field.store().getValuesArray(ValueSource.USER);
        jvalue.options.compactMode(true);
        String value = jvalue.toString();

        db.insertOrUpdate("CONFIG", row == null ? null : "config_id = " + row.getInt("config_id"))
                .arg("key", field.getKey())
                .arg("value", value)
                //   .arg("variable", field.variable())
                .arg("default", field.isDefaultState())
                .arg("last_modified", new Unquoted("CURRENT_TIMESTAMP"))
                .execute();

        QueryRow newRow = db.execute("SELECT * FROM config WHERE key = ?",
                field.getKey()).first();

        ServiceEvent event = new ServiceEvent("Konfiguracja", "Modyfikacja wartości "
                + field.getDisplayValue(null, true));

        event.attribute("Klucz", "key", field.getKey());
        event.dbDiffRow("modified", row, newRow, "LAST_MODIFIED");
        event.execute();
    }

    public void initialize() throws Exception {

        TList<ConfigField> fields = new TList<>();

        for (String s : Index.entries.get("config")) {

            TField field;
            try {
                field = new TField(s);
            } catch (Exception e) {
                Log.warning(e);
                continue;
            }

            field.checkModifiers(Modifier.STATIC);

            ConfigField cf = (ConfigField) field.raw.get(null);
            cf.field = field;
            cf.setAnnotation(field.getAnnotation(Cfg.class));

            fields.add(cf);
        }

        groups.put(this.getClass(), this);
        if (this instanceof HConfig)
            groups.put(HConfig.class, this);

        for (ConfigField<?, ?, ?> cf : fields) {

            Class<? extends ConfigNode> parCls = cf.annotation.parent();

            if (parCls == HConfig.class && ConfigNode.class.isAssignableFrom(cf.field.getDeclaringClass()))
                parCls = (Class<? extends ConfigNode>) cf.field.getDeclaringClass();

            ConfigNode par = groups.get(parCls);
            if (par == null) {
                par = new TClass<>(parCls).newInstance(null);
                groups.put(parCls, par);
            }
            cf.setParent(par, cf.field);
        }

    }

}
