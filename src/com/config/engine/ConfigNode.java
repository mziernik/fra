package com.config.engine;

import com.config.engine.interfaces.AfterChangeListener;
import com.config.engine.interfaces.BeforeChangeListener;
import com.config.engine.interfaces.FieldsOrder;
import com.events.Dispatcher;
import com.intf.callable.Callable1;
import com.json.JArray;
import com.json.JElement;
import com.json.JObject;

import com.servlet.Handlers;
import com.utils.Utils;
import com.utils.Is;
import com.utils.collections.TList;
import com.utils.hashes.Hashes;
import com.utils.reflections.TClass;
import com.utils.reflections.TField;
import java.util.*;

public class ConfigNode extends ConfigElement<ConfigNode> implements Iterable<ConfigElement> {

    final LinkedHashMap<String, ConfigElement> children = new LinkedHashMap<>();
    //  final Map<String, ConfigElement> all = new LinkedHashMap<>();
    public Boolean expanded; // ToDo: Sprawdzić czy działa
    public boolean groupsAtTop = true; // czy w drzewie jako pierwsze mają być wyświetlane grupy czy pola
    Class<?> clazz;
    String prefix;
    private boolean initialized;
    private FieldsOrder order = FieldsOrder.FIELD;

    final Dispatcher<ValueGetListener> onGetValue = new Dispatcher<>();
    final Dispatcher<BeforeChangeListener> onBeforeChange = new Dispatcher<>();
    final Dispatcher<AfterChangeListener> onAfterChange = new Dispatcher<>();

    public ConfigNode orderBy(FieldsOrder order) {
        this.order = Utils.coalesce(order, FieldsOrder.FIELD);
        return this;
    }

    public ConfigNode orderName(String orderName) {
        this.orderName = orderName;
        return this;
    }

    public void save(ConfigField<?, ?, ?> field) throws Exception {
        ConfigNode root = getRoot();

        if (root == HConfig.instance()) {
            Handlers.config.getInstance().save(field);
            return;
        }

        if (root != this) {
            root.save(field);
            return;
        }

        throw new UnsupportedOperationException("Cannot save");
    }

    /**
     *
     * @param parentClass
     * @param prefix Prefiksy w grupach nie są dziedziczone !!
     * @param name
     */
    public ConfigNode(Class<? extends ConfigNode> parentClass, String prefix, CharSequence name) {
        super(null, name);

        if (parentClass != null) {
            this.parent = HConfig.groups.get(parentClass);

            if (parent == null) {
                parent = new TClass<>(parentClass).newInstance(null);
                HConfig.groups.put(parentClass, parent);
            }
        }

        this.key = Hashes.idHash6(getClass().getName());

        this.prefix = prefix;
        if (parent != null)
            parent.children.put(key, this);
    }

    public ConfigNode(ConfigNode parent, String prefix, CharSequence name) {
        super(null, name);
        this.parent = parent;
        this.key = Hashes.idHash6(getClass().getName());
        this.prefix = prefix;
        if (parent != null)
            parent.children.put(key, this);
    }

    ConfigNode(Class<?> clazz) {
        super(Hashes.idHash6(clazz.getName()), null);
        this.clazz = clazz;
    }

    public static ConfigNode get(Class<?> cls) {
        for (ConfigNode cn : HConfig.groups.values())
            if (cn.clazz == cls)
                return cn;
        return null;
    }

    public TList<ConfigElement> getAll() {
        init();
        return new TList<>(all.values());
    }

    public ConfigField<?, ?, ?> getFieldF(String key) {
        ConfigField<?, ?, ?> field = getField(key);
        if (field == null)
            throw new ConfigException("Field not found: " + key);
        return field;
    }

    void init() {
        if (initialized)
            return;

        initialized = true;

        try {

            ConfigNode root = getRoot();

            for (TField f : new TClass<>(getClass()).getDeclaredFields(true, ConfigElement.class)) {
                f.setAccessible(true);
                if (f.isAbstract() || f.isPrivate())
                    continue;

                Object obj = f.get(this);
                if (obj == parent || obj == HConfig.instance() || obj == this)
                    continue;

                ConfigElement ce = (ConfigElement) obj;

                ConfigElement el = children.get(ce.key);
                if (el != null && el != ce)
                    throw new ConfigException("Config element key \"" + ce.key + "\" is not unique");

                if (root == HConfig.instance()) {
                    children.put(ce.key, ce);
                    continue;
                }

                ce.setParent(this, f);

                if (ce instanceof ConfigNode)
                    ((ConfigNode) ce).init();
            }

            final Set<String> keys = new HashSet<>();

            if (parent == null)
                Utils.visit(this, (node, visitor) -> {

                    for (ConfigElement el : node.children.values()) {
                        if (keys.contains(el.key))
                            throw new ConfigException("Config element key \"" + el.key + "\" is not unique");

                        if (el instanceof ConfigNode)
                            visitor.visit((ConfigNode) el, visitor);
                    }
                });

            sort();

        } catch (Error | RuntimeException e) {
            throw e;
        } catch (Throwable e) {
            throw new ConfigException(e);
        }

    }

    public void sort() {
        Utils.sortMap(children, (o1, o2) -> {

            Callable1<String, ConfigElement> getStr = (elm) -> {

                if (elm instanceof ConfigField) {
                    ConfigField cf = (ConfigField) elm;
                    String val = cf.orderName;

                    if (Is.empty(val))
                        switch (order) {
                            case FIELD:
                                return cf.field != null ? cf.field.getName().toLowerCase() : null;
                            case KEY:
                                return cf.getKey().toLowerCase();
                            case NAME:
                                return cf.name != null ? cf.name.toString().toLowerCase() : null;
                        }
                }

                if (elm instanceof ConfigNode) {
                    ConfigNode cn = (ConfigNode) elm;
                    String val = cn.orderName;

                    if (Is.empty(val))
                        switch (order) {
                            case FIELD:
                                return cn.getClass().getSimpleName().toLowerCase();
                            case KEY:
                                return Is.empty(cn.prefix) ? cn.getKey().toLowerCase() : cn.prefix;
                            case NAME:
                                return cn.name != null ? cn.name.toString().toLowerCase() : null;
                        }
                }

                return "";
            };

            String s1 = getStr.run(o1.getValue());
            String s2 = getStr.run(o2.getValue());
            return Utils.coalesce(s1, "").compareTo(Utils.coalesce(s2, ""));
        });
    }

    public ConfigField<?, ?, ?> getField(String key) {
        init();
        return (ConfigField<?, ?, ?>) all.get(key);
    }

    public JObject getStructure() {
        init();
        JObject json = new JObject();
        getStructure(json);
        //    json.toString();
        return json;
    }

    private void getStructure(JObject json) {

        if (!isVisible())
            return;

        JObject jgroup = json;

        if (parent != null) {
            JObject jGroups = json.objectC("groups");
            jgroup = jGroups.objectC(key);
            jgroup.put("name", name);
            jgroup.put("expanded", expanded);
            jgroup.put("disabled", !enabled());
            jgroup.put("groupsAtTop", groupsAtTop);
            jgroup.put("description", description);
        }

        for (ConfigElement ce : this)
            if (ce instanceof ConfigNode)
                ((ConfigNode) ce).getStructure(jgroup);

        for (ConfigElement ce : this)
            if (ce instanceof ConfigField)
                if (ce.isVisible())
                    jgroup.objectC("items")
                            .put(ce.key, ((ConfigField) ce).getStructure(true));
    }

    public JObject saveAll(JObject json) throws Exception {
        JObject result = new JObject();
        for (JObject el : json.getObjects()) {
            String id = el.getName();
            ConfigField field = getFieldF(id);
            field.store().set(el);
            result.add(id, field.getStructure(true));
        }
        return result;
    }

    @Override
    public Iterator<ConfigElement> iterator() {
        init();
        return new TList<>(children.values()).iterator();
    }

    public String export(boolean usersValue) {
        JObject json = new JObject();
        json.options.quotaNames(false);

        for (ConfigElement el : this.getAll()) {
            if (el instanceof ConfigNode)
                continue;

            ConfigField field = (ConfigField) el;
            if ((usersValue && field.isDefaultState()) // 
                    || (!usersValue && field.getValue(ValueSource.USER) == null))
                continue;

            ConfigStoreFactory store = field.store();

            JArray value = store.getValuesArray(ValueSource.USER);
            if (value != null)
                value.options.compactMode(true);

            if (usersValue)
                json.add(field.getKey(), value);
            else
                json.put(field.getKey(), store.toObject(ValueSource.USER));
        }
        return json.toString();
    }

    public boolean import_(JObject json, boolean usersValue) throws Exception {
        if (json == null)
            return false;

        // Walidacja
        for (JElement el : json) {
            ConfigField field = this.getFieldF(el.getName());
            ConfigStoreFactory store = field.store();
            field.validate(
                    store.parse(usersValue ? el.asArray() : el.asObject().arrayF("val")),
                    true);
        }

        // Nadpisanie konfiguracji
        for (JElement el : json) {
            ConfigField field = this.getFieldF(el.getName());

            ConfigStoreFactory store = field.store();

            if (usersValue)
                store.set(false, null, el);
            else
                store.set(json.asObject());
        }

        return true;
    }

    public ConfigNode onBeforeChange(Object context, BeforeChangeListener<?, ?> listener) {
        onBeforeChange.listen(context, listener);
        return this;
    }

    public ConfigNode onAfterChange(Object context, AfterChangeListener<?, ?> listener) {
        onAfterChange.listen(context, listener);
        return this;
    }

    public ConfigNode onGetValue(Object context, ValueGetListener<?, ?, ?> listener) {
        onGetValue.listen(context, listener);
        return this;
    }

    protected void onInitialize() throws Exception {
    }
}
