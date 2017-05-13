package com.config.engine;

import com.config.engine.interfaces.Cfg;
import com.exceptions.CoreException;
import com.intf.callable.Callable;

import com.user.right.UserRight;
import com.utils.Utils;
import com.utils.Is;
import com.utils.reflections.TField;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Objects;

public abstract class ConfigElement<SELF extends ConfigElement<SELF>> {

    protected ConfigNode parent;
    protected TField field;
    protected String key;
    protected CharSequence name;
    protected Callable<Boolean> visibleCalback;
    //------------------
    private Boolean enabled;
    private Boolean externalDB;
    private Boolean allowChangeDefault;
    //------------------
    protected CharSequence description;
    protected String orderName;

    protected Class<? extends UserRight>[] rights = new Class[]{};

    protected Map<String, ConfigElement> all = new LinkedHashMap<>();

    public String getKey() {
        return key;
    }

    public String getName() {
        return Utils.toString(name);
    }

    public ConfigElement(String key, CharSequence name) {
        this.key = key;
        this.name = Objects.requireNonNull(name, "Config field name");
    }

    @Override
    public String toString() {
        return key + " (" + name + ")";
    }

    public boolean isVisible() {
        return visibleCalback == null || Boolean.TRUE.equals(visibleCalback.run());
    }

    protected void checkField() {
        ConfigElement elm = all.get(key);
        if (field.isStatic() && elm != null)
            throw new CoreException("Klucz pola konfiguracji "
                    + field.getFullName() + " (" + key
                    + ") nie jest unikalny\n"
                    + (elm.field != null ? elm.field.getFullName() : ""));
    }

    public ConfigNode getRoot() {
        ConfigNode root = this instanceof ConfigNode ? (ConfigNode) this : parent;
        while (root.parent != null)
            root = root.parent;
        return root;
    }

    protected void setParent(ConfigNode parent, TField field) {

        this.field = field;
        this.parent = Objects.requireNonNull(parent);

        //   this.annotation = field.raw.getAnnotation(Cfg.class);
        String id = Is.empty(key) ? field.raw.getName() : key;

        String key = Utils.coalesce(parent.prefix, "");
        if (!key.isEmpty() && !key.endsWith("."))
            key += ".";

        key += id;

        this.key = key;

        Utils.checkId(id, false, ".");

        parent.init();
        parent.children.put(key, this);

        if (field == null || field.isStatic())
            if (all.containsKey(key))
                throw new ConfigException("Config item " + key + " already exists");

        ConfigNode par = parent;
        while (par != null) {
            par.all.put(key, this);
            par = par.parent;
        }
    }

    public SELF visible(Callable<Boolean> callback) {
        this.visibleCalback = callback;
        return (SELF) this;
    }

    public SELF enabled(boolean enabled) {
        this.enabled = enabled;
        return (SELF) this;
    }

    public boolean enabled() {
        Boolean val = enabled;
        if (val == null)
            val = parent != null ? parent.enabled() : Boolean.TRUE;
        return val;
    }

    protected boolean allowChangeDefault() {
        Boolean val = allowChangeDefault;
        if (val == null)
            val = parent != null ? parent.allowChangeDefault() : Boolean.TRUE;
        return val;
    }

    protected SELF externalDB(Boolean externalDB) {
        this.externalDB = externalDB;
        return (SELF) this;
    }

    protected boolean externalDB() {
        Boolean val = externalDB;
        if (val == null)
            val = parent != null ? parent.externalDB() : Boolean.TRUE;
        return val;
    }

    public SELF description(CharSequence description) {
        this.description = description;
        return (SELF) this;
    }

    void setAnnotation(Cfg annotation) {
        ConfigField cf = (ConfigField) this;
        cf.annotation = field.getAnnotation(Cfg.class);
        if (annotation == null)
            throw new ConfigException(cf, "Missing field annotation");
        allowChangeDefault = annotation.allowChangeDefault().bool;
        externalDB = annotation.externalDB().bool;
        rights = annotation.rights();
    }
}
