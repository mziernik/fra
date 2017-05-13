package com.mlogger.status;

import com.config.CService;
import com.context.AppContext;
import com.intf.runnable.Runnable1;
import com.json.JObject;
import com.utils.Utils;
import com.utils.Is;
import com.utils.date.TDate;
import java.util.*;

public class StatusItem {

    public Object value;
    public String comment;
    public String color;
    public final String key;
    public final String path;
    public final String caption;
    public final StatusGroup parent;

    public final LinkedHashMap<String, Object> attributes = new LinkedHashMap<>();
    public final LinkedHashMap<String, Runnable> actions = new LinkedHashMap<>();
    public final TDate created = new TDate();
    public TDate lastUpdate;
    public int updateCount;

    protected Runnable1<StatusItem> onUpdate;

    private StatusItem() {
        this(null, null, "ROOT");
    }

    protected StatusItem(StatusGroup parent, String key, String caption) {
        this.parent = parent;
        this.caption = caption;

        if (Is.empty(key))
            key = Utils.randomId();

        this.key = key;
        this.path = (parent != null && parent.key != null ? parent.key + "." : "") + key;

        if (parent != null && (!parent.devOnly || AppContext.devMode || CService.devMode()))
            synchronized (parent.children) {
                parent.children.put(key, this);

                Utils.sortMap(parent.children, (Map.Entry<String, StatusItem> o1, Map.Entry<String, StatusItem> o2) -> {
                    StatusItem item = o1.getValue();
                    String s1 = (item instanceof StatusGroup ? "a" : "b") + (parent.sorted ? item.caption : "");

                    item = o2.getValue();
                    String s2 = (item instanceof StatusGroup ? "a" : "b") + (parent.sorted ? item.caption : "");

                    return s1.compareTo(s2);
                });

            }

        update();
    }

    public JObject getJson() {
        if (onUpdate != null)
            onUpdate.run(this);

        JObject json = new JObject();

        if (this instanceof StatusGroup && !((StatusGroup) this).children.isEmpty()) {
            JObject jchn = json.objectC("children");
            synchronized (((StatusGroup) this).children) {
                for (StatusItem s : ((StatusGroup) this).children.values())
                    jchn.put(s.key, s.getJson());
            }
        }

        json.put("crt", created);
        json.put("cap", caption);
        json.put("val", value);
        json.put("com", comment);

        if (!attributes.isEmpty())
            json.put("attr", attributes);

        return json;
    }

    public StatusItem onUpdate(Runnable1<StatusItem> onUpdate) {
        this.onUpdate = onUpdate;
        return this;
    }

    public StatusItem value(Object value) {
        this.value = value;
        update();
        return this;
    }

    public StatusItem update() {
        lastUpdate = new TDate();
        ++updateCount;
        return this;
    }

    public StatusItem attribute(String name, Object value) {
        attributes.put(name, value);
        update();
        return this;
    }

//    public StatusItem action(String name, Runnable runnable) {
//        actions.put(name, runnable);
//        update();
//        return this;
//    }
    public void remove() {
        if (parent != null)
            synchronized (parent.children) {
                parent.children.remove(key);
            }
    }

    public StatusItem comment(String comment) {
        this.comment = comment;
        return this;
    }

    public StatusItem color(String color) {
        this.color = color;
        return this;
    }

}
