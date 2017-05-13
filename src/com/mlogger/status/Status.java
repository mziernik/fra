package com.mlogger.status;

import com.intf.runnable.Runnable1;
import com.json.JArray;
import com.json.JObject;
import com.mlogger.console.Console;
import com.servlet.controller.BaseSession;
import com.utils.Utils;
import com.utils.Is;
import com.utils.date.TDate;
import com.utils.hashes.Hashes;
import java.util.*;
import java.util.Map.Entry;
import java.util.concurrent.Callable;

public class Status {

    public final static Status ROOT = new Status();

    public Object value;
    public String comment;
    public String color;
    public final String key;
    public final String caption;
    public final Integer objectHash;
    public final String objectName;
    public final Object object;
    public final Status parent;
    public final LinkedList<Status> children = new LinkedList<>();
    public final LinkedHashMap<String, Object> attributes = new LinkedHashMap<>();
    public final LinkedHashMap<String, Runnable> actions = new LinkedHashMap<>();
    public final TDate created = new TDate();
    public TDate lastUpdate;
    public int updateCount;

    private Runnable1<Status> onUpdate;

    private Status() {
        this(null, Status.class, "ROOT");
    }

    public Status(Object object, String caption) {
        this(ROOT, object, caption);
    }

    public Status(String caption) {
        this(ROOT, null, caption);
    }

    public Status(Status parent, Object object, String caption) {
        this.object = object;
        this.parent = parent;
        this.caption = caption;
        this.objectHash = object != null ? object.hashCode() : null;
        this.objectName = object != null
                ? object instanceof Class ? ((Class) object).getName()
                        : object.getClass().getName() : null;

        this.key = objectName != null && objectHash != null
                ? Hashes.idHash12(objectName + "@" + objectHash)
                : Utils.randomId();

        if (parent != null)
            parent.children.add(this);

        update();
    }

    public JObject getJson() {
        if (onUpdate != null)
            onUpdate.run(this);

        JObject json = new JObject();

        json.put("crt", created);
        json.put("cap", caption);
        json.put("val", value);
        json.put("com", comment);

        if (!attributes.isEmpty())
            json.put("attr", attributes);

        if (!children.isEmpty()) {
            JObject jchn = json.objectC("children");
            for (Status s : new LinkedList<>(children))
                jchn.put(s.key, s.getJson());
        }

        return json;
    }

    public Status onUpdate(Runnable1<Status> onUpdate) {
        this.onUpdate = onUpdate;
        return this;
    }

    public Status value(Object value) {
        this.value = value;
        update();
        return this;
    }

    public Status update() {
        lastUpdate = new TDate();
        ++updateCount;
        return this;
    }

    public Status attribute(String name, Object value) {
        attributes.put(name, value);
        update();
        return this;
    }

    public Status action(String name, Runnable runnable) {
        actions.put(name, runnable);
        update();
        return this;
    }

    public Status add(Object object, String caption) {
        return new Status(this, object, caption);
    }

    public Status add(String caption) {
        return new Status(this, null, caption);
    }

    public Status get(Object object) {
        synchronized (children) {
            for (Status s : children)
                if (s.object.equals(object))
                    return s;
        }
        return null;
    }

    public boolean remove() {
        if (parent != null)
            synchronized (parent.children) {
                return parent.children.remove(this);
            }
        return false;
    }

    public Status remove(Object obj) {
        synchronized (children) {
            for (Status s : children)
                if (s.object.equals(object)) {
                    children.remove(s);
                    return s;
                }
        }
        return null;
    }

    public Status comment(String comment) {
        this.comment = comment;
        return this;
    }

    public Status color(String color) {
        this.color = color;
        return this;
    }

}
