package com.mlogger.status;

import com.intf.runnable.Runnable1;
import java.util.LinkedHashMap;
import java.util.Map;

public class StatusGroup extends StatusItem {

    public final static StatusGroup ROOT = new StatusGroup();
    public final Map<String, StatusItem> children = new LinkedHashMap<>();
    protected boolean sorted = true;
    boolean devOnly;

    public StatusGroup(StatusGroup parent, String key, String caption) {
        super(parent, key, caption);
    }

    public StatusGroup devOnly(boolean devOnly) {
        this.devOnly = devOnly;
        return this;
    }

    private StatusGroup() {
        super(null, null, null);
    }

    public StatusItem item(String caption) {
        return new StatusItem(this, null, caption);
    }

    public StatusItem item(String key, String caption) {
        return new StatusItem(this, key, caption);
    }

    public StatusItem get(String key) {
        return children.get(key);
    }

    public boolean remove(String key) {
        synchronized (children) {
            return children.remove(key) != null;
        }
    }

    public StatusGroup group(String key, String caption) {
        return new StatusGroup(this, key, caption);
    }

    @Override
    public StatusGroup onUpdate(Runnable1<StatusItem> onUpdate) {
        this.onUpdate = onUpdate;
        return this;
    }

    public StatusGroup sorted(boolean sorted) {
        this.sorted = sorted;
        return this;
    }

    public void clear() {
        synchronized (children) {
            children.clear();
        }
    }

}
