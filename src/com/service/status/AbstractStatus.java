package com.service.status;

import com.model.repository.RecordUpdate;
import com.model.repository.Repository;
import static com.service.status.RStatus.COLOR;
import static com.service.status.RStatus.COMMENT;
import com.utils.reflections.datatype.DataType;

public class AbstractStatus<SELF extends AbstractStatus<SELF, T>, T> {

    public final DataType<T> type;
    public final boolean isGroup;

    public final StatusGroup parent;
    public final String key;
    public final CharSequence name;
    public final CharSequence description;
    final RecordUpdate record;

    protected AbstractStatus(boolean isGroup, DataType<T> type, StatusGroup parent, String key, CharSequence name, CharSequence description) {
        this.isGroup = isGroup;
        this.type = type;
        this.parent = parent;
        this.key = key;
        this.name = name;
        this.description = description;

        if (RStatus.instance == null)
            RStatus.instance = Repository.register(new RStatus());

        this.record = RStatus.instance.localUpdate(null);
        record.set(RStatus.KEY, key)
                .set(RStatus.GROUP, isGroup)
                .set(RStatus.TYPE, type != null ? type.name : null)
                .set(RStatus.NAME, name.toString())
                .set(RStatus.DESCRIPTION, description != null ? description.toString() : null)
                .set(RStatus.PARENT, parent != null ? parent.key : null)
                .update();

    }

    public SELF color(String color) {
        record.set(COLOR, color).update();
        return (SELF) this;
    }

    public SELF comment(String comment) {
        record.set(COMMENT, comment).update();
        return (SELF) this;
    }

    public void remove() {

    }

    public SELF devOnly(boolean b) {
        return (SELF) this;
    }

}
