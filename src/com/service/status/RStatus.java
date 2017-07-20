package com.service.status;

import com.model.repository.Column;
import com.model.repository.ForeignColumn;
import com.model.repository.RecordUpdate;
import com.model.repository.Repository;
import com.model.repository.intf.CRUDE;
import com.utils.date.TDate;
import com.utils.reflections.datatype.DataType;
import com.utils.reflections.datatype.MapDataType;
import java.util.Map;

public class RStatus extends Repository<String> {

    static RStatus instance;

    public final static Column<String> KEY = new Column<>(c -> {
        c.repository = RStatus.class;
        c.type = DataType.KEY;
        c.key = "key";
        c.name = "Klucz";
    });

    public final static Column<String> NAME = new Column<>(c -> {
        c.repository = RStatus.class;
        c.type = DataType.STRING;
        c.key = "name";
        c.name = "Nazwa";
    });

    public final static Column<Object> VALUE = new Column<>(c -> {
        c.repository = RStatus.class;
        c.type = DataType.ANY;
        c.key = "value";
        c.name = "Wartość";
    });

    public final static Column<String> TYPE = new Column<>(c -> {
        c.repository = RStatus.class;
        c.type = DataType.STRING;
        c.key = "type";
        c.name = "Typ";
    });

    public final static Column<String> DESCRIPTION = new Column<>(c -> {
        c.repository = RStatus.class;
        c.type = DataType.MEMO;
        c.key = "desc";
        c.name = "Opis";
    });

    public final static ForeignColumn<String, RStatus> PARENT = new ForeignColumn<>(c -> {
        c.repository = RStatus.class;
        c.type = DataType.KEY;
        c.key = "parent";
        c.name = "Rodzic";
    }, KEY);

    public final static Column<Boolean> GROUP = new Column<>(c -> {
        c.repository = RStatus.class;
        c.type = DataType.BOOLEAN;
        c.key = "group";
        c.name = "Grupa";
    });

    public final static Column<String> COMMENT = new Column<>(c -> {
        c.repository = RStatus.class;
        c.type = DataType.STRING;
        c.key = "comment";
        c.name = "Komentarz";
    });

    public final static Column<String> COLOR = new Column<>(c -> {
        c.repository = RStatus.class;
        c.type = DataType.STRING;
        c.key = "color";
        c.name = "Kolor";
    });

    public final static Column<TDate> LAST_UPDATED = new Column<>(c -> {
        c.repository = RStatus.class;
        c.type = DataType.TIMESTAMP;
        c.key = "updated";
        c.name = "Zaktualizowany";
    });

    public final static Column<Integer> UPDATES = new Column<>(c -> {
        c.repository = RStatus.class;
        c.type = DataType.INT;
        c.key = "updates";
        c.name = "Aktualizacji";
    });

    public final static Column<Map<String, Object>> ATTRIBUTES = new Column<>(c -> {
        c.repository = RStatus.class;
        c.type = new MapDataType<>(DataType.ANY);
        c.key = "attrs";
        c.name = "Atrybuty";
    });

    public RStatus() {
        super(c -> {
            c.primaryKey = KEY;
            c.displayName = NAME;
            c.key = "status";
            c.name = "Status";
            c.group = "System";
            c.crude.set(CRUDE.READ); // tylko do odczytu
            c.local = true;
        });
        instance = this;
    }

    @Override
    protected RecordUpdate localUpdate(String pk) {
        return super.localUpdate(pk);
    }

}
