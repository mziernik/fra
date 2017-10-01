package com.config;

import com.model.repository.Column;
import com.model.repository.ForeignColumn;
import com.model.repository.Repository;
import com.model.repository.intf.CRUDE;
import com.utils.Is;
import com.utils.reflections.datatype.DataType;

public class RConfig extends Repository<String> {

    private static RConfig instance;

    public static RConfig instance() {
        return instance = Is.nullR(instance, () -> Repository.register(new RConfig()));
    }

    public final static Column<String> KEY = new Column<>(c -> {
        c.repository = RConfig.class;
        c.type = DataType.KEY;
        c.key = "key";
        c.name = "Klucz";
        c.unique = true;
    });

    public final static ForeignColumn<String, RConfig> PARENT = new ForeignColumn<>(c -> {
        c.repository = RConfig.class;
        c.type = DataType.KEY;
        c.key = "parent";
        c.name = "Rodzic";
    }, KEY);

    public final static Column<String> NAME = new Column<>(c -> {
        c.repository = RConfig.class;
        c.type = DataType.STRING;
        c.key = "name";
        c.name = "Nazwa";
    });

    public final static Column<String> DESCRIPTION = new Column<>(c -> {
        c.repository = RConfig.class;
        c.type = DataType.MEMO;
        c.key = "desc";
        c.name = "Opis";
    });

    public final static Column<Boolean> IS_DEFAULT_VALUE = new Column<>(c -> {
        c.repository = RConfig.class;
        c.type = DataType.BOOLEAN;
        c.key = "isDefVal";
        c.name = "Użyj wartości domyslnej";
    });

    public final static Column<Object> USER_VALUE = new Column<>(c -> {
        c.repository = RConfig.class;
        c.type = DataType.ANY;
        c.key = "userValue";
        c.name = "Wartość użytkownika";
    });

    public final static Column<Object> DEFAULT_VALUE = new Column<>(c -> {
        c.repository = RConfig.class;
        c.type = DataType.ANY;
        c.key = "defaultValue";
        c.name = "Wartość domyślna";
    });

    public final static Column<String> VARIABLE = new Column<>(c -> {
        c.repository = RConfig.class;
        c.type = DataType.STRING;
        c.key = "variable";
        c.name = "Zmienna";
    });

    public final static Column<Boolean> ENABLED = new Column<>(c -> {
        c.repository = RConfig.class;
        c.type = DataType.BOOLEAN;
        c.key = "enabled";
        c.name = "Aktywne";
    });

    public final static Column<Boolean> VISIBLE = new Column<>(c -> {
        c.repository = RConfig.class;
        c.type = DataType.BOOLEAN;
        c.key = "visible";
        c.name = "Widoczne";
    });

    public final static Column<Boolean> READ_ONLY = new Column<>(c -> {
        c.repository = RConfig.class;
        c.type = DataType.BOOLEAN;
        c.key = "readOnly";
        c.name = "Tylko do odczytu";
    });

    public RConfig() {
        super((RepoConfig r) -> {
            r.key = "configuration";
            r.name = "Konfiguracja";
            r.group = "System";
            r.primaryKey = KEY;
            r.displayName = NAME;
            r.parentColumn = PARENT;
            r.crude.set(CRUDE.READ, CRUDE.UPDATE);
            r.daoName = null;
        });
    }

}
