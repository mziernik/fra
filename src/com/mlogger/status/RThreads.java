package com.mlogger.status;

import com.model.repository.Column;
import com.model.repository.Repository;
import com.utils.reflections.datatype.DataType;

public class RThreads extends Repository<Long> {

    public static RThreads instance;

    public final static Column<Long> ID = new Column<>(c -> {
        c.repository = RThreads.class;
        c.type = DataType.LONG;
        c.key = "id";
        c.name = "ID";
    });

    public final static Column<String> NAME = new Column<>(c -> {
        c.repository = RThreads.class;
        c.type = DataType.STRING;
        c.key = "name";
        c.name = "Nazwa";
    });
    public final static Column<String> GROUP = new Column<>(c -> {
        c.repository = RThreads.class;
        c.type = DataType.STRING;
        c.key = "group";
        c.name = "Grupa";
    });

    //FixMe Enum Thread.State
    public final static Column<String> STATE = new Column<>(c -> {
        c.repository = RThreads.class;
        c.type = DataType.STRING;
        c.key = "state";
        c.name = "Stan";
    });

    public final static Column<Boolean> ALIVE = new Column<>(c -> {
        c.repository = RThreads.class;
        c.type = DataType.BOOLEAN;
        c.key = "alive";
        c.name = "Żyje";
    });

    public final static Column<Boolean> DAEMON = new Column<>(c -> {
        c.repository = RThreads.class;
        c.type = DataType.BOOLEAN;
        c.key = "daemon";
        c.name = "Demon";
    });

    public final static Column<Boolean> INTERRUPTED = new Column<>(c -> {
        c.repository = RThreads.class;
        c.type = DataType.BOOLEAN;
        c.key = "interrupted";
        c.name = "Przerwany";
    });

    public final static Column<Integer> PRIORITY = new Column<>(c -> {
        c.repository = RThreads.class;
        c.type = DataType.INT;
        c.key = "priority";
        c.name = "Priorytet";
    });

    public final static Column<Long> CPU_TIME = new Column<>(c -> {
        c.repository = RThreads.class;
        c.type = DataType.LONG;
        c.key = "cpuTime";
        c.name = "Czas procesora";
    });

    public final static Column<Long> USER_TIME = new Column<>(c -> {
        c.repository = RThreads.class;
        c.type = DataType.LONG;
        c.key = "userTime";
        c.name = "Czas użytkownika";
    });

    public final static Column<Long> ALLOCATED = new Column<>(c -> {
        c.repository = RThreads.class;
        c.type = DataType.SIZE;
        c.key = "alloc";
        c.name = "Zaalokowano";
    });

    public final static Column<Integer> BLOCKED = new Column<>(c -> {
        c.repository = RThreads.class;
        c.type = DataType.INT;
        c.key = "blocked";
        c.name = "Blokady";
    });

    public final static Column<Integer> WAITED = new Column<>(c -> {
        c.repository = RThreads.class;
        c.type = DataType.INT;
        c.key = "waited";
        c.name = "Oczekiwania";
    });

    public RThreads() {
        super(c -> {
            c.primaryKey = ID;
            c.key = "threads";
            c.name = "Wątki";
            c.readOnly = true;
            c.local = true;
        });
        instance = this;
    }

}
