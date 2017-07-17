package com.model;

import com.exceptions.ServiceException;
import com.model.repository.Column;
import com.model.repository.Record;
import com.model.repository.Repository;
import com.utils.reflections.datatype.*;

public class RRepoUpdate extends Repository<String> {

    private static RRepoUpdate instance;

    public final static Column<String> ID = new Column<>(c -> {
        c.repository = RRepoUpdate.class;
        c.type = DataType.KEY;
        c.key = "id";
        c.name = "ID";
        c.required = true;
        c.readOnly = true;
        c.unique = true;
    });

    public final static Column<String> NAME = new Column<>(c -> {
        c.repository = RRepoUpdate.class;
        c.type = DataType.STRING;
        c.daoName = null;
        c.key = "name";
        c.name = "Nazwa";
    });

    public final static Column<Boolean> STATE = new Column<>(c -> {
        c.repository = RRepoUpdate.class;
        c.type = DataType.BOOLEAN;
        c.key = "state";
        c.name = "Stan";
        c.defaultValue = true;
    });

    private RRepoUpdate() {
        super(c -> {
            c.key = "repoUpdate";
            c.daoName = "repo_update";
            c.name = "Aktualizacje repozytoriów";
            c.group = "System";
            c.primaryKey = ID;
            c.displayName = NAME;
        });

        if (instance != null)
            throw new ServiceException();
        instance = this;
        Repository.register(this);
    }

    public static void add(Repository<?> repo) {

        if (instance == null)
            instance = new RRepoUpdate();

        instance.localUpdate(null)
                .set(ID, repo.getKey())
                .set(NAME, repo.getName())
                .set(STATE, !Boolean.TRUE.equals(repo.config.onDemand))
                .update();
    }

    public static boolean canUpdate(Repository<?> repo) {
        if (instance == null)
            return false;
        Record rec = instance.read(repo.getKey());
        return Boolean.TRUE.equals(rec.get(STATE));
    }

}
