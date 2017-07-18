package com.model;

import com.context.intf.ContextInitialized;
import com.exceptions.ServiceException;
import com.model.repository.Column;
import com.model.repository.ForeignColumn;
import com.model.repository.Record;
import com.model.repository.Repository;
import com.model.repository.intf.CRUDE;
import com.resources.FontAwesome;
import com.service.RUsers;
import com.utils.date.TDate;
import com.utils.reflections.datatype.*;

public class RRepoSate extends Repository<String> {

    private static RRepoSate instance;

    public final static Column<String> KEY = new Column<>(c -> {
        c.repository = RRepoSate.class;
        c.type = DataType.KEY;
        c.key = "key";
        c.name = "Klucz";
        c.required = true;
        c.readOnly = true;
        c.unique = true;
    });

    public final static Column<String> NAME = new Column<>(c -> {
        c.repository = RRepoSate.class;
        c.type = DataType.STRING;
        c.key = "name";
        c.name = "Nazwa";
        c.required = true;
    });

    public final static Column<String> GROUP = new Column<>(c -> {
        c.repository = RRepoSate.class;
        c.type = DataType.STRING;
        c.key = "group";
        c.name = "Grupa";
    });

    public final static Column<String> DESC = new Column<>(c -> {
        c.repository = RRepoSate.class;
        c.type = DataType.MEMO;
        c.key = "desc";
        c.name = "Opis";
    });

    public final static Column<Boolean> BROADCAST = new Column<>(c -> {
        c.repository = RRepoSate.class;
        c.type = DataType.BOOLEAN;
        c.key = "broadcast";
        c.name = "Broadcast";
        c.defaultValue = true;
        c.required = true;
    });

    public final static Column<Boolean> ON_DEMAND = new Column<>(c -> {
        c.repository = RRepoSate.class;
        c.type = DataType.BOOLEAN;
        c.key = "onDemand";
        c.name = "Na żądanie";
        c.required = true;
    });

    public final static Column<FontAwesome> ICON = new Column<>(c -> {
        c.repository = RRepoSate.class;
        c.type = DataType.ICON;
        c.key = "icon";
        c.name = "Ikona";
    });

    public final static Column<CRUDE[]> CRUDE = new Column<>(c -> {
        c.repository = RRepoSate.class;
        c.type = EnumsDataType.ofEnum(CRUDE.class,
                e -> Character.toString(e.shortcut), e -> e.title);
        c.key = "crude";
        c.name = "CRUDE";
        c.daoType = "char[]";
        c.required = true;
    });

    public final static Column<TDate> LAST_MODIFIED = new Column<>(c -> {
        c.repository = RRepoSate.class;
        c.type = DataType.TIMESTAMP;
        c.key = "lastModified";
        c.name = "Ostatnio zmodyfikowany";
        c.readOnly = true;
    });

    public final static Column<String> LAST_MODIFIED_BY = new Column<>(c -> {
        c.repository = RRepoSate.class;
        c.type = DataType.STRING;
        c.key = "lastModifiedBy";
        c.name = "Ostatnio zmodyfikowany przez";
        c.readOnly = true;
    });

    public final static ForeignColumn<Integer, RUsers> LAST_MOD_BY_ID = new ForeignColumn<>(c -> {
        c.repository = RRepoSate.class;
        c.type = DataType.INT;
        c.key = "lastModById";
        c.name = "Ostatnio zmodyfikowany przez";
        c.readOnly = true;
    }, RUsers.ID);

    public final static Column<Integer> REVISION = new Column<>(c -> {
        c.repository = RRepoSate.class;
        c.type = DataType.INT;
        c.key = "revision";
        c.name = "Wersja";
        c.readOnly = true;
        c.required = true;
    });

    private RRepoSate() {
        super(c -> {
            c.key = "repoState";
            c.daoName = "repo_state";
            c.name = "Status repozytorium";
            c.group = "System";
            c.primaryKey = KEY;
            c.displayName = NAME;
        });

        if (instance != null)
            throw new ServiceException();
        instance = this;
        Repository.register(this);
    }

    @ContextInitialized
    private static void init() {
        if (instance == null)
            instance = new RRepoSate();

        Repository.ALL.values().forEach((Repository<?> repo) -> {
            instance.localUpdate(null)
                    .set(KEY, repo.getKey())
                    .set(NAME, repo.getName())
                    .set(CRUDE, repo.config.crude.getArray(new CRUDE[0]))
                    .set(AUTO_UPDATE, !Boolean.TRUE.equals(repo.config.onDemand))
                    .set(ON_DEMAND, Boolean.TRUE.equals(repo.config.onDemand))
                    .set(REVISION, repo.status.getRevision())
                    .set(LAST_MODIFIED, repo.status.getLastUpdate())
                    .set(LAST_MODIFIED_BY, repo.status.getLastUpdatedBy())
                    .update();
        });

    }

    public static boolean canUpdate(Repository<?> repo) {
        if (instance == null)
            return false;
        if (!instance.has(repo.getKey()))
            return false;
        Record rec = instance.read(repo.getKey());
        return Boolean.TRUE.equals(rec.get(AUTO_UPDATE));
    }

}
