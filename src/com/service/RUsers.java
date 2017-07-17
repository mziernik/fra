package com.service;

import com.json.JObject;
import com.model.repository.Column;
import com.model.repository.Record;
import com.model.repository.Repository;
import com.model.repository.intf.CRUDE;
import com.model.repository.intf.CaseConvert;
import com.resources.FontAwesome;
import com.resources.dict.Names;
import com.utils.StrUtils;
import com.utils.TObject;
import com.utils.collections.TList;
import com.utils.reflections.datatype.DataType;
import com.webapi.core.WebApiController;
import java.util.Random;
import java.util.UUID;

public class RUsers extends Repository<Long> {

    public static RUsers instance;

    public final static Column<Long> ID = new Column<>(c -> {
        c.repository = RUsers.class;
        c.type = DataType.LONG;
        c.key = "id";
        c.name = "ID";
        c.autoGenerated = true;
    });

    public final static Column<UUID> TOKEN = new Column<>(c -> {
        c.repository = RUsers.class;
        c.type = DataType.UUID;
        c.key = "token";
        c.name = "Zewnętrzny token";
        c.daoName = "external_token";
        c.hidden = true;
    });

    public final static Column<String> LOGIN = new Column<>(c -> {
        c.repository = RUsers.class;
        c.type = DataType.STRING;
        c.key = "login";
        c.name = "Login";
        c.daoName = "username";
        c.required = true;
        c.unique = true;
        c.caseConvert = CaseConvert.LOWER;
    });

    public final static Column<String> PASSWORD = new Column<>(c -> {
        c.repository = RUsers.class;
        c.type = DataType.PASSWORD;
        c.key = "pass";
        c.name = "Hasło";
        c.daoName = "password";
        c.trimmed = false;
        c.hidden = true;
    });

    public final static Column<Boolean> LDAP = new Column<>(c -> {
        c.repository = RUsers.class;
        c.type = DataType.BOOLEAN;
        c.key = "ldap";
        c.name = "Autoryzacja LDAP";
        c.daoName = "ldap_auth";
        c.defaultValue = false;
    });

    public final static Column<String> FIRST_NAME = new Column<>(c -> {
        c.repository = RUsers.class;
        c.type = DataType.STRING;
        c.key = "firstName";
        c.name = "Imię";
        c.daoName = "first_name";
    });

    public final static Column<String> LAST_NAME = new Column<>(c -> {
        c.repository = RUsers.class;
        c.type = DataType.STRING;
        c.key = "lastName";
        c.name = "Nazwisko";
        c.daoName = "last_name";
    });

    public final static Column<String> DISPLAY_NAME = new Column<>(c -> {
        c.repository = RUsers.class;
        c.type = DataType.STRING;
        c.key = "displayName";
        c.name = "Nazwa wyświetlana";
        c.daoName = "display_name";
        c.required = true;
    });

    public final static Column<String> EMAIL = new Column<>(c -> {
        c.repository = RUsers.class;
        c.type = DataType.EMAIL;
        c.key = "email";
        c.name = "e-mail";
        c.caseConvert = CaseConvert.LOWER;
    });

    public RUsers() {
        super(c -> {
            c.primaryKey = ID;
            c.displayName = LOGIN;
            c.key = "users";
            c.name = "Użytkownicy";
            c.daoName = "users.users";
            c.crude.set(CRUDE.CRU);
            c.icon = FontAwesome.USERS;

            c.recordAction("add", "Dodaj", ActionType.PRIMARY,
                    FontAwesome.USER_PLUS, null, (repo, rec, params) -> {

                    });

            c.recordAction("rem", "Usuń", ActionType.DANGER,
                    FontAwesome.USER_TIMES, "Czy na pewno usunąć?", (repo, rec, params) -> {

                    });

            c.repoAction("addRandom", "Dodaj losowy", ActionType.PRIMARY,
                    FontAwesome.USER_PLUS, null, (repo, params) -> {
                        RUsers r = (RUsers) repo;

                        String fname = Names.randomFirstname();
                        String sname = Names.randomSurname();

                        String login = StrUtils.convertPolishChars(fname).toLowerCase()
                        + "." + StrUtils.convertPolishChars(sname).toLowerCase();

                        r.localUpdate(null)
                                .set(ID, repo.min(ID, 0l) - 1)
                                .set(LOGIN, login)
                                .set(EMAIL, login + "@email.com")
                                .set(FIRST_NAME, fname)
                                .set(LAST_NAME, sname)
                                .set(DISPLAY_NAME, fname.charAt(0) + ". " + sname)
                                .update();

                    });

            c.repoAction("removeRandom", "Usuń losowy", ActionType.DANGER,
                    FontAwesome.USER_TIMES, null, (Repository<Long> repo, JObject params) -> {

                        RUsers r = (RUsers) repo;

                        TList<Long> ids = new TList<>();
                        repo.forEach((Record rec) -> {
                            if (rec.get(ID) < 0)
                                ids.add(rec.get(ID));
                            return true;
                        });

                        if (ids.isEmpty())
                            return;

                        r.localUpdate(ids.random()).delete();

                    });

        });
        instance = this;
    }

}
