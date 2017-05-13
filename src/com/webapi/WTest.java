package com.webapi;


import com.cache.CachedData;
import com.json.*;
import com.resources.dict.Names;
import com.resources.dict.Passwords;
import com.servlet.interfaces.Arg;
import com.utils.Utils;
import com.webapi.core.*;
import com.lang.LWebapi;
import com.model.dataset.AbstractDataSet;
import com.model.dataset.DataSet;
import com.utils.TObject;
import java.io.IOException;
import java.util.*;

public class WTest implements WebApi {

    final static List<ArrayList<Object>> names = new ArrayList<>(10000);

    public WTest(WebApiController ctrl) {
        ctrl.service.notifications.notifySources.put("users", LWebapi.USERS_MODIFICATIONS);
    }

    @WebApiEndpoint
    public AbstractDataSet dict(@Arg(name = "offset", required = false) Integer offset) throws IOException {

        offset = Utils.range(offset, 0, 10_000);
        int limit = 300;

        DataSet<ArrayList<Object>, Integer> data = new DataSet<>("dict", LWebapi.NAME_DICTIONARY);

        data.offset = offset;
        data.limit = limit;

        data.column(Integer.class, "id", LWebapi.ID,
                arr -> (Integer) arr.get(0))
                .primaryKey();

        data.column(String.class, "firstname", LWebapi.FIRSTNAME,
                arr -> (String) arr.get(1));

        data.column(String.class, "lastname", LWebapi.LASTNAME,
                arr -> (String) arr.get(2));

        if (names.isEmpty())
            for (int i = 0; i < 10000; i++) {
                ArrayList<Object> list = new ArrayList<>();
                list.add(i + 1);
                list.add(Names.randomFirstname());
                list.add(Names.randomSurname());
                names.add(list);
            }

        int cnt = 0;
        for (int i = offset; i < names.size(); i++) {
            data.fillRow(names.get(i));
            if (++cnt >= limit)
                break;
        }

        return data;
    }

    @WebApiEndpoint
    public void usersUpdate(@Arg(name = "action") String action) {

        JArray json = new JArray();
        Random rand = new Random();

        switch (action) {
            case "update":
            case "insert":
                for (int i = 0; i < 1 + rand.nextInt(4); i++) {
                    JObject obj = json.object();

                    obj.put("id", action.equals("update") ? 2 + rand.nextInt(12) : 20 + rand.nextInt(300));
                    obj.put("firstname", Names.randomFirstname());
                    obj.put("lastname", Names.randomSurname());
                    obj.put("ldap", rand.nextBoolean());
                }

                break;

            case "remove":
                JObject obj = json.object();
                obj.put("id", (2 + rand.nextInt(12)));
                break;

        }

        new WebApiBroadcast("users", "update", json).send();
    }

    @WebApiEndpoint
    public AbstractDataSet numbers() throws IOException {
        /*
        AbstractDataSet data = new AbstractDataSet("numbers", LWebapi.PREMIUM_NUMBERS);

        data.column(Integer.class, "key", LWebapi.ID).align(DSColumnAlign.RIGHT).primaryKey(true);
        data.column(DataType.BOOLEAN, "enabled", LWebapi.ACTIVE).hidden(true);
        data.column(DataType.ARRAY, "numbers", LWebapi.NUMBERS).searchable(true);
        data.column(String.class, "name", LWebapi.NAME).searchable(true);
        data.column(DataType.DATE, "added", LWebapi.ADDES).searchable(true);

        for (JArray arr : JSON.parse(Resources.get("/com/webapi/test/numbers.json")).asArray().getArrays())
            data.addRow().addAll(arr.asRawCollection());
        return data;
         */
        return null;
    }

    @WebApiEndpoint
    public AbstractDataSet users(WebApiRequest request) throws IOException {
        /*
        AbstractDataSet data = new AbstractDataSet("users", LWebapi.USERS)
                .updatable(true)
                .sortable(true)
                .selectable(true);

        data.column(Integer.class, "key", LWebapi.USER_ID)
                .subtitle("INT")
                .align(DSColumnAlign.RIGHT)
                .primaryKey(true);

        data.column(DataType.ENUM, "type", LWebapi.TYPE)
                .subtitle("ENUM")
                .searchable(true)
                .align(DSColumnAlign.CENTER);

        data.column(DataType.ENUM, "status", LWebapi.STATUS)
                .subtitle("ENUM")
                .searchable(true)
                .align(DSColumnAlign.CENTER);

        data.column(String.class, "login", LWebapi.LOGIN)
                .subtitle("STRING")
                .unique(true)
                .searchable(true);

        data.column(String.class, "firstname", LWebapi.FIRSTNAME)
                .subtitle("STRING")
                .searchable(true);

        data.column(String.class, "lastname", LWebapi.LASTNAME)
                .subtitle("STRING")
                .searchable(true);

        data.column(String.class, "displayName", LWebapi.NAME)
                .subtitle("STRING")
                .searchable(true);

        data.column(String.class, "email", LWebapi.EMAIL)
                .subtitle("STRING")
                .searchable(true);

        data.column(DataType.BOOLEAN, "ldap", LWebapi.LDAP)
                .subtitle("BOOLEAN")
                .align(DSColumnAlign.CENTER);

        data.column(String.class, "passwd", LWebapi.PASSWORD)
                .subtitle("STRING")
                .hidden(true);

        data.column(DataType.DATE, "created", LWebapi.CREATED)
                .subtitle("DATE")
                .searchable(true)
                .dateFormat("yyyy-MM-dd HH:mm:ss", "yyyy-MM-dd HH:mm:ss", "dd MMMM yyyy HH:mm");

        data.column(DataType.BOOLEAN, "incontactbook", LWebapi.CONTACT_BOOK)
                .subtitle("BOOLEAN")
                .hidden(true);

        data.column(DataType.ARRAY, "groups", LWebapi.GROUPS)
                .subtitle("ARRAY")
                .hidden(true);

        data.column(DataType.ARRAY, "accounts", LWebapi.ACCOUNTS)
                .subtitle("ARRAY")
                .hidden(true)
                .searchable(true);

        data.column(Integer.class, "calls", LWebapi.CALLS)
                .subtitle("INT")
                .hidden(true);

        data.column(Integer.class, "activity", LWebapi.ACTIVITY)
                .align(DSColumnAlign.CENTER);

        for (JArray arr : JSON.parse(Resources.get("/com/webapi/test/users.json")).asArray().getArrays())
            data.addRow().addAll(arr.asList());

        return data;
         */
        return null;
    }

    @WebApiEndpoint
    public CachedData file() throws IOException {
        return new CachedData("bla", "bbb", "dict.zip", 300,
                Passwords.class.getResourceAsStream("dict.zip"));
    }

    @WebApiEndpoint
    public void event(WebApiRequest req) throws IOException {

    }

    @WebApiEndpoint(description = "Test metody cancel webapi - jeśli anulowanie nie nastepi w ciągu ok 10s, usługa zgłosi błąd")
    public void cancel(WebApiRequest req) throws Exception {

        TObject<String> event = new TObject<>();
        req.onEvent((name, data) -> {
            req.info("Zdarzenie " + name + ": " + data);
        });

        for (int i = 0; i < 1000; i++) {
            Thread.sleep(10);
            if (req.isCancelled())
                return;
        }

        throw new Error("Timeout");
    }

    @WebApiEndpoint
    public void broadcast(WebApiRequest req) throws IOException {

    }

    @WebApiEndpoint
    public void messages(WebApiRequest req) throws IOException {
//        req.success("Success");
//        req.warning(null, "Warning");
//        req.error(null, "Error");
    }

    @WebApiEndpoint
    public void hints(WebApiRequest req) throws IOException {
//        req.successHint("Success");
//        req.warningHint("Warning");
//        req.errorHint("Error");
    }

    @WebApiEndpoint
    public String delay(WebApiRequest req) throws IOException, InterruptedException {
        Thread.sleep(3000);
        // req.success("OK");
        return Utils.randomId();
    }
}
