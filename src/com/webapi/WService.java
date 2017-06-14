package com.webapi;

import com.model.dataset.AbstractDataSet;
import com.model.repository.WRepository;
import com.exceptions.EError;
import com.json.JObject;
import com.model.dataset.DataSet;
import com.utils.Is;
import com.utils.collections.Pair;
import com.webapi.core.*;
import java.io.FileNotFoundException;

public class WService implements WebApi {

    @WebApiEndpoint
    public final WNotifications notifications = new WNotifications();

    @WebApiEndpoint
    public final WRepository repository = new WRepository();

    @WebApiEndpoint
    public final WHttpSessions httpSession = new WHttpSessions();

    @WebApiEndpoint
    public final WSession session = new WSession();

    public JObject record() {
        JObject json = new JObject();

        return json;
    }

    @WebApiEndpoint(dataType = DataType_old.ARRAY, description = "Zwraca wszystko co może się przydać")
    public JObject getData(WebApiRequest req) throws Exception {
        JObject json = new JObject();

        for (String methodName : req.getJson().asArray().getValuesStr()) {
            Pair<WebApiControllerMeta, WebApi> endp = req.controller.getEndpoint(req, methodName);
            if (endp == null)
                throw new FileNotFoundException(methodName);

            try {

                Object result = endp.first.invoke(req, endp.second);

                result = Is.instanceOf(result, DataSet.class, (ds) -> ds.getJson());

                json.put(methodName, result);

            } catch (Throwable e) {
                EError.addDetails(e, "method", methodName);
                throw e;
            }
        }

        return json;
    }

}
