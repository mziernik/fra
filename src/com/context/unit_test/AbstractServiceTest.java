package com.context.unit_test;

import com.utils.StrUtils;
import com.utils.Utils;
import com.utils.Is;
import com.context.*;
import com.exceptions.EError;
import com.exceptions.ThrowableException;
import com.exceptions.http.HttpException;
import com.json.JArray;
import com.mlogger.Log;
import com.net.HttpClient;
import com.servers.Connector;
import com.servers.WebAppServer;
import com.servlet.controller.Controller;
import com.servlet.controller.ControllerMetaData;
import com.utils.Url;
import com.utils.reflections.TClass;
import java.io.IOException;
import java.lang.reflect.Constructor;
import org.junit.Assert;
import org.junit.FixMethodOrder;
import org.junit.Ignore;
import org.junit.runner.RunWith;
import org.junit.runners.MethodSorters;

/**
 * @author Miłosz Ziernik
 * @param <Ctx>
 * @date 08 stycznia 2016
 * @encoding UTF-8
 */
@Ignore
@FixMethodOrder(MethodSorters.NAME_ASCENDING)
@RunWith(ServiceRunner.class)
public class AbstractServiceTest<Ctx extends AppContext> extends Assert {

    private static AppContext context;
    static boolean webAppServerRequired;

    private static boolean initialized;

    public static void require(Class<?> cls) {
        try {
            Class.forName(cls.getName(), true, cls.getClassLoader());
        } catch (ClassNotFoundException ex) {
            throw new ThrowableException(ex);
        }
    }

    protected AbstractServiceTest(Class<Ctx> contextClass) {
        if (initialized)
            return;

        initialized = true;

        try {
            WebAppServer.disabled = !webAppServerRequired && !(this instanceof WebAppServerRequired);
            context = new TClass<Ctx>(contextClass).newInstance(context, (Object) new String[0]);
            if (WebAppServer.disabled)
                AppContextInitializer.initialize();
            AppContext.waitForContext();

        } catch (Throwable e) {
            throw new ThrowableException(e);
        }

    }

    protected static void registerController(Class<? extends Controller> controller) {
        new ControllerMetaData(controller, false);
    }

    public Ctx getContext() {
        return (Ctx) context;
    }

    public Url getUrl(String relativePath) {
        Connector conn = getConnector(null);
        Url url = conn.getUrl();
        url.path().add(relativePath);
        return url;
    }

    protected Connector getConnector(Boolean https) {
        for (Connector conn : AppConfig.server.connectors) {
            if (https == null)
                return conn;

            if (https && conn.isSecure())
                return conn;

            if (!https && !conn.isSecure())
                return conn;

        }
        throw new UnsupportedOperationException("Nie znaleziono konektora");
    }

    public Url getUrl(String relativePath, Boolean https) {

        return new Url("http://127.0.0.1:49682/" + relativePath);
    }

    public String read(HttpClient http) throws IOException, HttpException {
        int code = http.getResponseCode();

        String err = http.getHeader("Error");
        if (err != null) {
            err = StrUtils.decodeURIComponent(err);

            JArray arr = JArray.parse(err);
            Throwable e = EError.getException(arr.getValuesStr().first());
            Log.error(e);
            throw new ThrowableException(e);
        }

        if (code != 200)
            throw new HttpException(code, http.getResponseMessage());

        return http.readStr(Utils.UTF8);

    }

}
