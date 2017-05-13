package com.context.fra;

import com.context.AppContext;
import com.context.WebAppContext;
import com.context.fra.handler.Auth;
import com.servlet.Handlers;
import com.webapi.WApi;
import com.webapi.core.WebApiControllerMeta;

@javax.servlet.annotation.WebListener
public class Framework extends AppContext implements WebAppContext {

    public static boolean isRunning() {
        return AppContext.instance instanceof Framework;
    }

    @Override
    protected void config() throws Exception {
        Handlers.auth.setHandler(Auth.class);

        WebApiControllerMeta.autoGenerateJsClient("com/service/serviceApi.js", WApi::new);

        // logger.addHandler(new MLogFileHandler());
    }

    public static void main(String[] args) throws Exception {

        new Framework(args);
    }

    public Framework(String[] args) {
        super(args);

    }

    public Framework() {
        super(null);
    }

}
