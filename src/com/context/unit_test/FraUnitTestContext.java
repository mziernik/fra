package com.context.unit_test;

import com.context.AppContext;
import com.context.AppContextInitializer;
import com.context.WebAppContext;
import com.context.intf.ContextInitStage;
import com.servers.WebAppServer;
import com.servlet.Handlers;
import com.user.BaseUserData;

/**
 * @author Miłosz Ziernik
 * @date 10 grudnia 2015
 * @encoding UTF-8
 */
public final class FraUnitTestContext extends AppContext implements WebAppContext {

    public static boolean isRunning() {
        return instance instanceof FraUnitTestContext;
    }

    FraUnitTestContext(String[] args) throws Throwable {
        super(args);
    }

    @Override
    protected void onInitialize(ContextInitStage stage) throws Exception {
        if (stage == ContextInitStage.allDone) {
            BaseUserData root = new BaseUserData(null);
            root.username = "root";
            root.displayName = "ROOT";
            root.passPlain = "1234";
            Handlers.users.getInstance().editUser(root, true);
        }
    }

    @Override
    protected void config() throws Exception {

    }

}
