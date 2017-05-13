package com.fx;

import com.context.*;
import com.dev.console.DevConsole;
import com.exceptions.CoreException;
import com.mlogger.Log;
import com.sun.javafx.application.LauncherImpl;
import com.thread.TThread;
import javafx.application.Application;
import javafx.stage.Stage;

public class FxApplication extends Application {

    static FxApplication instance;
    static FxContext context;
    static Stage stage;

    public static void init(String[] args, FxContext context) {
        FxApplication.context = context;
        if (context != AppContext.instance())
            throw new CoreException("FxContext <> AppContext");

        TThread.create("FX Launcher", (TThread thread) -> {
            LauncherImpl.launchApplication(FxApplication.class, args);
        }, (Throwable e) -> {
            AppContextInitializer.addInitError(e);
        }).start();
    }

    public FxApplication() {
        instance = this;
    }

    @Override
    public void start(Stage primaryStage) throws Exception {
        try {
            FxApplication.stage = stage;
            context.start(primaryStage);
        } catch (Throwable e) {
            AppContextInitializer.addInitError(e);
            throw e;
        }
    }

    public static void onException(Throwable e) {
        if (context != null)
            context.onException(e);
        else
            Log.error(e);
    }

}
