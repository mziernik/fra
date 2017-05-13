package com.fx;

import com.mlogger.Log;
import javafx.application.Application;
import javafx.stage.Stage;

public interface FxContext {

    default Application application() {
        return FxApplication.instance;
    }

    default Stage stage() {
        return FxApplication.stage;
    }

    public void start(Stage primaryStage) throws Exception;

    default void onException(Throwable e) {
        Log.error(e);
        FX.showException(e);
    }

}
