package com.fx.webkit;

import com.context.AppContext;
import com.fx.FX;
import com.fx.webkit.webapp.Handler;
import com.mlogger.Log;
import com.thread.RunLater;
import java.util.LinkedList;
import java.util.List;
import javafx.application.Platform;
import javafx.beans.value.ObservableValue;
import javafx.concurrent.Worker;
import javafx.concurrent.Worker.State;
import javafx.scene.Scene;
import javafx.scene.web.*;
import javafx.stage.Stage;
import javafx.stage.StageStyle;
import javafx.util.Callback;
import netscape.javascript.JSException;
import netscape.javascript.JSObject;

public class WebKit implements Handler.ExceptionHandler {

    public final WebView webView = new WebView();
    public final WebEngine engine = webView.getEngine();
    private JSObject jsWindow;
    public final JsBridge bridge;
    public String indexPageUrl;
    private boolean ready;
    private final List<Runnable> onReady = new LinkedList<>();

    public WebKit(String indexPageUrl, JsBridge bridge) {
        this.bridge = bridge;
        this.indexPageUrl = indexPageUrl;

        if (indexPageUrl != null && indexPageUrl.startsWith("webapp://")) {
            Handler.register();
            Handler.exceptionHandlers.add(this);
        }

        final Worker<Void> loadWorker = engine.getLoadWorker();

        loadWorker.stateProperty().addListener(this::onStateChange);

        loadWorker.exceptionProperty().addListener(
                (ObservableValue<? extends Throwable> observable,
                        Throwable oldValue, Throwable newValue) -> {
                    onError(observable, oldValue, newValue);
                });

        engine.setUserDataDirectory(AppContext.varPath.getFile("webkit"));
        engine.setOnError(this::onError);
        engine.setCreatePopupHandler(this::onPopup);
        engine.setOnAlert(this::onAlert);

        if (AppContext.devMode)
            try {
                new DevToolsServer(engine.impl_getDebugger());
            } catch (Throwable ex) {
                Log.error(ex);
            }
    }

    public void onReady(Runnable runnable) {
        if (ready) {
            runnable.run();
            return;
        }
        synchronized (onReady) {
            onReady.add(runnable);
        }
    }

    public boolean isReady() {
        return ready;
    }

    public void reload() {
        engine.load(indexPageUrl);
    }

    protected void onAlert(WebEvent<String> event) {
        //  Log.info("WebKit|Alert", event.getData());
        FX.alertInfo(event.getData());
    }

    protected void onError(WebErrorEvent event) {
        FX.alertError(event.getMessage());
    }

    protected WebEngine onPopup(PopupFeatures p) {
        Stage stage = new Stage(StageStyle.UTILITY);
        WebView wv2 = new WebView();
        stage.setScene(new Scene(wv2));
        stage.show();
        return wv2.getEngine();
    }

    protected void onError(ObservableValue<? extends Throwable> observable,
            Throwable oldValue, Throwable newValue) {
        if (oldValue != null)
            oldValue.printStackTrace(System.err);
        if (newValue != null)
            newValue.printStackTrace(System.err);
    }

    protected void onStateChange(ObservableValue<? extends State> observable,
            State oldState, State newState) {

//        webView.getEngine().executeScript("if (!document.getElementById('FirebugLite')){E = document['createElement' + 'NS'] "
//                + "&& document.documentElement.namespaceURI;E = E ? document['createElement' + 'NS'](E, 'script') "
//                + ": document['createElement']('script');E['setAttribute']('id', 'FirebugLite');E['setAttribute']"
//                + "('src', 'https://getfirebug.com/' + 'firebug-lite.js' + '#startOpened');E['setAttribute']"
//                + "('FirebugLite', '4');(document['getElementsByTagName']('head')[0] || document['getElementsByTagName']"
//                + "('body')[0]).appendChild(E);E = new Image;E['setAttribute']('src', 'https://getfirebug.com/' + '#startOpened');}");
        if (newState == State.RUNNING) {
            jsWindow = (JSObject) engine.executeScript("window");
            jsWindow.setMember("app", bridge);
        }

        /*
                    this.message = source.message;
            this.file = source.filename;
            this.line = source.lineno;
            this.column = source.colno;
            this.stack = source.error.stack;
         */
        if (newState == State.SUCCEEDED)
            engine.executeScript(
                    "window.addEventListener('error', function(e){\n"
                    + "  app.onError(e.message ? e.message : null, "
                    + "e.filename ? e.filename : null, "
                    + "e.lineno ? e.lineno : null, "
                    + "e.column ? e.column : null, "
                    + "e.error ? e.error : null);"
                    + "});"
                    + "console.log = function(message){\n"
                    + "  app.consoleLog(message);"
                    + "};\n"
                    + "console.error = function(message){\n"
                    + "  app.consoleError(message);"
                    + "};"
            );
        if (newState == State.SUCCEEDED)
            synchronized (onReady) {
                for (Runnable runnable : onReady)
                    runnable.run();
            }

        ready = newState == State.SUCCEEDED;
    }

    public Object call(String string, Object... os) throws JSException {
        if (!ready)
            throw new JSException("Not ready");

        if (!Platform.isFxApplicationThread())
            throw new JSException("Not FX application thread");
        return jsWindow.call(string, os);
    }

    public Object eval(String string) throws JSException {
        if (!ready)
            throw new JSException("Not ready");
        if (!Platform.isFxApplicationThread())
            throw new JSException("Not FX application thread");
        return jsWindow.eval(string);
    }

    public Object getMember(String string) throws JSException {
        if (!ready)
            throw new JSException("Not ready");
        if (!Platform.isFxApplicationThread())
            throw new JSException("Not FX application thread");
        return jsWindow.getMember(string);
    }

    public void loadIndexPage() {
        FX.sync(() -> {
            engine.load(indexPageUrl);
        });

    }

    /**
     * Zdarzenie pochodzące z handlera (URLStreamHandler) protokołu webapp
     *
     * @param e
     * @param source
     * @param method
     */
    @Override
    public void onException(Throwable e, Object source, String method) {
        Log.error(e);
    }

}
