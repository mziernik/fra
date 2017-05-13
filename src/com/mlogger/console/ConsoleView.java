package com.mlogger.console;

import com.utils.console.TConsole;
import com.utils.Utils;
import com.utils.Is;
import com.html.js.Call;
import com.html.js.core.JsAction;
import com.mlogger.Log;
import com.mlogger.utils.JsonBuilder;
import com.servlet.interfaces.JsMethod;
import com.servlet.views.ViewController;
import com.servlet.views.ViewsManager;
import com.utils.Unquoted;

public class ConsoleView extends ViewController {

    public boolean ready;

    @JsMethod
    public void logsReady() {
        // metoda wywolywana przez javascript gdy calosc jest juz zainicjowana
        JsonBuilder jb = new JsonBuilder();

        jb.obj('[');

        for (Log log : Console.getLogs())
            log.toJson(jb, null);
        jb.obj(']');

        try {
            for (ConsoleView view : ViewsManager.get(ConsoleView.class))
                view.push(new Call("logs.add", new Unquoted(jb.toString())));
        } catch (Throwable e) {
            TConsole.printErr(e);
        }
        ready = true;
    }

    @Override
    public void onError(Throwable e) {
        TConsole.printErr(e);
    }

    @Override
    public void onJsError(String url, String title, String message, String[] stack) {
        TConsole.printErr(message);
    }

}
