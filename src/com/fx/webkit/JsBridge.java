package com.fx.webkit;

import com.utils.Utils;
import com.utils.Is;
import com.mlogger.Log;
import netscape.javascript.JSObject;

/**
 *
 * @author milosz
 */
public interface JsBridge {

    default void consoleLog(Object text) {
        Log.info("WebKit", text);
    }

    default void consoleError(Object text) {
        Log.error("WebKit", text);
    }

    default void onError(String msg, String file, Integer line, Integer column, JSObject ex) {
        Log.error("WebKit", Utils.coalesce(msg, "") + " (" + file + ":" + line + ":" + column + ")");
    }

}
