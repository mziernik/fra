package com.html.js;

import com.utils.text.StrWriter;
import com.html.core.tag.Element;
import com.html.js.core.JsAction;
import com.html.js.core.AbstractUrlAction;
import com.json.JObject;
import com.resources.Res;
import com.servlet.controller.Controller;
import com.utils.Unquoted;
import com.utils.Url;
import java.io.IOException;
import java.util.*;
import java.util.Map.Entry;

public class Ajax extends AbstractUrlAction<Ajax> {

    final JObject prompts = new JObject();
    final Map<String, Object> params = new LinkedHashMap<>();
    final Map<String, String> headers = new LinkedHashMap<>();
    public final Busy busy = new Busy();
    private boolean top;

    private final JsActions onDone = new JsActions();
    private final JsActions onError = new JsActions();

    private final JObject json = new JObject();

    public Ajax(String url) {
        super(url);
        json.options
                .acceptNulls(false)
                .javascriptMode(true);

    }

    public Ajax(Url url) {
        super(url);
        json.options
                .acceptNulls(false)
                .javascriptMode(true);
    }

    public Ajax(Class<? extends Controller> cls) {
        this(new Url(cls));
    }

    public Ajax(Controller page) {
        this(new Url(page));
    }

    public Ajax reload(boolean reload) {
        json.put("reload", reload);
        return this;
    }

    public Ajax contentType(String contentType) {
        json.put("contentType", contentType);
        return this;
    }

    public Ajax formId(String formId) {
        json.put("form", formId);
        return this;
    }

    /**
     * @param formId Obiekt formularza, np this
     * @return
     */
    public Ajax formObj(String formId) {
        json.put("form", new Unquoted(formId));
        return this;
    }

    public Ajax addHeader(String name, String value) {
        headers.put(name, value);
        return this;
    }

    public Ajax postParam(String name, Object value) {
        params.put(name, value);
        return this;
    }

    /**
     * Dabe, które będą przesłane jako post. Warto przy okazji ustawić
     * Content-Type
     *
     * @param post
     * @return
     */
    public Ajax post(String post) {
        json.put("post", post);
        return this;
    }

    /**
     * Nie będą wyświetlane komunikaty błedów, wyszarzenie ani animacja
     * oczekiwania.
     *
     * @param silent
     * @return
     */
    public Ajax silent(boolean silent) {
        json.put("silent", silent);
        return this;
    }

    public Ajax skipLog(boolean skipLog) {
        json.put("skipLog", skipLog);
        return this;
    }

    /**
     * Zdarzenie, które zostanie wykonane w momecie odebrania prawidłowego
     * statusu odpowiedzi
     *
     * @param onDone
     * @return
     */
    public Ajax onDone(JsAction... actions) {
        this.onDone.add(actions);
        return this;
    }

    /**
     * Zdarzenie, które wykona się w momencie zwrócenia przez serwer kodu błedu
     (<200 lub > 399). W momencie zadeklarowania tego zdarzenia pominięty
     * zostaje standardowy komunikat błedu.
     *
     * @param actions
     * @return
     */
    public Ajax onError(boolean showErrorDialog, JsAction... actions) {
        onError.add(new Function(null)
                .body(actions).body("return " + showErrorDialog + ";")
        );
        return this;
    }

    /**
     * Treść komunikatu, który użytkownik musi zaakceptować przed wysłaniem
     * żądania
     *
     * @param confirm
     * @return
     */
    public Ajax confirm(String confirm) {
        json.put("confirm", confirm);
        return this;
    }

    public Ajax prompt(String paramName, String message, String defaultValue) {
        JObject json = prompts.objectC(paramName);
        json.put("message", message);
        json.put("default", defaultValue);
        return this;
    }

    @Override
    public void getContent(StrWriter writer) {

        if (writer.isCompact())
            json.options.singleLine(writer.isCompact());

        if (!prompts.isEmpty())
            json.add("prompt", prompts);

        if (!params.isEmpty()) {
            JObject jparams = json.objectC("params");
            for (Entry<String, Object> en : params.entrySet())
                jparams.put(en.getKey(), en.getValue());
        }

        if (!headers.isEmpty()) {
            JObject jheaders = json.objectC("headers");
            for (Entry<String, String> en : headers.entrySet())
                jheaders.put(en.getKey(), en.getValue());
        }

        if (!onError.isEmpty()) {
            StrWriter sw = new StrWriter();
            onError.getContent(sw);
            json.put("onError", new Unquoted(sw.toString()));
        }

        writer.append(top ? "top." : "")
                .append("ajax.post('")
                .append(getUrl().toString())
                .append("'");

        if (!json.isEmpty()) {
            writer.append(", ");
            json.getContent(writer);
        }

        if (!onDone.isEmpty()) {
            writer.append(", ");
            new Function(null, "http").body(onDone).getContent(writer);
        }
        writer.append(")");
    }

    public Ajax top(boolean top) {
        this.top = top;
        return this;
    }

    @Override
    public JsAction setTag(Element tag) {
        super.setTag(tag);
        link(tag, Res.utils);
        onDone.setTag(tag);
        onError.setTag(tag);
        return this;
    }

}
