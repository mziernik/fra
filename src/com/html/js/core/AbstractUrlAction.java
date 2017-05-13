package com.html.js.core;

import com.html.core.tag.Element;
import com.servlet.requests.HttpRequest;
import com.utils.Url;

public abstract class AbstractUrlAction<Parent extends AbstractUrlAction> extends JsAction {

    protected final Url url;
    private HttpRequest http;
    private Boolean ssl;

    public AbstractUrlAction(String url) {
        this.url = new Url(url);
    }

    public AbstractUrlAction(Url url) {
        this.url = url;
    }

    public Parent ssl(Boolean ssl) {
        this.ssl = ssl;
        return (Parent) this;
    }

    public Parent param(Object name) {
        url.param(name);
        return (Parent) this;
    }

    public Parent param(String name, Object value) {
        url.param(name, value);
        return (Parent) this;
    }

    protected Url getUrl() {
        if (ssl != null) {
            HttpRequest http = this.http;
            if (http == null)
                http = HttpRequest.getInstance();
            return new Url(http.getAbsolutePath(url.toString(), ssl));
        }
        return url;
    }

    @Override
    public JsAction setTag(Element tag) {
        if (tag == null || url == null)
            return this;

        http = tag.getHTML().getController().http();

        return this;
    }

}
