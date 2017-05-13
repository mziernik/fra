package com.servlet.page;

import com.config.CService.CTechnicalBreak;
import com.html.core.styles.TextAlign;
import com.html.core.tag.semantic.Div;
import com.servlet.controller.Page;
import com.servlet.interfaces.Endpoint;
import com.servlet.requests.HttpRequest;

@Endpoint(title = "Przerwa techniczna")
public class PTechnicalBreak extends Page {

    @Override
    public void onRequest(HttpRequest http) throws Exception {

        Div div = body.div();
        div.style().textAlign(TextAlign.center);

        div.h3().textToTags(Div.class, CTechnicalBreak.message.value());

        div = body.div();
        div.style().textAlign(TextAlign.center);
        div.h4().textToTags(Div.class, CTechnicalBreak.details.value());

    }

}
