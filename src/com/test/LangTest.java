package com.test;

import com.html.core.tag.form.Select;
import com.html.js.Eval;
import com.lang.LConfig;
import com.lang.core.Language;
import com.lang.core.Languages;
import com.servlet.controller.Page;
import com.servlet.interfaces.Endpoint;
import com.servlet.requests.HttpRequest;

@Endpoint(url = "$langtest")
public class LangTest extends Page {

    @Override
    public void onRequest(HttpRequest http) throws Exception {

        body.h3(LConfig.LANG + ": " + http.session.language.get().name);

        Select select = body.select();

        for (Language lang : Languages.allLanguages.values())
            select.option(lang.name, lang.key).selected(lang == http.session.language.get());

        select.onChange(new Eval("document.cookie = 'language=' + this.value + ';expires=0;path=/'; location.reload();"));

    }

}
