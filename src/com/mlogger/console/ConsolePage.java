package com.mlogger.console;

import com.html.core.tag.list.Ul;
import com.html.core.tag.semantic.Div;
import com.html.jquery.EnhSplitter;
import com.resources.Res;
import com.servlet.controller.Page;
import com.servlet.interfaces.Endpoint;
import com.servlet.requests.HttpRequest;

@Endpoint(url = "$/logs/console", title = "logi", view = ConsoleView.class)
public class ConsolePage extends Page {

    @Override
    public void onRequest(HttpRequest http) throws Exception {
        link(Res.utils, Res.filesSaver, Res.jQuery);
        link(Res.enhsplitter);
        link(Res.awesome);
        link(Res.popup);

        link("console.css");
        link("console.js");
        link("status.js");
        link("filters.js");
        link("highlight.js");
        link("export.js");

        Div main = body.div().id("main");

        main.div().div().id("filters");
//        Div center = main.div();
//
//        Div top = center.div().id("status");
        Div content = main.div().id("content");

        content.div().id("statuses");

        content.ul().id("console");

        //    main.span().id("postConsole");
        new EnhSplitter(main)
                .position("20%");

        new EnhSplitter(content)
                .vertical(false)
                .position("20%")
                .minSize(0);

        body.div().id("statusbar");

//        new EnhSplitter(center)
//                .position("10%")
//                .vertical(false);
    }

}
