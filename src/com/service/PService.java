package com.service;

import com.html.HtmlPackage;
import com.resources.Res;
import com.resources.core.html.ResourceFile;
import com.servlet.controller.Page;
import com.servlet.interfaces.Endpoint;
import com.servlet.requests.HttpRequest;

@Endpoint(url = "$")
public class PService extends Page {

    @Override
    public void onRequest(HttpRequest http) throws Exception {

        new HtmlPackage(this)
                //    .add("/res/jsx/react.js")
                //    .add("/res/jsx/react-dom.js")
                //    .add("/res/jsx/babel-core.js")
                //-----------------------------------
                .add("serviceApi.js")
                .add("/res/webApi.js")
                .add("/res/jquery/jquery.js")
                .add("/res/jquery/jquery-ui.js")
                .add("/res/jquery/jquery-ui.css")
                .add("/res/jquery/enhsplitter/enhsplitter.js")
                .add("/res/jquery/enhsplitter/enhsplitter.css")
                .add("/res/bootstrap/css/bootstrap.css")
                .add("/res/bootstrap/js/bootstrap.min.js")
                .add("/res/font-awesome/font-awesome.css")
                .add("/res/swal/sweetalert.js")
                .add("/res/swal/sweetalert.css")
                .add("/res/moment/moment-with-locales.min.js")
                .add("/res/moment/moment-timezone.min.js")
                .add("/res/bootstrap/dialog/bootstrap-dialog.min.js")
                .add("/res/bootstrap/dialog/bootstrap-dialog.min.css")
                .add("/res/bootstrap/datetimepicker/bootstrap-datetimepicker.min.js")
                .add("/res/bootstrap/datetimepicker/bootstrap-datetimepicker.min.css")
                //--------------------------------------------------------------
                .add(Res.dsTable)
                .add(Res.dsTree)
                .add("/res/popup/popup.js")
                .add("/res/popup/popup.css")
                //--------------------------------------------------------------
                .add("/res/ace/ace.js")
                .add("/res/ace/ace_langs.js")
                .add("events/filterpanel.js")
                .addDir(getClass().getPackage());

        String skin = http.getCookie("skin", "dark");

        body.attr("skin", skin);
        if ("dark".equals(skin))
            body.style().backgroundColor("#3a3633");

    }
}
