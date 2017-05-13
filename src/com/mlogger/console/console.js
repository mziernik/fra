


var Logs = function () {
    "use strict";

    this.TOTAL_LIMIT = 10000; //10000;
    this.DISPLAY_LIMT = 500; // 500;
    var console = this.console = $id("console");
    var filters = this.filters = new Filters(this);
    this.all = []; // lista wszystkich logów (TLog[])
    var statusBar = this.statusBar = $id("statusbar");


    var addQueue = [];
    var addTimeout;
    this.processedLogs = 0;

    this.add = function (logs) {

        var self = window.logs;

        if (!logs)
            return;

        self.busy(true);

        if (typeof logs === "string")
            logs = JSON.parse(logs);

        if (logs instanceof Array)
            addQueue = addQueue.concat(logs);
        else
            addQueue.push(logs);

        clearTimeout(addTimeout);
        addTimeout = setTimeout(function () {

            try {
                self.busy(true);

                addQueue.forEach(function (src) {
                    var log = new TLog(src);
                    self.all.push(log);
                    filters.onNewLog(log);
                });

                addQueue = [];

                var overflow = self.all.length - self.TOTAL_LIMIT;
                if (overflow > 0)
                    self.all.splice(0, overflow);

                if (!filters.paused)
                    filters.filterAll(false);
                else
                    statusBar.txt("Widocznych logów: " + console.children.length
                            + ",  Łącznie: " + self.all.length + ", przetworzono: "
                            + self.processedLogs);

            } finally {
                self.busy(false);
            }

        });

    };


    var TLog = function (log) {
        this.log = log;
        var self = this;
        this.visible = null; // true/false/null

        this.tmain = document.createElement("li");
        this.tmain.log = this;

        this.build = function (li) {
            var _export = li && li !== this.tmain;
            li = li || this.tmain;

            var now = new Date().toISOString().split('T')[0];

            if (li.children.length > 0)
                return true;


            if (log.knd)
                li.cls("lk-" + log.knd);

            var div = li.tag("div")
                    .cls("log-line");


            div.tag("span")
                    .cls("log-line-id")
                    .txt(log.cnt + ".");

            var date = log.dte;
            if (!_export && date.indexOf(now) === 0)
                date = date.substring(now.length).trim();

            div.tag("span")
                    .cls("log-line-date")
                    .attr({title: _export ? undefined : date})
                    .txt(date);

            var stag = div.tag("span");

            if (log.tag && log.tag.length)
                stag.cls("log-line-tags")
                        .txt(log.tag ? "[" + log.tag.join(", ") + "]" : null);

            var tag = div.tag("span").cls("log-line-value");
            if (log.val && log.val.length === 3) {
                var val = log.val[2];
                if (!val)
                    val = "";
                if (val.length > 150)
                    val = val.substring(0, 150).trim() + " […]";
                val = val.replaceAll("\n", " ↵ ");
                tag.txt(val)
                        .attr({title: _export ? undefined : log.val[2]});
            }

            // komentarz
            if (log.com) {
                div.tag("span").cls("log-line-comment")
                        .txt(log.com)
                        .attr({title: _export ? undefined : log.com});
            }

            if (_export) {

                div.attr("onclick", "showDetails(this)");

            }

            li.oncontextmenu = showPopupMenu;

            div.onclick = function (e) {
                var div = e.currentTarget.nextSibling;

                if (div.children.length === 0)
                    self.buildDetails(div);

                $(div).slideToggle(200, function () {
                    if ($(this).is(":visible"))
                        $(li).attr("expanded", true);
                    else
                        $(li).removeAttr("expanded");

                    if (filters.searchPhrase)
                        $('#console').highlight(filters.searchPhrase);

                });
            };



            //-----------------------

            var div = li.tag("div");
            div.cls("log-details");
            if (_export)
                this.buildDetails(div);

        };

        this.buildDetails = function (div) {
            var log = this.log;
            function arg(name, value) {
                if (!value)
                    return;

                var tr = tbl.tag("tr");
                tr.tag("td").txt(name + ":");
                tr.tag("td").txt(value instanceof Array ? value.join(", ") : value);
            }

            function data(name, value) {
                if (value && !value[2])
                    return;

                var tag = div.tag("div").cls("log-details-data");

                tag.tag("div")
                        .txt(name)
                        .onclick = function (e) {
                            $(e.currentTarget.nextSibling)
                                    .slideToggle(200);
                        };

                if (value)
                    tag.tag("pre").txt(value ? value[2] : null);

                return tag;
            }

            function stack(name, err, value) {
                if (!value)
                    return;
                var ul = data(name, null).tag("ul");

                if (err)
                    ul.cls("log-details-data-error-stack");
                else
                    ul.cls("log-details-data-call-stack")
                            .css({display: "none"});

                value.forEach(function (s) {
                    var li = ul.tag("li");

                    if (s === "--") {
                        li.tag("hr");
                        return;
                    }

                    var own = s.indexOf("*") === 0;
                    var fra = s.indexOf("+") === 0;
                    if (own || fra) {
                        s = s.substring(1);
                        li.setAttribute(own ? "own" : "fra", true);
                        if (window.app) {
                            li.onclick = function () {
                                window.app.showSourceFile(s);
                            };
                            li.setAttribute("src-file", true);
                        }
                    }
                    li.txt(s);


                });
                return ul;
            }

            var tbl = data("Atrybuty", null).tag("div").tag("table").tag("tbody");

            arg("Data", log.dte);
            arg("Rodzaj", log.knd);
            arg("Poziom", log.lvl + ", " + log.lvn);

            arg("Źródło", log.src);
            arg("Wersja", log.ver);
            arg("Adres", log.adr);
            arg("System operacyjny", log.os);
            arg("Urządzenie", log.dev);
            arg("Host", log.hst);
            arg("User Agent", log.ua);
            arg("Użytkownik", log.usr);

            arg("Instancja", log.ist);
            arg("Żądanie", log.req);
            arg("Sesja", log.ses);
            arg("URL", log.url);

            arg("Metoda", log.mth);
            arg("Klasa", log.cls);
            arg("Tryb", log.mde);

            arg("Id procesu", log.prc);
            arg("Wątek", log.thr + ", " + log.thp + ", " + log.thn);

            arg("logger", log.lgr);
            arg("UID", log.uid);
            arg("Komentarz", log.com);

            arg("Licznik", log.cnt);
            arg("Klucze", log.key);

            arg("Kolor", log.fcl);
            arg("Tło", log.bcl);

            arg("Tag", log.tag);

            //-----------------
            if (log.atr) {

                var defTable = tbl;

                for (var name in log.atr) {

                    tbl = name ? data(name, null).tag("div").tag("table").tag("tbody") : defTable;

                    log.atr[name].forEach(function (atr) {
                        arg(atr[0], atr[1]);
                    });
                }
            }

            data("Wartość", log.val);

            if (log.dta)
                log.dta.forEach(function (dta) {
                    data(dta[1], dta);
                });

            stack("Stos błędów", true, log.est);
            stack("Stos wywołań", false, log.cst);
        };

        var showPopupMenu = function () {
            var pmMain = new PopupMenu();

            var byDate = function (older) {
                var input = filters.groups.date.mainTag
                        .getElementsByTagName("input")[older ? 0 : 1];
                filters.groups.date.expand(true);
                input.value = log.dte;
                input.onkeydown({
                    keyCode: 13,
                    srcElement: input
                });
            };

            pmMain.add("Pokaż starsze niż " + log.dte, function () {
                byDate(true);
            });

            pmMain.add("Pokaż młodsze niż " + log.dte, function () {
                byDate(false);
            });

            /*  var pmKind = pmMain.add("Rodzaj \"" + log.knd + "\"");
             pmKind.add("Pokaż wyłącznie");
             pmKind.add("Wyklucz");
             
             var pmSource = pmMain.add("Źródło \"" + log.src + "\"");
             
             
             pmSource.add("Pokaż wyłącznie");
             pmSource.add("Wyklucz");
             
             var pmSource = pmMain.add("Adres \"" + log.adr + "\"");
             var pmSource = pmMain.add("Użytkownik \"" + log.usr + "\"");
             
             var pmSave = pmMain.add("Zapisz do pliku");
             */

            pmMain.separator();
            pmMain.add("Zapisz do pliku", function () {
                saveToFile();
            });

            pmMain.show();
            return false; // onContext
        };
    };



    this.changeVisibility = function () {
        var preConsole = console.parentNode;
        var scrollToEnd = preConsole.scrollHeight - preConsole.scrollTop
                - preConsole.clientHeight <= 50;

        var list = [];
        $.each(this.all, function (index, log) {
            if (log.visible)
                list.push(log);
        });

        // ------------- usun zbedne elementy -------------
        var toRemove = [];

        $.each(console.children, function (index, tag) {
            if (list.indexOf(tag.log) < 0)
                toRemove.push(tag.log);
        });

        $.each(toRemove, function (index, log) {
            console.removeChild(log.tmain);
        });

        // ---------------------------------------

        var dst = console.children[0];

        while (dst) {
            var tag = list.splice(0, 1)[0].tmain;
            if (tag !== dst)
                console.insertBefore(tag, dst);
            else
                dst = dst.nextSibling;
        }


        // dodaj pozostale tagi
        $.each(list, function (index, log) {
            console.appendChild(log.tmain);
        });


        //console.appendChild(tag);

        logs.statusBar.txt("Widocznych logów: " + console.children.length
                + ",  Łącznie: " + this.all.length + ", przetworzono: "
                + logs.processedLogs);

        if (scrollToEnd)
            preConsole.scrollTop = preConsole.scrollHeight;

        //     document.title = "Logów: " + cnt + " / " + logs.length;
    };


    var busyTag; //tag ikony (div)

    this.busy = function (state) {

        if (!busyTag) {
            busyTag = document.body.tag("div")
                    .cls("busy");
            new SVG(busyTag, 64, 64, "0 0 453.872 453.871")
                    .tag("path")
                    .attr("fill", "#666")
                    .attr("fill-opacity", "0.6")
                    .attr("d", "M369.822,42.794h17.744V0H66.305v42.794h17.746v11.105c0,69.716,23.859,133.656,63.155,171.591 "
                            + "c-39.296,37.942-63.155,101.877-63.155,171.596v13.992H66.305v42.793h321.261v-42.793h-17.744v-13.992 "
                            + "c0-69.719-23.863-133.653-63.154-171.596c39.291-37.935,63.154-101.864,63.154-171.591V42.794z M276.738,214.327l-14.735,11.163 "
                            + "l14.735,11.163c36.771,27.885,61.451,84.345,64.71,146.425H112.431c3.257-62.074,27.926-118.534,64.708-146.425l14.727-11.163 "
                            + "	l-14.727-11.163c-36.776-27.888-61.451-84.34-64.708-146.42h229.008C338.189,129.987,313.508,186.439,276.738,214.327z "
                            + "	 M141.955,90.167h169.96c-2.457,47.136-21.202,90.009-49.143,111.183c0,0-4.784,2.066-11.173,8.47 "
                            + "	c-13.218,18.876-13.923,87.873-13.945,90.915c9.49,1.013,19.743,5.018,29.904,14.299c35.85,32.755,46.252,36.618,47.503,60.396 "
                            + "	H146.965c1.25-23.772,21.646-40.785,47.5-60.396c0,0,12.3-10.795,29.552-13.79c-0.314-0.542-0.498-0.908-0.498-0.908 "
                            + "	c0-64.99-21.248-92.857-21.248-92.857l-15.103-8.47C159.236,177.821,144.42,137.304,141.955,90.167z"
                            );
        }


        busyTag.style.opacity = state ? 1 : 0;
    };

    this.busy(true);





};

addEventListener("load", function () {

    window.logs = new Logs();

    // blokada menu kontekstowego w wersji WebKit
    if (window.app)
        window.oncontextmenu = function () {
            return false;
        };


    window.setTimeout(function () {

        if (window.app && window.app.logsReady)
            window.app.logsReady();

        if (window.logsReady)
            window.logsReady();

    }, 1);
});





var service = new function () {
    this._url = "";
    this._httpsUrl = "";
    this.logsEnabled = false;
    this.path = document.URL; // context path
    this.loaded = false; // czy dokument jest załadowany
    this.unloading = false;
    this.standby = false;
    this.username = "";
    this.requestId = "";
    this.viewId = "";
    this.viewAutoInitialize = false;


    var stby = function (data) {
        ajax.post("/", {
            silent: true,
            skipLog: true,
            contentType: "application/javascript",
            post: data,
            headers: {
                "standby-request-id": service.requestId
            }
        }, function (response) {
            var cancel = false;
            var result;
            try {
                if (!response.error) {
                    var json = response.responseText;
                    if (!json)
                        return;
                    json = JSON.parse(json);
                    cancel = json.cancel;
                    if (json.data)
                        result = eval(json.data);
                }
            } finally {
                if (!cancel)
                    setTimeout(function () {
                        stby(result);
                    }, response.error ? 1000 : 1);
            }
        });


    };


    addEventListener("load", function () {
        service.loaded = true;

        var opts;
        var metas = document.getElementsByTagName('meta');
        for (var i = 0; i < metas.length; i++)
            if (metas[i].getAttribute("name") === "$service")
                if (metas[i].getAttribute("content"))
                    opts = metas[i].getAttribute("content").split("|");


        if (!opts || opts.length !== 6)
            return;

        service.requestId = opts[0];
        service.viewId = opts[1];
        var flags = opts[2] || "";
        service._url = opts[3];
        service._httpsUrl = opts[4];
        service.debugMode = flags.contains("D");
        service.standby = flags.contains("S");
        service.viewAutoInitialize = flags.contains("V");

        service.logsEnabled = flags.contains("L");
        if (opts[5])
            service.keepAlive(parseInt(opts[4]));

        if (!service._url.endsWith("/"))
            service._url += "/";

        if (service.standby)
            stby();
    });

    addEventListener("beforeunload", function () {
        service.unloading = true;
    });

    if (this.path.indexOf("?") > 0)
        this.path = this.path.substring(0, this.path.indexOf("?"));

    this.url = function (url) {
        if (!url)
            return url;
        if (!url.startsWith("/"))
            return url;
        return this._url + url.substring(1);
    };

    var keepAliveTimeout;
    // podtrzymanie sesji
    this.keepAlive = function (time) {
        // time - czas w sekundach

        if (!time || isNaN(time) || time < 0)
            time = 300;

        clearTimeout(keepAliveTimeout);
        keepAliveTimeout = window.setTimeout(function () {
            ajax.post(location.href, {
                headers: {"ajax-keep-alive": "NOP"},
                silent: true,
                skipLog: true,
                busy: null,
                onError: function () {
                }
            }, function () {
                service.keepAlive(time);
            });

        }, time * 1000);
    };

    this.log = function (tag, log) {
        sendLog("debug", tag, log);
    };

    this.logWarning = function (tag, log) {
        sendLog("warning", tag, log);
    };

    this.logError = function (tag, log) {
        sendLog("error", tag, log);
    };

    function sendLog(kind, tag, val) {
        if (!service.logsEnabled || !service.loaded)
            return;
        if (val === undefined) {
            val = tag;
            tag = "";
        }

        var d = new Date();
        function add(val, len) {
            val = val.toString();
            while (val.length < len) {
                val = "0" + val;
            }
            return val;
        }

        d = add(d.getFullYear(), 4) + "-" +
                add(d.getMonth() + 1, 2) + "-"
                + add(d.getDate(), 2) + " "
                + add(d.getHours(), 2) + ":"
                + add(d.getMinutes(), 2) + ":"
                + add(d.getSeconds(), 2) + "."
                + add(d.getMilliseconds(), 3);
        ajax.post("$?addLog", {
            silent: true,
            skipLog: true,
            knd: kind,
            dte: d,
            tag: tag,
            val: val
        });
    }

    this.layer = function (url, post) {
// jesli warstwa tworzona jest w juz instniejacej
        if (document.layer === undefined)
            return new Layer(this.url(url), post);
        else
            return new top.Layer(this.url(url), post);
    };

    this.error = function (source) {
        if ($className(source) === "XMLHttpRequestProgressEvent")
            return;

        var err = $className(source) === "EError" ? source : new EError(source);
        if (err.id && err.id !== "" && typeof Layer !== 'undefined') {
            var layer = this.layer("/$?error=" + err.id);
            layer.onerror = function (e) {
                $error(err.message);
            };
        } else
            this.errorInternal(source);
    };

    this.errorInternal = function (message, extraHtml) {
        var err = (message instanceof EError) ? message : new EError(message);
        if (typeof top.Layer === 'undefined') {
            alert(err.message);
            return;
        }
        new top.Layer(null, null).message("com/res/error.png",
                "Błąd", err.message, extraHtml);
    };

    this.info = function (message, extraHtml) {
        if (typeof Layer === 'undefined') {
            alert(message);
            return;
        }
        new top.Layer(null, null).message("com/res/info.png",
                "Informacja", message, extraHtml);
    };

    this.warning = function (message, extraHtml) {
        if (typeof Layer === 'undefined') {
            alert(message);
            return;
        }
        new top.Layer(null, null).message("com/res/warning.png",
                "Ostrzeżenie", message, extraHtml);
    };

    this.logout = function (reloadPage) {
        ajax.get("$logout", {}, function (http) {
            if (reloadPage === undefined || reloadPage === true)
                window.location.reload();
        });
    };

    this.debugWindow = function (popup) {
        if (popup)
            showPopupWindow(this.url("$status"), 1024, 600);
        else
            load(this.url("$status"), true);
    };

    this.logsWindow = function (popup) {
        if (popup)
            showPopupWindow(this.url("$logs"), 1024, 600);
        else
            load(this.url("$logs"), true);
    };

    this.userFiles = function (popup) {
        if (popup)
            showPopupWindow(this.url("$userFiles"), 1024, 600);
        else
            load(this.url("$userFiles"), true);
    };

    this.popupWindow = function (url, w, h) {
        showPopupWindow(this.url(url), w, h);
    };

    this.page = function (url, newWindow) {
        if (newWindow)
            window.open(this.url(url), '_blank');
        else
            window.location.href = this.url(url);
    };

    this.downloadAsZip = function (cacheIds, includeContentSize, fileName) {
        var url = new UrlBuilder();
        for (var i = 0; i < cacheIds.length; i++)
            if (cacheIds[i] && cacheIds[i].length === 26)
                url.add("cid", cacheIds[i]);
        if (url.items.length === 0)
            return;
        window.location.href = this.url("$?getZip&")
                + (includeContentSize ? "contentSize&" : "")
                + (fileName ? +"&fileName=" + escapeUrl(fileName) : "")
                + url();
    };

    // sprawdza czy istnieje plik, następnie pobiera lub zwraca błąd 
    this.download = function (cacheId) {
        ajax.get("$?checkCache=" + escapeUrl(cacheId), {},
                function (http) {
                    if (!http.error && http.responseText === "OK" + cacheId)
                        window.location = service.url("$?get=" + escapeUrl(cacheId));
                });
    };

    this.get = this.download;

    this.postAndDownload = function (url, params) {
        // wykonaj i pobierz jako cache data
        ajax.post(url, {
            busy: "Proszę czekać...",
            params: params
        }, function (http) {
            if (http.status !== 200)
                return;
            var cid = http.getResponseHeader("Cached-File-Id");
            if (cid)
                service.download(cid);
        }, false);
    };

    this.load = function (url, newWindow) {
        return load(this.url(url), newWindow);
    };
    // powtwierdzenie, wykonanie żądania i przeładowanie strony

    this.confirmAndReload = function (message, url, post, expectedResponse) {
        if (confirm(message)) {
            this.postAndReload(url, post, expectedResponse);
        }
    };

    this.postAndReload = function (url, params, expectedResponse) {
        return ajax.post(url, {
            params: params,
            reload: true
        }, function () {
            if (this.error)
                return;
            if (expectedResponse && this.responseText !== expectedResponse) {
                $error("Nieprawidłowa odpowiedź");
                return false;
            }
            return true;
        });
    };

    this.submitAndReload = function (formId, onClick) {
        if (onClick)
            if (this.window[onClick](this) === false)
                return;
        var frm = $id(formId);

        return ajax.post(frm.action, {
            form: frm,
            reload: true
        });

    };

    this.removeFile = function (id, sync) {
        return sync ? ajax.getSync("$?remove=" + escape(id))
                === "remove ACK " + id
                : ajax.get("$?remove=" + id);
    };

    this.previewFile = function (id, name, newWindow) {
        ajax.get("$?checkCache=" + escape(id), {}, function (http) {
            if (!http.error) {
                load(service.url("$preview/"
                        + escape(id) + "/" + escapeUrl(name)), newWindow);
            }
        }
        );
    };

    this.previewAsHex = function (id, newWindow) {
        load(this.url("$hex?") + id, newWindow);
    };

    this.previewAsText = function (id, newWindow) {
        load(this.url("$text?") + id, newWindow);
    };

    this.ping = function () {
        var tt = new Date().getTime();
        var s = http.getSync("$?pingTest");
        if (s !== "PingACK")
            return "";
        return new Date().getTime() - tt;
    };

    this.logAjaxError = function (http) {
        if (!(http instanceof XMLHttpRequest)
                || !this.reportJsErrors
                || http.status === 0)
            return;
        var url = http.url;
        if (!url && http.responseXML)
            url = http.responseXML.URL;
        ajax.post("$?ajaxError", {
            msg: http.status + ", " + http.statusText + (url ? ", " + url : "")
        });
    };
    /*
     * obiekt będzie serializowany i zapisywany w konfiguracji
     */

    this.setConfig = function (objectName) {
        // objectName: nazwa zmiennej

        // metody obiektu:
        //  - onBeforeSave

        var opt = window.localStorage.getItem(objectName);

        var object = window[objectName];
        if (!object)
            throw "Nie znaleziono obiektu \"" + objectName + "\"";

        if (opt)
            try {
                opt = window.JSON.parse(opt);

                for (var s in object) {
                    if (opt[s] !== undefined)
                        object[s] = opt[s];
                }
            } catch (e) {
                service.logError("JS config", e);
            }

        addEvent(window, "beforeunload", function () {
            var object = window[objectName];
            if (!object)
                return;
            if (object.onBeforeSave && typeof object.onBeforeSave === "function")
                object.onBeforeSave();

            var field = false;
            var obj = {};
            for (var s in object) {
                if (typeof obj[s] !== "function") {
                    field = true;
                    obj[s] = object[s];
                }
            }
            if (field)
                window.localStorage.setItem(objectName, window.JSON.stringify(obj));
        }, false);
    };

    this.uicallback = function (sender, id) {
        ajax.post(id, {}, function (resp) {
            if (resp.error)
                return;

            var json = JSON.parse(resp.responseText);

            if (json.data)
                eval(json.data);

        });
    };
};


(function (window) {


    var $$service = window.$$service = {
        documentLoaded: false
    };


    $$service.loadDynContent = function (http) {
        if ($className(http) !== "XMLHttpRequest") {
            console.error("Nieprawidowe żądanie");
            return;
        }
        var tag = $id(http.getResponseHeader("dt-tag-id"));
        if (!tag)
            return;
        tag.innerHTML = http.responseText;

        var scripts = tag.getElementsByTagName("script");
        for (var i = 0; i < scripts.length; i++)
            eval(scripts[i].textContent);
    };



    window.addEventListener("load", function () {
        $$service.documentLoaded = true;

        if (window.swal)
            window.swal.setDefaults({
                cancelButtonText: "Anuluj"
            });

    });

    //==========================================================================

    var $docReady = function (callback) {
        if ($$service.documentLoaded) {
            callback();
            return true;
        }
        addEventListener("load", function () {
            callback();
        });
    };

    //==========================================================================

    window.$call = function (functionName /*, params*/) {

        var args = [];
        for (var i = 1; i < arguments.length; i++)
            args.push(arguments[i]);

        ajax.post("", {
            post: JSON.stringify(args),
            contentType: "application/javascript",
            headers: {
                "js-method": functionName
            }
        });
    };

    //==========================================================================

    window.$swal = function (data, onDone) {

        if (typeof data === "string") {
            data = {
                type: "info",
                title: data
            };
        }

        $docReady(function () {
            if (!window.swal) {
                window.alert(data.title);
                return;
            }
            window.swal(data, function (e) {
                if (typeof onDone === "function")
                    onDone(e);
            });

        });
    };

    //==========================================================================

    window.$alert = function (title, text, onDone) {
        $docReady(function () {
            $swal({
                type: "info",
                text: text,
                title: title
            }, onDone);
        });
    };

    //==========================================================================

    window.$error = function (title, text, onDone) {
        if ($className(title) === "EError") {
            var err = title;
            text = title.title || "Błąd";
            title = title.message;

            $docReady(function () {

                var text = "";

                for (var obj in err.details)
                    text += "<small>" + (obj ? escapeHTML(obj) + ": " : "")
                            + escapeHTML(err.details[obj]) + "</small></br>";

                $swal({
                    type: "error",
                    title: (err.title ? "<small>" + escapeHTML(err.title) + "</small></br>" : "")
                            + escapeHTML(err.message),
                    text: text,
                    html: true}
                );

            });
            return;
        }

        $docReady(function () {
            $swal({
                type: "error",
                text: title, // celowa zamiana text <-> title
                title: text // celowa zamiana text <-> title
            }, onDone);
        });
    };

    //==========================================================================

    window.$warning = function (title, text, onDone) {
        $docReady(function () {
            $swal({
                type: "warning",
                text: text,
                title: title
            }, onDone);
        });
    };

    //==========================================================================

    window.$success = function (title, text, onDone) {
        $docReady(function () {
            $swal({
                type: "success",
                text: text,
                title: title
            }, onDone);
        });
    };

    //==========================================================================

    window.$confirm = function (title, onDone) {
        $docReady(function () {
            if (!window.swal) {
                var c = window.confirm(title);
                if (typeof onDone === "function")
                    onDone(c);
                return;
            }
            $swal({
                type: "warning",
                showCancelButton: true,
                title: title
            }, onDone);
        });
    };

    //==========================================================================

    window.$prompt = function (title, value, onDone) {
        $docReady(function () {
            if (!window.swal) {
                window.prompt(title);
                return;
            }
            $swal({
                type: "input",
                inputValue: value,
                showCancelButton: true,
                title: title
            }, onDone);
        });
    };


})(window);




if (window.jQuery) {

    $(document).ajaxSend(function (event, jqXHR, ajaxOptions) {
        jqXHR.setRequestHeader('X-Requested-With', 'XMLHttpRequest');
    });

    $(document).ajaxError(function (event, jqxhr, settings, thrownError) {
        window.$error(new EError(jqxhr));
        return true;
    });
}

