//# sourceURL=file:///tools/db/db.js

/**
 * @param {SPAController} ctrl
 * @param {FrameworkApi} api
 * @returns {void}
 */
registerController(function (ctrl, api) {
    "use strict";

    ctrl.id = "database";
    ctrl.name = "Baza danych";
    ctrl.urls = ["db", "database"];
    ctrl.htmlTemplate = "tools/db/db.html";
    ctrl.clearOnUnload = false;


    var databases = {};
    var currentDb;
    var currentTable;
    var statusBar;

    var tree = new DsTree(opt => {
        opt.tag = "db_tree";
        opt.controller = ctrl;
    });

    tree.onExpand = (node, expanded) => {
        setCurrentDb(node.extra.database.id);
    };

    ctrl.onNewInstance = e => {
        statusBar = $id("db-status-bar");

        $id("db_tree").oncontextmenu = showPopup;

        var edt = $id("ta_db_query");
        var dsTbl = new DsTable(opt => {
            opt.controller = ctrl;
            opt.tag = $id("tbl_db_result");
        });

        edt.value = ctrl.loadValue("query") || "";

        edt.onkeypress = (e) => {

            if (!e.ctrlKey || e.charCode !== 10) // CTRL + Enter
                return;

            var query = edt.value.substring(edt.selectionStart,
                    edt.selectionEnd).trim()
                    || edt.value.trim();

            if (!query)
                return;

            ctrl.saveValue("query", edt.value);

            api.database.execute({
                params: {
                    id: currentDb.id,
                    query: query
                },
                onSuccess: (e) => {
                    dsTbl.load(e, true);
                }
            });

            e.cancelBubble = true;
        };

        $('#db_container').enhsplitter({
            vertical: true,
            position: "200px",
            leftMaxSize: "50%"
        });

        $('#db_main').enhsplitter({
            vertical: false,
            position: "50%"

        });

        api.database.getDatabases({
            onSuccess: (e) => {

                $.each(e.data, (name, idx) => {
                    tree.item(name, e.data[name].name);

                    api.database.getMeta({
                        params: {
                            id: name
                        },
                        onSuccess: (e) => {
                            buid(e.data);
                        }
                    });
                });
            }
        });

    };

    function setCurrentDb(id) {
        currentTable = null;
        currentDb = databases[id];
        statusBar.txt("Bieżąca baza: " + currentDb.name);
    }

    function buid(data) {

        var node = tree.getById(data.id);

        var db = databases[data.id] = {
            id: data.id,
            node: node,
            name: data.name
        };

        if (!currentDb)
            setCurrentDb(db.id);


        $.each(data.catalog, (name, cat) => {
            cat.name = name;
            var itCat = node;
            if (Object.keys(data.catalog).length > 1) {
                itCat = node.item(name, name);
            }

            itCat.extra.database = db;

            $.each(cat.schema, (name, sch) => {
                sch.name = name;
                var itSch = itCat.item(name, name);

                itSch.extra.database = db;
                itSch.extra.schema = sch;

                $.each(sch.function, (name, funct) => {
                    var parentItem = itSch.getById("#funct") || itSch.item("#funct", "# FUNCTION");
                    var itFunct = parentItem.item(name, name);

                    itFunct.extra.database = db;
                });

                $.each(sch.table, (name, tbl) => {
                    tbl.name = name;
                    var parentItem = itSch;
                    if (("" + tbl.type).toLowerCase() !== "table")
                        parentItem = itSch.getById(tbl.type) || itSch.item(tbl.type, "# " + tbl.type);

                    var itTbl = parentItem.item(name, name);
                    itTbl.extra.database = db;
                    itTbl.extra.table = tbl;

                    $.each(tbl.column, (name, col) => {
                        col.name = name;
                        var itCol = itTbl.item(name, name);
                        itCol.extra.database = db;
                        itCol.extra.column = col;

                        itCol.title = name + " " + col.type
                                + "[" + col.size + "] "
                                + (col.primaryKey ? " PRIMARY_KEY" : "")
                                + (col.autoIncrement ? " AUTO_INCREMENT" : "");

                        itCol.comment = col.type;
                    });

                });


            });
        });

    }



    var showPopup = (e, f) => {
        var menu = new PopupMenu();
        var node = e.target.dsTree || (e.target.parentNode && e.target.parentNode.dsTree);
        if (!node)
            return false;

        if (node.extra.table)
            menu.add("Pokaż zawartość", (a) => {
                alert(node.extra.table.name);
            });


        menu.show();
        return false;
    };


});








