//# sourceURL=file:///threads/threads.js

/**
 * @param {SPAController} ctrl
 * @param {FrameworkApi} api
 * @returns {void}
 */
registerController(function (ctrl, api) {
    "use strict";

    ctrl.id = "threads";
    ctrl.name = "Wątki";
    ctrl.urls = ["threads"];
    ctrl.htmlTemplate = "threads/threads.html";
    ctrl.clearOnUnload = false;


    ctrl.registerEvent("service.threads", function (obj) {
        var ts = new Date().getTime();
        dsTable.load(obj.data.list, false);
        console.info("Update: " + (new Date().getTime() - ts) + "ms");
    });

    var dsTable;

    ctrl.onNewInstance = e => {

        dsTable = new DsTable(opt => {
            opt.controller = ctrl;
            opt.tag = $id("tblThreads");
        });

        dsTable.load({
            key: "threads",
            name: "Wątki",
            results: null,
            offset: null,
            limit: null,
            updatable: true, // ważne
            selectable: false,
            columns: {
                id: {
                    primaryKey: true,
                    caption: "ID",
                    type: "int",
                    searchable: true,
                    sortable: true
                },
                prior: {
                    caption: "Priorytet",
                    type: "int",
                    searchable: true,
                    sortable: true
                },

                state: {
                    caption: "Stan",
                    type: "string",
                    searchable: true,
                    sortable: true
                },
                group: {
                    caption: "Grupa",
                    type: "string",
                    searchable: true,
                    sortable: true
                },
                alive: {
                    caption: "Żyje",
                    type: "boolean",
                    searchable: true,
                    sortable: true
                },
                daemon: {
                    caption: "Daemon",
                    type: "boolean",
                    searchable: true,
                    sortable: true
                },
                int: {
                    caption: "Przerwany",
                    type: "boolean",
                    searchable: true,
                    sortable: true
                },
                name: {
                    caption: "Nazwa",
                    type: "string",
                    searchable: true,
                    sortable: true
                },
                cpu: {
                    caption: "CPU",
                    type: "int",
                    searchable: true,
                    sortable: true
                },
                user: {
                    caption: "Użytkownik",
                    type: "int",
                    searchable: true,
                    sortable: true
                },
                mem: {
                    caption: "Pamięć",
                    type: "length",
                    searchable: true,
                    sortable: true
                },
                bc: {
                    caption: "BlockedCount",
                    type: "int",
                    searchable: true,
                    sortable: true
                },
                wc: {
                    caption: "WaitedCount",
                    type: "int",
                    searchable: true,
                    sortable: true
                }



            },
            rows: []
        });

    };
});







