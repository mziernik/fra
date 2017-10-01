//# sourceURL=file:///config/list.js

/**
 * @param {SPAController} ctrl
 * @param {FrameworkApi} api
 * @returns {void}
 */
registerController(function (ctrl, api) {
    "use strict";

    ctrl.id = "config.list";
    ctrl.name = "Konfiguracja";
    ctrl.urls = ["config/list"];
    ctrl.htmlTemplate = null;
    ctrl.clearOnUnload = false;

    var dsTable;

    ctrl.onNewInstance = (e) => {

        /**
         * @param {DsTableOptions} opt 
         */
        dsTable = new DsTable((opt) => {
            opt.controller = ctrl;
            opt.tag = spa.container.tag("table")
                    .attr({class: "dsTbl"});
            opt.webApiMethod = api.config.getList;
        });
    };

    ctrl.onLoad = function (e) {
        dsTable.reloadData();
    };

});

