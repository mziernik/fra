//# sourceURL=file:///logs/storage.js

/**
 * @param {SPAController} ctrl
 * @param {FrameworkApi} api
 * @returns {void}
 */
registerController(function (ctrl, api) {
    "use strict";

    ctrl.id = "logs.storage";
    ctrl.name = "Magazyn logów";
    ctrl.urls = ["storage"];
    ctrl.htmlTemplate = null;
    ctrl.clearOnUnload = false;

    var dsTable;

    ctrl.onNewInstance = (e) => {
        dsTable = new DsTable(opt => {
            opt.controller = ctrl;
            opt.tag = ctrl.htmlDoc.tag("table").attr({class: "dsTbl"});
            opt.webApiMethod = api.log.storage.getAll;
        });
    };

    ctrl.onLoad = (e) => {
        dsTable.reloadData();
    };

});

