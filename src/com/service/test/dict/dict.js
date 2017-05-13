//# sourceURL=file:///test/dict.js

/**
 * @param {SPAController} ctrl
 * @param {FrameworkApi} api
 * @returns {void}
 */
registerController(function (ctrl, api) {
    "use strict";

    ctrl.name = "Słownik";
    ctrl.urls = ["dict"];
    ctrl.htmlTemplate = "test/dict/dict.html";
    ctrl.clearOnUnload = false;
    ctrl.id = 'dict';

    var dsTable;

    ctrl.onNewInstance = e => {
        dsTable = new DsTable(opt => {
            opt.controller = ctrl;
            opt.tag = $id("tblDict");
            opt.webApiMethod = api.test.dict;
        });
    };

    ctrl.onLoad = e => {
        dsTable.reloadData();
    };
});










