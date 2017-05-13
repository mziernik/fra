//# sourceURL=file:///test/numbers.js

/**
 * @param {SPAController} ctrl
 * @param {FrameworkApi} api
 * @returns {void}
 */
registerController(function (ctrl, api) {
    "use strict";

    ctrl.id = "numbers";
    ctrl.name = "Numery";
    ctrl.urls = ["numbers"];
    ctrl.htmlTemplate = "test/numbers/numbers.html";
    ctrl.clearOnUnload = false;

    var dsTable;

    ctrl.onNewInstance = e => {
        dsTable = new DsTable(opt => {
            opt.controller = ctrl;
            opt.tag = $id("tblNumbers");
            opt.webApiMethod = api.test.numbers;
        });
    };

    ctrl.onLoad = e => {
        dsTable.reloadData();
    };

});










