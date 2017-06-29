//# sourceURL=file:///sessions/sessions.js

/**
 * @param {SPAController} ctrl
 * @param {FrameworkApi} api
 * @returns {void}
 */
registerController(function (ctrl, api) {
    "use strict";
    ctrl.id = "sessions";
    ctrl.name = "Sesje";
    ctrl.urls = ["sessions"];
    ctrl.htmlTemplate = "sessions/sessions.html";
    ctrl.clearOnUnload = false;

    var dsTable;

    ctrl.onNewInstance = (e) => {
        dsTable = new DsTable(opt => {
            opt.controller = ctrl;
            opt.tag = $id("tblSessions");
            opt.webApiMethod = api.test.users;
        });
    };

    ctrl.onLoad = (e) => {
        dsTable.reloadData();
    };

});