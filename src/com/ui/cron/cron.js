//# sourceURL=file:///cron/cron.js

/**
 * @param {SPAController} ctrl
 * @param {FrameworkApi} api
 * @returns {void}
 */
registerController(function (ctrl, api) {
    "use strict";
    ctrl.id = "cron";
    ctrl.name = "Harmonogram zadań";
    ctrl.urls = ["cron"];
    ctrl.htmlTemplate = "cron/cron.html";
    ctrl.clearOnUnload = false;
    var tbl;

    ctrl.onNewInstance = (e) => {

        tbl = new DsTable(opt => {
            opt.controller = ctrl;
            opt.tag = $id("tblCron");
            opt.webApiMethod = api.cron.getAll;
        });
//        new schedulePanel.Panel($('#scheduleTab'), '');

    };

    ctrl.onLoad = (e) => {
        tbl.reloadData();
    };

});

