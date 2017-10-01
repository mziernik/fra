//# sourceURL=file:///shell/cache.js

/**
 * @param {SPAController} ctrl
 * @param {FrameworkApi} api
 * @returns {void}
 */
registerController(function (ctrl, api) {
    "use strict";

    ctrl.name = "Użytkownicy";
    ctrl.urls = ["users", "user/edit/.+"];
    ctrl.htmlTemplate = "test/users/users.html";
    ctrl.clearOnUnload = false;

    var tbl;
    ctrl.onNewInstance = (e) => {

        tbl = new DsTable(opt => {
            opt.controller = ctrl;
            opt.tag = $id("tblCache");
        });

        tbl.reloadData = function (params) {
            api.users({
                params: params,
                onSuccess: function (response) {
                    console.info("Request: " + (new Date().getTime() - ts) + "ms");
                    ts = new Date().getTime();
                    tbl.build(response.data);
                    console.info("Build: " + (new Date().getTime() - ts) + "ms");
                }
            });
        };


    };

    ctrl.onLoad = (e) => {
        tbl.reloadData();
    };


});







