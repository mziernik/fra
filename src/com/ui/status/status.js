//# sourceURL=file:///status/status.js

/**
 * @param {SPAController} ctrl
 * @param {FrameworkApi} api
 * @returns {void}
 */
registerController(function (ctrl, api) {
    "use strict";

    ctrl.name = "Status";
    ctrl.urls = ["status"];
    ctrl.htmlTemplate = "status/status.html";
    ctrl.clearOnUnload = false;


    ctrl.registerEvent("service.status", function (obj) {
        $id("serviceStatus").setText(JSON.stringify(obj.data, null, 2));
    });


});








