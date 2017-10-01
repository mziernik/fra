//# sourceURL=file:///tools/cms/cms.js


/**
 * @param {SPAController} ctrl
 * @param {FrameworkApi} api
 * @returns {void}
 */

registerController(function (ctrl, api) {
    "use strict";

    ctrl.id = "cms";
    ctrl.name = "CMS";
    ctrl.urls = ["cms"];
    ctrl.htmlTemplate = "tools/cms/cms.html";
    ctrl.clearOnUnload = false;

    ctrl.onUnload = function (e) {
    };

    ctrl.onLoad = function (e) {

    };
});








