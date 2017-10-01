//# sourceURL=file:///tools/eval/eval.js

/**
 * @param {SPAController} ctrl
 * @param {FrameworkApi} api
 * @returns {void}
 */
registerController(function (ctrl, api) {
    "use strict";

    ctrl.id = "eval";
    ctrl.name = "Evaluator";
    ctrl.urls = ["eval"];
    ctrl.htmlTemplate = "tools/eval/eval.html";
    ctrl.clearOnUnload = false;


    ctrl.onNewInstance = function (e) {

        var edt = $id("eval_src");
        edt.onkeypress = (e) => {
            if (!e.ctrlKey || e.charCode !== 10) // CTRL + Enter
                return;

            api.tools.eval({
                data: edt.value
            }).then((resp) => {
                $id("eval_dst").txt(resp.data);
            });

            e.cancelBubble = true;
        };
    };
});








