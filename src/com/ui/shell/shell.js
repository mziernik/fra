//# sourceURL=file:///shell/shell.js

/**
 * @param {SPAController} ctrl
 * @param {FrameworkApi} api
 * @returns {void}
 */

registerController(function (ctrl, api) {
    "use strict";

    ctrl.id = "shell";
    ctrl.name = "Shell";
    ctrl.urls = ["shell"];
    ctrl.htmlTemplate = "shell/shell.html";
    ctrl.clearOnUnload = false;
    var active;
    var shellIn;

    document.body.addEventListener("keypress", function (e) {
        if (active && e.target === document.body)
            shellIn.focus();
    });

    ctrl.onUnload = (e) => {
        active = false;
    };

    ctrl.onLoad = (d) => {

        active = true;
        shellIn = $id("shell-in");

        var tshell = $id("shell");
        shellIn.onkeypress = function (e) {
            if (e.charCode === 13) {
                if (req.event)
                    req.event("line", shellIn.textContent);
                shellIn.textContent = "";
                e.cancelBuble = true;
                return;
            }
        };
        var req = api.system.shell({
            onSent: function (msg) {
                //    events = msg.event;
            },
            onEvent: function (event) {

                var lines = event.data.split("\n");
                for (var i = 0; i < lines.length; i++)
                    tshell.tag("pre").txt(lines[i]);
                tshell.removeChild(shellIn);
                tshell.appendChild(shellIn);
                tshell.scrollTop = tshell.scrollHeight;
                shellIn.focus();
                shellIn.textContent = "";
                // tshell.setSelectionRange(strLength, strLength);

            },
            onSuccess: function (response) {
                shellIn.attr("contenteditable", "false");
                alert("koniec");
            },
            onError: function (response) {
                alert("blad");
            }
        });


    };
});










