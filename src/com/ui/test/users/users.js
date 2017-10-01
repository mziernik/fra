//# sourceURL=file:///test/users.js

/**
 * @param {SPAController} ctrl
 * @param {FrameworkApi} api
 * @returns {void}
 */

registerController(function (ctrl, api) {
    "use strict";

    ctrl.id = "users";
    ctrl.name = "Użytkownicy";
    ctrl.urls = ["users", "users/.+"];
    ctrl.htmlTemplate = "test/users/users.html";
    ctrl.clearOnUnload = false;

    ctrl.registerEvent("users", obj => {
        dsTable.onUpdate(obj.data);
    });


    var dsTable = null;


    ctrl.onNewInstance = e => {
        $id("btnCancel").onclick = () => {
            var req = api.test.cancel();

            setTimeout(() => {
                req.event("Test", "Zdarzenie");
            }, 1000);

            setTimeout(() => {
                req.cancel();
            }, 2000);
        };

        dsTable = new DsTable(opt => {
            opt.id = ctrl.id;
            opt.tag = $id("tblUsers");
            opt.autoUpdate = true;
            opt.webApiMethod = api.test.users;
        });

    };

    ctrl.onLoad = e => {
        dsTable.reloadData();
    };
});



//=============================
/*
 var resolved, rejected;
 
 var p = new Promise((res, rej) => {
 resolved = res;
 rejected = rej;
 });
 
 
 Promise.all([p]).then(() => {
 alert("Koniec");
 });
 
 p.then(() => {
 alert("Koniec 2");
 });
 
 
 
 
 
 */