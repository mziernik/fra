//# sourceURL=file:///config/config.js

/**
 * @param {SPAController} ctrl
 * @param {FrameworkApi} api
 * @returns {void}
 */
registerController(function (ctrl, api) {
    "use strict";

    ctrl.id = "config";
    ctrl.name = "Konfiguracja";
    ctrl.urls = ["config"];
    ctrl.htmlTemplate = "config/configuration.html";
    ctrl.clearOnUnload = false;


    var methods = {itemMethods: {}};

    ctrl.onNewInstance = (e) => {

        BootstrapDialog.showExt = function (obj) {
            var instance = BootstrapDialog.show(obj);
            return instance;
        };



        methods.itemMethods.export = function (usersValue) {
            return api.config.exportConf({params: {usersValue: usersValue}}).then(function (data) {
                return data;
            });
        };

        methods.itemMethods.import = function (usersValue) {
            return api.config.importConf({params: {usersValue: usersValue}}).then(function (data) {
                return data;
            });
        };

        methods.itemMethods.get = function (itemId) {
            return api.config.getItem({params: {id: itemId}});
        };

        methods.itemMethods.save = function (value, itemID, isDefault, variable) {
            var obj = {};

            if (variable === null) {
                obj.def = isDefault;
                obj.val = value;
            } else {
                obj.def = isDefault;
                obj.val = value;
                obj.var = variable;
            }

            return api.config.save({params: {id: itemID}, data: obj}).then(function (data) {
                return data;
            });
        };

        methods.itemMethods.validate = function (itemID, data) {
            return api.config.validate({params: {id: itemID}, data: data}).then(function (data) {
                return data;
            });
        };
    };

    ctrl.onLoad = (e) => {
        api.config.getStructure().then(function (response) {
            var tree = {
                configData: response.data,
                itemMethods: methods.itemMethods,
                id: 'tree',
                lang: spa.language["config-tree"]
            };

            var div = document.getElementById('configurationTree');

            window.onConfigurationLoad(tree, div);
        });
    };
});