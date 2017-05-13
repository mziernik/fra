
//# sourceURL=file:///config/configuration.js

(function (window) {
    'use strict';

    var defaultExpandedItems = [];
    var defaultConvolutedItems = [];
    var paths = {}; // mapa ścieżek
    var currentPath = []; // aktualna ścieżka
    var filterData; // dane do filtrowanie
    var lang;
    var itemOverload;

    function showGroup(group, idGroup, element, lvl, treeTag) {

        if (group.expanded === true) {
            defaultExpandedItems.push(idGroup);
        } else if (group.expanded === false) {
            defaultConvolutedItems.push(idGroup);
        }

        var li = $(document.createElement('li'));
        $(element).append(li);
        var groupName = $(document.createElement('a'));
        groupName.addClass('config-group');
        var spanIcon = $(document.createElement('span'));
        spanIcon.addClass('glyphicon glyphicon-menu-down');
        groupName.append(spanIcon);
        groupName.attr('id', idGroup);
        var padding = lvl * 2.8;

        var style = 'padding-left: ' + (0.3 + padding) + 'em;';
        if (group.disabled)
            style += ' color: #BDBDBD;';

        groupName.attr('style', style);
        groupName.append(document.createTextNode(group.name));
        var descGroup = $(document.createElement('description'));
        descGroup.attr('class', 'config-item-desc');
        descGroup.attr('style', 'font-weight: normal;');
        if (group.description)
            descGroup.text(group.description);
        groupName.append(descGroup);

        li.append(groupName);
        var ul = $(document.createElement('ul'));
        ul.attr('style', 'display: block;');
        li.append(ul);

        if (group.groups && group.groupsAtTop === true) {
            for (var groupIn in group.groups) {
                if (group.groups.hasOwnProperty(groupIn))
                    showGroup(group.groups[groupIn], groupIn, ul, lvl + 1, treeTag);
            }
        }

        for (var item in group.items) {
            if (!group.items.hasOwnProperty(item))
                continue;

            showItem(ul, group, idGroup, item, padding, treeTag);
        }

        if (group.groups && group.groupsAtTop !== true) {
            for (var groupIn in group.groups) {
                if (group.groups.hasOwnProperty(groupIn))
                    showGroup(group.groups[groupIn], groupIn, ul, lvl + 1, treeTag);
            }
        }
    }

    function showItem(ul, group, idGroup, item, padding, treeTag) {

        var liItem = $(document.createElement('li'));
        var divItem = $(document.createElement('div'));
        divItem.addClass('config-item');

        var style = 'padding-left: ' + (3.8 + padding) + 'em;';
        if (group.items[item].disabled)
            style += ' color: #BDBDBD;';

        divItem.attr('style', style);
        divItem.attr('id', item);
        liItem.append(divItem);
        var itemName = $(document.createElement('span'));
        itemName.text(group.items[item].name);
        if (group.items[item].missing) {
            itemName.attr('style', 'color: red; font-weight: bold;');

            if (idGroup !== null) {
                for (var q = 0; q < paths[idGroup].length; q++) {
                    $(document.getElementById(paths[idGroup][q])).css('color', 'red');
                }
            }
        }
        divItem.append(itemName);
        var spanColon = $(document.createElement('span'));
        spanColon.text(':');
        divItem.append(spanColon);
        var itemValue = $(document.createElement('span'));
        itemValue.addClass('config-item-value');

        if (group.items[item].displayValue) {
            itemValue.text(group.items[item].displayValue);
            if (group.items[item].disabled) {
                itemValue.attr('style', 'color: #BDBDBD;');
            } else if (group.items[item].modified) {
                itemValue.attr('style', 'color: #04c;');
            }
        } else {
            itemValue.text('""');
        }

        divItem.append(itemValue);
        var descItem = $(document.createElement('span'));
        descItem.addClass('config-item-desc');
        if (group.items[item].description) {
            descItem.text(group.items[item].description);
        }

        divItem.append(descItem);
        divItem.click(function () {
            var itemId = this.id;
            treeTag.itemMethods.get(itemId).then(function (response) {
                response.data.id = itemId;
                var confDialog = new configDialog(response.data, function (value, isDefault, callback, variable) {
                    return treeTag.itemMethods.save(value, itemId, isDefault, variable).then(function (response) {
                        if (!response.error) {
                            if (itemOverload) {
                                loadItem(itemId, response.data, document.getElementById(treeTag.id));
                            } else {
                                loadTree(response.data, document.getElementById(treeTag.id));
                            }
                            confDialog.close();
                        }
                        callback(response);
                    }, function (response) {
                        callback(response);
                    });
                }, function (value, callback) {
                    treeTag.itemMethods.validate(itemId, value)
                            .then(function (response) {
                                callback(response.data);
                            });
                }, lang);
            });
        });
        ul.append(liItem);
    }

    function showData(dataObj, treeTag) {

        for (var group in dataObj.groups) {
            if (dataObj.groups.hasOwnProperty(group)) {
                showGroup(dataObj.groups[group], group, $(treeTag), 0, treeTag);
            }
        }

        if (dataObj.items) {

            var li = $(document.createElement('li'));
            $(treeTag).append(li);

            var ul = $(document.createElement('ul'));
            ul.attr('style', 'display: block;');
            li.append(ul);

            for (var item in dataObj.items) {
                if (!dataObj.items.hasOwnProperty(item))
                    continue;

                showItem(ul, dataObj, null, item, -2.9, treeTag);
            }
        }
    }

    var saveRowChanges = true;
    function updateExpandIcon(jqTag) {
        if (jqTag.hasClass('glyphicon-menu-right')) {
            jqTag.removeClass('glyphicon-menu-right');
            jqTag.addClass('glyphicon-menu-down');
        } else {
            jqTag.removeClass('glyphicon-menu-down');
            jqTag.addClass('glyphicon-menu-right');
        }
    }

    function loadExpandedRows(treeTag) {
        var arr = JSON.parse(localStorage.getItem('config.expanded_rows_' + treeTag.id));

        if (!arr || !(arr instanceof Array)) {
            return;
        }

        arr.forEach(function (element) {
            if (defaultConvolutedItems.indexOf(element) === -1) {
                $('#' + treeTag.id).find("#" + element).click();
                if (defaultExpandedItems.indexOf(element) > -1)
                    defaultExpandedItems.slice(defaultExpandedItems.indexOf(element), 1);
            }
        });

        if (defaultExpandedItems.length > 0) {
            defaultExpandedItems.forEach(function (element) {
                $('#' + treeTag.id).find("#" + element).click();
            });
        }
    }

    function saveExpandedRows(treeTag) {
        if (saveRowChanges === false) {
            return;
        }

        var arr = [];
        $('#' + treeTag.id).find('.config-group').each(function () {
            //if ($(this).children('span').hasClass('glyphicon-menu-right')) {
            if ($(this).children('span').hasClass('glyphicon-menu-down')) {
                arr.push($(this).attr("id"));
            }
        });
        localStorage.setItem('config.expanded_rows_' + treeTag.id, JSON.stringify(arr));
    }

    function hideAllGroups(treeTag) {
        $('#' + treeTag.id).find('.config-group').each(function () {
            if ($(this).children('span').hasClass('glyphicon-menu-down')) {
                $(this).click();
            }
        });
    }

    function setFastOptionToggle(treeTag) {
// Zdarzenie rozwinięcia menu (szybkie) - tylko podczas wczytywania strony
        $('#' + treeTag.id).find('.config-group').unbind('click'); // usunięcie starego zdarzenia 'click'
        $('#' + treeTag.id).find('.config-group').click(function () {
            $(this).next('ul').toggle();
            updateExpandIcon($(this).children("span"));
        });
    }

    function setSlowOptionToggle(treeTag) {
// Zdarzenie rozwinięcia menu (powolne - animowane)
        $('#' + treeTag.id).find('.config-group').unbind('click'); // usunięcie starego zdarzenia 'click'
        $('#' + treeTag.id).find('.config-group').click(function (event) {
            $(this).next('ul').slideToggle(200);
            event.stopPropagation();
            updateExpandIcon($(this).children('span'));
            saveExpandedRows(treeTag); // zapisanie rozwiniętych opcji
        });
    }

    window.onConfigurationLoad = function (tag, element) {

        var treeTag = tag;
        lang = treeTag.lang;

        itemOverload = treeTag.itemOverload;

        var navbar = document.createElement('div');
        $(navbar).attr('class', 'navbar-form navbar-right');
        $(navbar).attr('style', 'margin-right: 20px; margin-top: 20px;');
        element.append(navbar);

        var inputGroup = document.createElement('div');
        $(inputGroup).attr('class', 'input-group');
        navbar.append(inputGroup);

        var inputFilter = document.createElement('input');
        $(inputFilter).attr('type', 'text');
        $(inputFilter).attr('id', 'filterInput_' + treeTag.id);
        $(inputFilter).attr('class', 'form-control');
        $(inputFilter).attr('placeholder', 'Szukaj');
        inputGroup.append(inputFilter);

        var inputGroupBtn = document.createElement('div');
        $(inputGroupBtn).attr('class', 'input-group-btn');
        inputGroup.append(inputGroupBtn);

        var btnClear = document.createElement('button');
        $(btnClear).attr('id', 'filterClear_' + treeTag.id);
        $(btnClear).attr('class', 'btn btn-default');
        $(btnClear).attr('title', lang['config.removeFilter']);
        inputGroupBtn.append(btnClear);

        var spanTrash = document.createElement('span');
        $(spanTrash).attr('class', 'glyphicon glyphicon-trash');
        $(spanTrash).attr('aria-hiden', 'true');
        btnClear.append(spanTrash);

        var exportBtn = document.createElement('a');
        $(exportBtn).attr('class', 'btn btn-default');
        $(exportBtn).attr('id', 'export-config-btn');
        $(exportBtn).attr('style', 'margin-left: 10px;');
        $(exportBtn).text(lang['config.export']);
        navbar.append(exportBtn);

        $(exportBtn).click(function () {
            var dialog = BootstrapDialog.showExt({
                title: "<span style='user-select: initial;'>" + lang['config.configExport'] + "</span>",
                closable: false,
                message: document.createElement('div'),
                onshow: function (win) {
                    window.moment.tz.add('Etc/UTC|WMT CET CEST EET EEST|-1o -10 -20 -20 -30|012121234312121212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121|-2ctdo 1LXo 11d0 1iO0 11A0 1o00 11A0 1on0 11A0 6zy0 HWP0 5IM0 WM0 1fA0 1cM0 1dz0 1mL0 1en0 15B0 1aq0 1nA0 11A0 1io0 17c0 1fA0 1a00 iDX0 LA0 1cM0 1cM0 1C00 Oo0 1cM0 1cM0 1zc0 Oo0 1zc0 Oo0 1zc0 Oo0 1C00 LA0 uso0 1a00 1fA0 1cM0 1cM0 1cM0 1fA0 1a00 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1fA0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cN0 1cM0 1cM0 1cM0 1cM0 1fA0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1fA0 1o00 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00');
                    var form = $('<form class="form-horizontal smart-form" name="itemEdition"></form>');
                    form.attr('id', 'mainForm');
                    win.$modalBody.append(form);

                    form.submit(function () {
                        return false;
                    });

                    var checkbox = $(document.createElement('input'));
                    checkbox.attr('type', 'checkbox');
                    checkbox.attr('value', 'onlyModified');
                    checkbox.attr('name', 'onlyModified');
                    checkbox.attr('id', 'onlyModified');
                    checkbox.attr('checked', 'true');

                    checkbox.change(function () {
                        var check = $(this).prop('checked');

                        treeTag.itemMethods.export(check).then(function (response) {
                            textarea.val(response.data);
                        });
                    });

                    form.append(checkbox);

                    var label = $(document.createElement('label'));
                    label.attr('for', 'onlyModified');
                    label.text(lang['config.onlyModified']);
                    label.attr('style', 'margin-left: 10px; margin-bottom: 10px;');
                    form.append(label);

                    var textarea = $(document.createElement('textarea'));
                    textarea.addClass('form-control');
                    textarea.attr({style: "width: 100%; padding-left: 10px;" +
                                "padding-right: 10px; font-family: 'Consolas', 'Lucida Console'," +
                                "'Courier New', monospace; resize: vertical;"});
                    textarea.attr('name', 'configurationTextArea');
                    textarea.attr('id', 'configurationTextArea');
                    textarea.attr('rows', '30');
                    textarea.attr('spellcheck', 'false');

                    treeTag.itemMethods.export(true).then(function (response) {
                        textarea.val(response.data);
                    });

                    form.append(textarea);
                },
                buttons: [
                    {
                        key: 'confirm-button',
                        cssClass: 'btn-primary',
                        label: lang['config.apply'],
                        action: function (dialogRef) {

                            $.SmartMessageBox({
                                title: lang['config.fillingIn'],
                                content: lang['config.reallySave'],
                                buttons: '[' + lang['config.cancel'] + '][' + lang['config.save'] + ']'
                            }, function (ButtonPressed, Value) {
                                if (ButtonPressed === lang['config.save']) {
                                    treeTag.itemMethods.import(document.getElementById('configurationTextArea').value,
                                            $('#onlyModified').prop('checked'))
                                            .then(function (response) {
                                                if (!response.error) {
                                                    loadTree(response.data, document.getElementById(treeTag.id));
                                                    dialogRef.close();
                                                }
                                            });
                                } else if (ButtonPressed === lang['config.cancel']) {
                                    $.smallBox({
                                        title: lang['config.canceled'],
                                        content: lang['config.abandonedSave'],
                                        color: '#C46A69',
                                        iconSmall: 'fa fa-times fa-2x fadeInRight animated',
                                        timeout: 4000
                                    });
                                }
                            });
                        }
                    },
                    {
                        key: 'cancel-button',
                        hotkey: 27,
                        label: lang['config.cancel'],
                        action: function (dialogRef) {
                            dialogRef.close();
                        }
                    }
                ]
            });
        });

        var divFAccordion = document.createElement('div');
        $(divFAccordion).attr('class', 'fAccordion');
        $(divFAccordion).attr('style', 'font: 10pt Verdana;');
        element.append(divFAccordion);

        divFAccordion.append(document.createElement('br'));
        divFAccordion.append(document.createElement('br'));
        divFAccordion.append(document.createElement('br'));

        var ulTree = document.createElement('ul');
        $(ulTree).attr('id', treeTag.id);
        ulTree.itemMethods = treeTag.itemMethods;
        divFAccordion.append(ulTree);

        var configData = treeTag.configData;
        loadTree(configData, ulTree);

        // Filtrowanie danych
        $('#filterInput_' + treeTag.id).bind('input propertychange', function () {
            var value;
            value = convertPolishChars($(this).val().toLowerCase()); // Szukana wartość

            setFastOptionToggle(ulTree);
            //localStorage.setItem('config.search_value', JSON.stringify(value));
            // Usunięcie koloru z wszystkich poprzednich rezultatów
            $('#' + treeTag.id).find('.searched-element').each(function () {
                $(this).removeClass('searched-element');
            });
            // Rozwinięcie schowanych atrybutów
            $('#' + treeTag.id).find('li:hidden, a:hidden').show();

            // Usunięcie filtru w przypadku pustego tekstu
            if (value.replace(' ', '').length === 0) {
                // Wyświetlenie wszystkich schowanych elementów
                $('#' + treeTag.id).find('li:hidden, a:hidden').show();
                setFastOptionToggle(ulTree);
                hideAllGroups(ulTree);
                loadExpandedRows(ulTree); // Rozwinięcie wierszy na podstawie listy z localstorage
                setSlowOptionToggle(ulTree);
                return;
            }

            saveRowChanges = false;
            var grpToShow = []; // grupy, które są wynikiem wyszukiwania
            var itmToShow = []; // itemy, które są wynikiem wyszukiwania

            function checkElement(element, elementId, isGroup, groupId) {
                if (convertPolishChars(element.name).toLowerCase().indexOf(value) !== -1 ||
                        (element.displayValue && element.displayValue.toLowerCase().indexOf(value) !== -1)
                        || elementId.toLowerCase().indexOf(value) !== -1) {

                    if (isGroup) {
                        grpToShow.push(elementId);
                    } else {
                        var obj = {};
                        obj.group = groupId; // id grupy - potrzebne do wyświetlenia
                        obj.item = elementId; // id elementu w grupie

                        itmToShow.push(obj);
                    }
                }

                if (element.groups) {
                    for (var group in element.groups) {
                        if (element.groups.hasOwnProperty(group))
                            checkElement(element.groups[group], group, true, null);
                    }
                }

                if (element.items) {
                    for (var item in element.items) {
                        if (element.items.hasOwnProperty(item))
                            checkElement(element.items[item], item, false, elementId);
                    }
                }
            }

            for (var group in filterData.groups) {
                if (filterData.groups.hasOwnProperty(group)) {
                    checkElement(filterData.groups[group], group, true, null);
                }
            }

            for (var item in filterData.items) {
                if (filterData.items.hasOwnProperty(item)) {
                    checkElement(filterData.items[item], item, false, null);
                }
            }

            hideAllGroups(ulTree);

            for (var i = 0; i < itmToShow.length; i++) {
                $('#' + treeTag.id).find('[id="' + itmToShow[i].item + '"]').addClass('searched-element');
                var pathArr = paths[itmToShow[i].group];
                if (pathArr)
                    for (var j = 0; j < pathArr.length; j++) {
                        if ($('#' + treeTag.id).find('[id="' + pathArr[j] + '"]').children('span').hasClass('glyphicon-menu-right')) {
                            $('#' + treeTag.id).find('[id="' + pathArr[j] + '"]').click();
                        }
                    }
            }

            for (var m = 0; m < grpToShow.length; m++) {
                $('#' + treeTag.id).find('[id="' + grpToShow[m] + '"]').addClass('searched-element');
                var pathGArr = paths[grpToShow[m]];
                for (var k = 0; k < pathGArr.length; k++) {
                    if ($('#' + treeTag.id).find('[id="' + pathGArr[k] + '"]').children('span').hasClass('glyphicon-menu-right')) {
                        $('#' + treeTag.id).find('[id="' + pathGArr[k] + '"]').click();
                    }
                }
            }

            $('#' + treeTag.id).find('.config-item').each(function () {
                if (!$(this).hasClass('searched-element')) {
                    $(this).parent().hide();
                }
            });
            $('#' + treeTag.id).find('.config-group').each(function () {
                if ($(this).hasClass('searched-element')) {
                    $(this).parent().find('.config-item').each(function () {
                        $(this).parent().show();
                    });
                }
            });

            $('#' + treeTag.id).find('.glyphicon-menu-right').each(function () {
                $(this).parent().hide();
            });
        });
        // Usunięcie filtrowania
        $('#filterClear_' + treeTag.id).click(function () {

            var filterInput = $('#filterInput_' + treeTag.id);
            filterInput.val('');

            $('#' + treeTag.id).find('.searched-element').each(function () {
                $(this).removeClass('searched-element');
            });

            $('#' + treeTag.id).find('li:hidden, a:hidden').show();
            setFastOptionToggle(ulTree);
            hideAllGroups(ulTree);
            loadExpandedRows(ulTree); // Rozwinięcie wierszy na podstawie listy z localstorage
            setSlowOptionToggle(ulTree);
        });
    };

    function loadItem(itemId, configData, ulTree) {
        var divItem = $(ulTree).find('#' + itemId);
        divItem.empty();
        var itemName = $(document.createElement('span'));
        itemName.text(configData.name);
        if (configData.missing)
            itemName.attr('style', 'color: red;');
        divItem.append(itemName);
        divItem.append('<span>:</span>');
        var itemValue = $(document.createElement('span'));
        itemValue.addClass('config-item-value');
        if (configData.displayValue) {
            itemValue.text(configData.displayValue);
            if (configData.modified) {
                itemValue.attr('style', 'color: #04c;');
            }
        } else {
            itemValue.text('""');
        }

        divItem.append(itemValue);
        var descItem = $(document.createElement('span'));
        descItem.addClass('config-item-desc');
        if (configData.description) {
            descItem.text(configData.description);
        }

        divItem.append(descItem);
    }

    function loadTree(configData, ulTree) {
        if (configData) {
            $(ulTree).empty();
            filterData = configData;

            for (var group in filterData.groups) {
                if (filterData.groups.hasOwnProperty(group)) {
                    findPath(filterData.groups[group], group);
                }
            }

            showData(configData, ulTree);
        }

        if ($('#filterInput_' + ulTree.id).val() !== '') {
            $('#filterInput_' + ulTree.id).trigger('input');
        } else {
            setFastOptionToggle(ulTree);
            hideAllGroups(ulTree);
            loadExpandedRows(ulTree); // Rozwinięcie wierszy na podstawie listy z localstorage
            setSlowOptionToggle(ulTree);
        }
    }

    function getChildItems(element, idElement, id) {
        if (idElement === id) {
            return element.items;
        } else {
            if (element.groups) {
                for (var groupIn in element.groups) {
                    if (element.groups.hasOwnProperty(groupIn))
                        return getChildItems(element.groups[groupIn], groupIn, id);
                }
            }
        }
    }

    function findPath(group, groupId) {

        currentPath.push(groupId);
        if (!paths.hasOwnProperty(groupId)) {
            paths[groupId] = [];
            for (var i = 0; i < currentPath.length; i++) {
                paths[groupId].push(currentPath[i]);
            }
        }

        if (group.groups) {
            for (var groupItem in group.groups) {
                if (group.groups.hasOwnProperty(groupItem)) {
                    findPath(group.groups[groupItem], groupItem);
                }
            }
        }
        var index = currentPath.indexOf(groupId);
        currentPath.splice(index, 1);
    }

    function convertPolishChars(text) {
        var src = 'ąćęśźńżółĄĆĘŚŹŃŻÓŁ';
        var dest = 'acesznzolACESZNZOL';
        var res = '';
        var c, v;

        for (var i = 0; i < text.length; i++) {
            c = text[i];
            v = src.indexOf(c);
            res += (v >= 0) ? dest[v] : c;
        }

        return res;
    }

})(window);