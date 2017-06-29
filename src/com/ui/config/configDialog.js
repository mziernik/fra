//# sourceURL=file:///config/configDialog.js

function configDialog(data, saveCallback, validateCallback, lang) {

    var variableValue;
    var isVariableValue = false;
    var buttonVarText = lang['config.valueFromVar'];
    var isCheckboxes = false;
    var rows = [];

    var itemId = data.id ? data.id : data.name;

    if (data.variable !== null) {
        isVariableValue = true;
        buttonVarText = lang['config.ordinaryVal'];
    }

    var dialog = BootstrapDialog.showExt({
        title: "<span style='user-select: initial;'>" + lang['config.valueEdition'] + (itemId ? ': ' + itemId : '') + "</span>",
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

            win.data = data;
            var lblName = $(document.createElement('span'));
            lblName.attr('id', 'lblName');
            lblName.append(data.name + ':');
            form.append(lblName);
            var lblType = $(document.createElement('span'));
            lblType.attr('id', 'lblType');
            lblType.append(data.cells[0].class);
            form.append(lblType);

            var table = $(document.createElement('table'));
            table.attr('id', 'tblCfgItems');
            table.attr('style', '-khtml-user-select: none; '
                    + '-moz-user-select: none; '
                    + '-ms-user-select: none; '
                    + '-o-user-select: none; '
                    + '-webkit-user-select: none; '
                    + 'border: none; '
                    + 'margin-bottom: 6px; '
                    + 'margin-top: 8px; '
                    + 'width: 100%;');
            var tableHead = $('<thead></thead>');
            var trHead = $(document.createElement('tr'));
            tableHead.append(trHead);

            for (var i = 0; i < data.cells.length; i++) {
                var thHead = $(document.createElement('th'));
                thHead.append(data.cells[i].name !== null ? data.cells[i].name : '');
                trHead.append(thHead);
            }

            if (data.multiple) {
                var thHead = $(document.createElement('th'));
                trHead.append(thHead);
            }

            table.append(tableHead);
            var tableBody = $(document.createElement('tbody'));
            table.append(tableBody);
            var showData = data.userValue;

            if (showData.length === 0)
                showTableRow(tableBody, data.cells, data, null, true, 1, itemId);
            else
                for (var m = 0; m < showData.length; m++) {
                    showTableRow(tableBody, data.cells, data, showData, false, m, itemId);
                }

            form.append(table);

            if (data.multiple) {
                var addSpan = $(document.createElement('span'));
                addSpan.addClass('glyphicon glyphicon-plus');
                addSpan.attr('style', 'cursor: pointer; float: right; margin: 4px 2px;');
                addSpan.click(function (event) {
                    showTableRow(tableBody, data.cells, data, null, true, 1, itemId);
                    setDefaultChbx(false);
                });
                form.append(addSpan);
            }

            var hr = $(document.createElement('hr'));

            if (!data.multiple)
                hr.attr('style', 'margin-bottom: 15px; margin-top: 15px;');
            else
                hr.attr('style', 'margin-bottom: 15px; margin-top: 40px;');
            form.append(hr);

            if (data.defaultValue)
                createDefaultCheckbox(form, data.default, data.defaultDisplayValue);

            form.append(createDescription(data.description));
            form.append(createFieldLbl(data.field));

            var formVariable = $('<form class="form-horizontal smart-form" name="variableEdition"></form>');
            formVariable.attr('id', 'variableForm');
            win.$modalBody.append(formVariable);

            var inputVariables = $(document.createElement('input'));
            inputVariables.attr('placeholder', lang['config.typeNameVar']);
            inputVariables.attr('id', 'inputVariables');
            inputVariables.addClass('form-control');
            inputVariables.attr('type', 'text');
            if (data.variable !== null)
                inputVariables.val(data.variable);
            formVariable.append(inputVariables);

            inputVariables.on('change', function () {
                if (inputVariables.val() === '') {
                    $('#mainForm').removeAttr('novalidate');
                    $('#inputVariables').addClass('modalInvalid');
                    $('#errorVarNote').text(lang['config.typeNameVar']);
                } else {
                    $('#mainForm').attr('novalidate', '');
                    $('#inputVariables').removeClass('modalInvalid');
                    $('#errorVarNote').text('');
                }
            });

            var aVariables = $(document.createElement('a'));
            aVariables.attr('id', 'aVariables');
            aVariables.addClass('glyphicon glyphicon-list');
            aVariables.attr('style', 'cursor: pointer; display: inline-block; margin-left: 20px;');
            if (typeof data.variables !== typeof undefined && data.variables !== null) {
                formVariable.append(aVariables);
                inputVariables.attr('style', 'width: 90%; padding-left: 10px; padding-right: 10px; display: inline-block;');
            } else
                inputVariables.attr('style', 'width: 100%; padding-left: 10px; padding-right: 10px;');

            var errorNote = $(document.createElement('div'));
            errorNote.addClass('note');
            errorNote.attr('id', 'errorVarNote');
            errorNote.attr('style', 'color: #cc1313; padding-bottom: 10px;');
            formVariable.append(errorNote);

            aVariables.on('click', function () {
                BootstrapDialog.show({
                    title: lang['config.valueFromVar'],
                    message: document.createElement('div'),
                    closable: false,
                    onshow: function (win) {

                        var inputValue = $('#inputVariables').val();

                        var form = $(document.createElement('form'));
                        form.addClass('form-horizontal smart-form');
                        form.attr('id', 'variableForm');

                        var labelDesc = $(document.createElement('label'));
                        labelDesc.text(lang['config.chooseFromList'] + ':');
                        form.append(labelDesc);

                        form.append($(document.createElement('br')));
                        form.append($(document.createElement('br')));

                        var select = $(document.createElement('select'));
                        select.addClass('form-control');
                        select.attr('style', 'width: 100%; padding-left: 10px;');
                        select.attr('id', 'selectValue');

                        var option = $(document.createElement('option'));
                        option.attr('value', '');
                        option.append('<i>' + lang['config.lack'] + '</i>');
                        select.append(option);

                        for (var item in data.variables) {
                            if (data.variables.hasOwnProperty(item)) {
                                var option = $(document.createElement('option'));
                                option.attr('value', item);
                                option.append(data.variables[item][0] + ' [' + data.variables[item][1] + ', ' + data.variables[item][2] + ']');
                                select.append(option);

                                if (inputValue === item) {
                                    option.attr('selected', '');
                                }
                            }
                        }

                        form.append(select);

                        win.$modalBody.append(form);
                    },
                    buttons: [
                        {
                            key: 'confirm-button',
                            cssClass: 'btn-primary',
                            label: lang['config.apply'],
                            action: function (dialogRef) {

                                var selectValue = $('#selectValue').val();
                                variableValue = selectValue;
                                $('#inputVariables').val(variableValue);
                                $("#inputVariables").change();

                                dialogRef.close();
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

            formVariable.append($(document.createElement('br')));

            if (data.variable !== null) {
                form.hide();
            } else {
                formVariable.hide();
            }
        },
        buttons: [
            {
                key: 'change-button',
                id: 'change-button-id',
                cssClass: 'btn-info btn-modal-left',
                label: buttonVarText,
                action: function (dialogRef) {
                    if (isVariableValue) {
                        $('#variableForm').hide();
                        $('#mainForm').show();
                        $('#change-button-id').text(lang['config.valueFromVar']);
                    } else {
                        $('#mainForm').hide();
                        $('#variableForm').show();
                        $('#change-button-id').text(lang['config.ordinaryVal']);
                    }
                    isVariableValue = !isVariableValue;
                }
            },
            {
                key: 'confirm-button',
                cssClass: 'btn-primary',
                label: lang['config.apply'],
                action: function (dialogRef) {

                    var inputValue = null;
                    if (isVariableValue) {
                        inputValue = $('#inputVariables').val();
                        if (inputValue === '') {
                            $('#inputVariables').addClass('modalInvalid');
                            $('#errorVarNote').text(lang['config.valRequired']);
                            return;
                        }
                    } else {
                        var childrens = $('form[name=itemEdition]').find('.modalInvalid');

                        if (childrens.length !== 0)
                            return;
                    }

                    var $button = this;

                    $button.disable();
                    $button.spin();
                    dialogRef.setClosable(false);


                    var result = [];

                    for (var rowIndex = 0; rowIndex < rows.length; rowIndex++) {
                        var resultRow = [];

                        for (var rIndex = 0; rIndex < rows[rowIndex].length; rIndex++) {
                            if (Object.prototype.toString.call(rows[rowIndex][rIndex]) === '[object Array]') {
                                var tmp = [];
                                for (var rrIndex = 0; rrIndex < rows[rowIndex][rIndex].length; rrIndex++) {
                                    if ((rows[rowIndex][rIndex][rrIndex].prop('type') !== 'checkbox' && rows[rowIndex][rIndex][rrIndex].prop('type') !== 'radio') ||
                                            (rows[rowIndex][rIndex][rrIndex].prop('type') === 'checkbox' && rows[rowIndex][rIndex][rrIndex].prop('checked')))
                                        tmp.push(rows[rowIndex][rIndex][rrIndex].val());
                                    else if (rows[rowIndex][rIndex][rrIndex].prop('type') === 'radio' && rows[rowIndex][rIndex][rrIndex].prop('checked'))
                                        resultRow.push(rows[rowIndex][rIndex][rrIndex].val());
                                }

                                if (tmp.length > 0)
                                    resultRow.push(tmp);
                            } else {
                                resultRow.push(rows[rowIndex][rIndex].val());
                            }
                        }

                        result.push(resultRow);
                    }

                    var defaultValue = false;
                    var chbx = document.getElementById('cbDef');

                    if (chbx !== null) {
                        defaultValue = chbx.checked;
                    }

                    if (saveCallback) {
                        var cb = function (result) {
                            if (result.error) {
                                $button.enable();
                                $button.stopSpin();
                                dialogRef.setClosable(true);
                            }
                        };
                        saveCallback(result, defaultValue, cb, inputValue);
                    }
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

    this.close = function () {
        dialog.close();
    };

    function randomGuid() {
        return s4() + s4();
    }

    function s4() {
        return Math.floor((1 + Math.random()) * 0x10000)
                .toString(16)
                .substring(1);
    }

    function showTableRow(tableBody, cells, data, showData, empty, m, itemId) {
        var trBody = $(document.createElement('tr')),
                guid = randomGuid();
        tableBody.append(trBody);

        var row = [];
        var checkboxes = [];
        var radiobuttons = [];

        for (var k = 0; k < cells.length; k++) {
            var tdBody = $(document.createElement('td'));

            checkboxes.push([]);
            radiobuttons.push([]);

            tdBody.attr('style', 'vertical-align: top;');

            var input = null,
                    select = null,
                    unitSelect = null;

            if (!(cells[k].type === 'bool'
                    || cells[k].type === 'enum' || cells[k].type === 'date'
                    || cells[k].type === 'time' || cells[k].type === 'timestamp')) {

                var tdBodyNum = $(document.createElement('td'));

                tdBodyNum.attr('style', 'vertical-align: top;');

                if (cells[k].type === 'text' && cells[k].multiple === true) {
                    input = $(document.createElement('textarea'));

                    input.attr('rows', '5');
                } else {
                    input = $(document.createElement('input'));
                    input.attr('type', 'text');
                }

                input.addClass('form-control');

                if (cells[k].type === 'text' && cells[k].multiple === true) {
                    input.attr('style', 'width: 100%; padding-left: 10px; padding-right: 10px; resize: vertical;');
                } else {
                    input.attr('style', 'width: 100%; padding-left: 10px; padding-right: 10px;');
                }

                if (!empty) {
                    if ((cells[k].type === 'interval' || cells[k].type === 'size') && showData[m][k] && showData[m][k][0] !== null) {
                        input.val(showData[m][k][0]);
                    } else if (cells.length > 1) {
                        input.val(showData[m][k]);
                    } else {
                        input.val(showData[m]);
                    }
                } else if (cells[k].defaultValue) {
                    input.val(cells[k].defaultValue);
                }

                input.attr('name', guid + '.' + k);

                tdBodyNum.append(input);
                trBody.append(tdBodyNum);

            } else if (cells[k].type === 'date' || cells[k].type === 'time' || cells[k].type === 'timestamp') {

                var tdBodyNum = $(document.createElement('td'));
                tdBodyNum.attr('style', 'vertical-align: top;');
                var divFormGroup = $(document.createElement('div'));
                divFormGroup.addClass('form-group');
                var divInputGroup = $(document.createElement('div'));
                divInputGroup.addClass('input-group date datetimepicker');
                divInputGroup.attr('id', 'datetimepicker' + guid);
                divFormGroup.append(divInputGroup);
                input = $(document.createElement('input'));
                input.addClass('form-control');
                input.attr('type', 'text');
                input.attr('name', guid + '.' + k);
                input.attr('style', 'padding-left: 10px;');
                divInputGroup.append(input);
                var span = $(document.createElement('span'));
                span.addClass('input-group-addon');
                divInputGroup.append(span);
                var iconSpan = $(document.createElement('span'));
                iconSpan.addClass('glyphicon glyphicon-calendar');
                span.append(iconSpan);
                tdBodyNum.append(divFormGroup);
                trBody.append(tdBodyNum);

                var format;
                switch (cells[k].type) {
                    case 'date':
                        format = 'YYYY-MM-DD';
                        break;
                    case 'time':
                        format = 'HH:mm:ss';
                        break;
                    case 'timestamp':
                        format = 'YYYY-MM-DD HH:mm:ss';
                        break;
                }

                if (cells[k].type !== 'time')
                    setTimeout(function () {
                        $('#datetimepicker' + guid).datetimepicker({
                            format: format,
                            locale: 'pl',
                            defaultDate: empty ? (new Date()) : (showData[m].length !== 0 ? (new Date(showData[m])) : new Date())
                        }).on('dp.change', function (e) {
                            setDefaultChbx(false);
                        });
                    }, 300);
                else {

                    var time = empty ? ('12:00:00') : (showData[m].length !== 0 ? (showData[m][0]) : '12:00:00');

                    setTimeout(function () {
                        $('#datetimepicker' + guid).datetimepicker({
                            format: format,
                            locale: 'pl'
                        }).on('dp.change', function (e) {
                            setDefaultChbx(false);
                        });

                        input.val(time);
                    }, 300);
                }
            } else {
                var tdBodyNum = $(document.createElement('td'));

                //if (!cells[k].ordered) {
                if (cells[k].list) {
                    // jeśli to ma być select

                    tdBodyNum.attr('style', 'vertical-align: top;');
                    select = $(document.createElement('select'));
                    select.addClass('form-control');
                    select.attr('style', 'width: 100%; padding-left: 10px;');
                    select.attr('name', guid + '.' + k);

                    if (cells[k].multiple)
                        select.attr('multiple', '');

                    for (var item in cells[k].enum) {
                        if (!cells[k].enum.hasOwnProperty(item)) {
                            continue;
                        }

                        var option = $(document.createElement('option'));
                        option.attr('value', item);
                        option.append(cells[k].enum[item]);
                        select.append(option);

                        if (!empty) {
                            if (cells.length > 1 && (showData[m][k] === item ||
                                    (typeof showData[m][k] !== "undefined" && showData[m][k] !== null && showData[m][k].toString() === item)) ||
                                    (typeof showData[m] !== "undefined" && showData[m] !== null && showData[m].toString() === item)) {
                                select.val(item);
                            }
                        } else if (cells[k].defaultValue) {
                            if (item === cells[k].defaultValue)
                                select.val(item);
                        }
                    }

                    select.change(function () {
                        setDefaultChbx(false);
                    });

                    tdBodyNum.append(select);

                } else if (cells[k].ordered || cells[k].multiple) {
                    // jeśli to ma być lista checkboxów ze zmianą kolejności lub bez
                    var table = document.createElement('table');
                    var tbody = document.createElement('tbody');
                    table.appendChild(tbody);
                    $(table).attr('id', guid + k + 'table');

                    if (cells[k].ordered) {
                        var fixHelperModified = function (e, tr) {
                            var $originals = tr.children();
                            var $helper = tr.clone();
                            $helper.children().each(function (index) {
                                $(this).width($originals.eq(index).width());
                            });
                            return $helper;
                        };

                        var number = k;

                        $(tbody).sortable({
                            helper: fixHelperModified,
                            start: function (event, ui) {
                                $(ui.item).data("startindex", ui.item.index());
                            },
                            stop: function (event, ui) {
                                ui.item.data('startindex');
                                ui.item.index();

                                var tmp = checkboxes[number].splice(ui.item.data('startindex'), 1);
                                checkboxes[number].splice(ui.item.index(), 0, tmp[0]);
                            }
                        }).disableSelection();
                    }

                    var enumObj = jQuery.extend(true, {}, cells[k].enum);

                    if (!empty && showData[m].length > 0) {
                        var counter = -1;

                        if (cells.length > 1) {
                            counter = k;
                        } else if (showData[m][0] !== null) {
                            counter = 0;
                        }

                        if (counter > -1)
                            for (var z = 0; z < showData[m][counter].length; z++) {
                                var checkbox = createCheckbox(tbody, showData[m][counter][z],
                                        cells[k].enum[showData[m][counter][z]], guid + '.' + k, true, cells[k].ordered);
                                checkboxes[k].push(checkbox);
                                delete enumObj[showData[m][counter][z]];
                            }
                    }

                    for (var item in enumObj) {
                        if (!enumObj.hasOwnProperty(item)) {
                            continue;
                        }
                        var checkbox = createCheckbox(tbody, item, enumObj[item], guid + '.' + k, false, cells[k].ordered);
                        checkboxes[k].push(checkbox);
                    }

                    tdBodyNum.append(table);
                } else {
                    // radiobuttony

                    var table = document.createElement('table');
                    var tbody = document.createElement('tbody');
                    table.appendChild(tbody);
                    $(table).attr('id', guid + k + 'table');

                    var enumObj = jQuery.extend(true, {}, cells[k].enum);

                    for (var item in enumObj) {
                        if (!enumObj.hasOwnProperty(item)) {
                            continue;
                        }

                        var tr = $(document.createElement('tr'));
                        $(tbody).append(tr);

                        var tdRadio = $(document.createElement('td'));
                        tdRadio.attr('style', 'padding: 0px 15px 0px 2px;');

                        var inputRadio = $(document.createElement('input'));
                        inputRadio.attr('type', 'radio');
                        inputRadio.attr('name', guid + '.' + k);
                        inputRadio.attr('id', guid + '.' + k + item);
                        inputRadio.val(item);

                        tdRadio.append(inputRadio);
                        tr.append(tdRadio);

                        var tdLabel = $(document.createElement('td'));
                        tdLabel.attr('style', 'vertical-align: top;');

                        var lblRadio = $(document.createElement('label'));
                        lblRadio.attr('for', guid + '.' + k + item);
                        lblRadio.text(enumObj[item]);

                        tdLabel.append(lblRadio);
                        tr.append(tdLabel);

                        if (!empty) {
                            if ((cells.length > 1 && showData[m][k] === item) ||
                                    (showData[m] !== null && showData[m].toString() === item)) {
                                inputRadio.attr('checked', 'checked');
                            }
                        } else if (cells[k].defaultValue) {
                            if (cells[k].defaultValue === item)
                                inputRadio.attr('checked', 'checked');
                        }

                        radiobuttons[k].push(inputRadio);
                    }

                    tdBodyNum.append(table);
                }

                trBody.append(tdBodyNum);
            }

            if (cells[k].units) {
                var units = cells[k].units;
                var unitSelect = $(document.createElement('select'));
                var selectedUnit = false;

                unitSelect.addClass('form-control');
                unitSelect.attr('style', 'width: 100%; padding-left: 10px;');
                unitSelect.attr('name', guid + '.' + k);

                for (var unit in units) {
                    var option = $(document.createElement('option'));
                    option.attr('value', unit);
                    option.append(units[unit]);
                    unitSelect.append(option);

                    if (!empty && showData[m][k] && showData[m][k][1] === unit) {
                        selectedUnit = true;
                        unitSelect.val(unit);
                    }
                }

                if (!selectedUnit && cells[k].selectedUnit !== null) {
                    unitSelect.val(cells[k].selectedUnit);
                }

                unitSelect.change(function () {
                    setDefaultChbx(false);
                    validation(row, errorNote);
                });

                tdBody.append(unitSelect);
                trBody.append(tdBody);
            }

            var errorNote = $(document.createElement('div'));
            errorNote.addClass('note');
            errorNote.attr('style', 'color: #cc1313; padding-bottom: 10px;');

            if (input) {
                if (unitSelect) {
                    var unitArr = [];
                    unitArr.push(input);
                    unitArr.push(unitSelect);

                    row.push(unitArr);
                } else {
                    row.push(input);
                }

                if (data.defaultValue)
                    input.on('input', function () {
                        setDefaultChbx(false);
                    });

                input.change(function () {
                    validation(row, errorNote);
                });
            }

            if (select) {
                if (unitSelect) {
                    var unitArr = [];
                    unitArr.push(select);
                    unitArr.push(unitSelect);

                    row.push(unitArr);
                } else {
                    row.push(select);
                }

                if (data.defaultValue)
                    select.on('change', function () {
                        setDefaultChbx(false);
                        validation(row, errorNote);
                    });
                else
                    select.on('change', function () {
                        validation(row, errorNote);
                    });
            }

            if (checkboxes[k].length > 0) {
                row.push(checkboxes[k]);
                isCheckboxes = true;
            }

            for (var z = 0; z < checkboxes[k].length; z++) {
                if (data.defaultValue)
                    checkboxes[k][z].change(function () {
                        setDefaultChbx(false);
                        validation(row, errorNote);
                    });
                else
                    checkboxes[k][z].change(function () {
                        validation(row, errorNote);
                    });
            }

            if (radiobuttons[k].length > 0) {
                row.push(radiobuttons[k]);
                isCheckboxes = true;
            }

            for (var w = 0; w < radiobuttons[k].length; w++) {
                if (data.defaultValue)
                    radiobuttons[k][w].change(function () {
                        setDefaultChbx(false);
                        validation(row, errorNote);
                    });
                else
                    radiobuttons[k][w].change(function () {
                        validation(row, errorNote);
                    });
            }
        }

        rows.push(row);

        if (data.multiple) {
            var td = $(document.createElement('td'));
            td.attr('style', 'padding-left: 15px; padding-right: 3px; width: 16px;');
            var removeSpan = $(document.createElement('span'));
            removeSpan.addClass('glyphicon glyphicon-remove');
            removeSpan.attr('style', 'cursor: pointer; vertical-align: middle;');
            td.click(function (event) {
                if (rows.indexOf(row) > -1)
                    rows.splice(rows.indexOf(row), 1);
                event.currentTarget.parentNode.remove();
                setDefaultChbx(false);
            });
            td.append(removeSpan);
            trBody.append(td);
        }
    }

    function setDefaultChbx(value) {
        var chbx = document.getElementById('cbDef');
        if (chbx !== null) {
            chbx.checked = value;
        }
    }

    function createFieldLbl(field) {
        if (typeof field === 'undefined')
            return;

        var fieldDiv = $(document.createElement('div'));
        fieldDiv.attr('style', 'color: #555; font: 10pt Courier New; margin-top: 16px; user-select: initial;');
        fieldDiv.append(field);
        return fieldDiv;
    }

    function createDefaultCheckbox(form, defaultValue, displayValue) {
        var inputChbx = $(document.createElement('input'));
        inputChbx.addClass('checkbox');
        inputChbx.attr('type', 'checkbox');
        inputChbx.attr('id', 'cbDef');
        inputChbx.attr('name', 'default');
        inputChbx.attr('style', 'visibility: visible !important; min-height: 18px; width: auto; height: auto;');
        if (defaultValue) {
            inputChbx.attr('checked', 'true');
        }

        form.append(inputChbx);

        var labelDef = $(document.createElement('label'));
        labelDef.attr('id', 'lblDefault');
        labelDef.attr('style', 'margin-left: 25px; font-style: italic; cursor: pointer;');
        labelDef.attr('for', 'cbDef');
        var labelSpan = $(document.createElement('span'));
        labelSpan.append(lang['config.valDefault'] + ': ');
        var labelDefValue = $(document.createElement('span'));
        labelDefValue.attr('style', 'user-select: initial;');
        labelDefValue.text(displayValue);
        labelDef.append(labelSpan);
        labelDef.append(labelDefValue);
        form.append(labelDef);
    }

    function createDescription(descriptionTxt) {
        if (typeof descriptionTxt === 'undefined')
            return;

        var description = $(document.createElement('div'));
        description.addClass('edit-description');
        description.attr('style', 'margin-top: 10px;');
        var descContent = $(document.createElement('div'));
        descContent.text(descriptionTxt);
        description.append(descContent);

        return description;
    }

    function createCheckbox(tbody, value, txtLbl, name, checked, ordered) {
        var tr = $(document.createElement('tr'));
        $(tbody).append(tr);

        var tdCheckbox = $(document.createElement('td'));
        tdCheckbox.attr('style', 'padding: 0px 15px 0px 2px;');
        var checkbox = $(document.createElement('input'));
        checkbox.attr('type', 'checkbox');
        checkbox.attr('value', value);
        checkbox.attr('name', name);
        checkbox.attr('id', name);

        if (checked)
            checkbox.attr('checked', '');

        tdCheckbox.append(checkbox);
        tr.append(tdCheckbox);

        var tdLabel = $(document.createElement('td'));
        tdLabel.attr('style', 'vertical-align: top;');
        var label = $(document.createElement('label'));
        label.attr('for', name + value);
        label.text(txtLbl);
        if (ordered)
            label.attr('style', 'cursor: move');
        tdLabel.append(label);
        tr.append(tdLabel);

        return checkbox;
    }

    function validation(row, errorNote) {

        var attribute = $('#mainForm').attr('novalidate');

        if ((typeof attribute !== typeof undefined && attribute !== false)
                || validateCallback === null || typeof validateCallback === typeof undefined)
            return;

        var result = [];
        var resultRow = [];

        for (var rIndex = 0; rIndex < row.length; rIndex++) {
            if (Object.prototype.toString.call(row[rIndex]) === '[object Array]') {
                var tmp = [];
                for (var rrIndex = 0; rrIndex < row[rIndex].length; rrIndex++)
                    if ((row[rIndex][rrIndex].prop('type') !== 'checkbox' && row[rIndex][rrIndex].prop('type') !== 'radio') ||
                            (row[rIndex][rrIndex].prop('type') === 'checkbox' && row[rIndex][rrIndex].prop('checked')))
                        tmp.push(row[rIndex][rrIndex].val());
                    else if (row[rIndex][rrIndex].prop('type') === 'radio' && row[rIndex][rrIndex].prop('checked'))
                        resultRow.push(row[rIndex][rrIndex].val());

                if (tmp.length > 0)
                    resultRow.push(tmp);
            } else {
                resultRow.push(row[rIndex].val());
            }
        }

        result.push(resultRow);

        var cb = function (response) {
            if (!response.correct) {
                for (var rIndex = 0; rIndex < row.length; rIndex++) {
                    if (Object.prototype.toString.call(row[rIndex]) === '[object Array]') {
                        for (var rrIndex = 0; rrIndex < row[rIndex].length; rrIndex++) {
                            row[rIndex][rrIndex].addClass('modalInvalid');
                        }
                    } else {
                        row[rIndex].addClass('modalInvalid');
                    }
                }

                var errorMsg = document.createElement('span');
                $(errorMsg).attr('style', 'position: absolute;');
                $(errorMsg).text(response.message);
                errorNote.empty();
                errorNote.append(errorMsg);

                if (row.length > 0) {
                    var object;

                    if (Object.prototype.toString.call(row[0]) !== '[object Array]') {
                        object = row[0];
                    } else if (isCheckboxes) {
                        object = row[0][row[0].length - 1];
                    } else {
                        object = row[0][0];
                    }

                    var list = object.parent().find('div');
                    if (list.length === 0) {
                        var parent = object.parent();
                        if (!parent.attr('class') || parent.attr('class').indexOf('datetimepicker') === -1)
                            parent.append(errorNote);
                        else
                            parent.parent().append(errorNote);
                    }
                }
            } else {

                var responseCells = response.cells;

                for (var rIndex = 0; rIndex < row.length; rIndex++) {
                    if (Object.prototype.toString.call(row[rIndex]) === '[object Array]') {
                        for (var rrIndex = 0; rrIndex < row[rIndex].length; rrIndex++) {
                            row[rIndex][rrIndex].removeClass('modalInvalid');
                            if (!isCheckboxes)
                                row[rIndex][rrIndex].val(responseCells[rIndex][rrIndex]);
                        }
                    } else {
                        row[rIndex].removeClass('modalInvalid');
                        if (!isCheckboxes)
                            row[rIndex].val(responseCells[rIndex]);
                    }
                }

                if (row.length > 0) {
                    var object;

                    if (Object.prototype.toString.call(row[0]) !== '[object Array]') {
                        object = row[0];
                    } else if (isCheckboxes) {
                        object = row[0][row[0].length - 1];
                    } else {
                        object = row[0][0];
                    }

                    var list = object.parent().find('div');
                    if (list.length > 0) {
                        list.remove();
                    }
                }
            }
        };

        validateCallback(result, cb);
    }
}