var butils;
(function (butils) {
    var BUtils = (function () {
        function BUtils() {
        }
        BUtils.addOptions = function (tag, values, addDefaultValue) {
            if (!tag)
                return;
            var options = [];
            $(tag).children().remove();
            if (addDefaultValue)
                options.push('<option value="---">---</option>');
            if (values && $.isArray(values) && values[0]) {
                if (typeof values[0] === 'string' || typeof values[0] === 'number') {
                    for (var i = 0; i < values.length; i++)
                        options.push('<option value="' + values[i] + '">' + values[i] + '</option>');
                } else {
                    if (values[0].name) {
                        for (var i = 0; i < values.length; i++)
                            if (values[i].hidden !== true)
                                options.push('<option value="' + values[i].key + '">' + values[i].name + '</option>');
                    } else {
                        for (var i = 0; i < values.length; i++)
                            if (values[i].hidden !== true)
                                options.push('<option value="' + values[i].key + '">' + values[i].key + '</option>');
                    }
                }
            }
            $(tag).append(options.join());
        };
        BUtils.addCustomOptions = function (tag, values, addDefaultValue, allowedKeys) {
            var options = [];
            if (values)
                for (var i = 0; i < values.length; i++)
                    if (values[i].hidden !== true) {
                        if (allowedKeys) {
                            if ($.inArray(values[i].key, allowedKeys) >= 0)
                                options.push('<option value="' + values[i].key + '">' + values[i].name + '</option>');
                        } else
                            options.push('<option value="' + values[i].key + '">' + values[i].name + '</option>');
                    }
            if (options.length > 0) {
                $(tag).children().remove();
                if (addDefaultValue)
                    $(tag).append('<option value="---">---</option>');
                $(tag).append(options.join());
            }
        };
        return BUtils;
    }());
    butils.BUtils = BUtils;
})(butils || (butils = {}));
(function ($) {
    $(function () {
        if (!window.$alert)
            window.$alert = function (title, text, onDone) {
                alert(text);
            };
    });
}(jQuery));

'use strict';
var schedulePanel;
(function (schedulePanel) {
    var Group = (function () {
        function Group(panel, parentTag) {
            this.panel = panel;
            this.parentTag = parentTag;
            this.rows = [];
            this.build();
            this.addRow();
        }
        Group.prototype.build = function () {
            var _this = this;
            $(this.parentTag).append(''
                    + '<tbody class="schedule-group-empty">'
                    + '   <tr><td class="empty-cell"></td></tr>'
                    + '</tbody>'
                    + '<tbody class="schedule-group-wrapper">'
                    + '   <tr class="schedule-group">'
                    + '      <td class="buttons-panel">'
                    + '         <button class="btn-duplicate" data-toggle="tooltip" data-placement="right" title="Powiel grupę">'
                    + '            <span class="fa fa-files-o"></span>'
                    + '         </button>'
                    + '         <button class="btn-state-switch" data-toggle="tooltip" state="true" data-placement="right" title="Wł / Wył grupę">'
                    + '            <span class="fa fa-check-square-o"></span>'
                    + '         </button>'
                    + '         <button class="btn-remove" data-toggle="tooltip" data-placement="right" title="Usuń grupę">'
                    + '            <span class="fa fa-trash-o"></span>'
                    + '         </button>'
                    + '      </td>'
                    + '      <td>'
                    + '         <table class="group-content"></table>'
                    + '      </td>'
                    + '   </tr>'
                    + '</tbody>');
            this.$groupTags = $(this.parentTag).find('tbody.schedule-group-wrapper').last();
            this.tableTag = this.$groupTags.find('table.group-content').get(0);
            this.$groupTags.find('.btn-duplicate').click(function (e) {
                _this.panel.addGroup(_this.getValue());
                $(e.currentTarget).trigger('focusout');
            });
            this.$groupTags.find('.btn-state-switch').click(function (e) {
                $(e.currentTarget).trigger('focusout');
                var state = $(e.currentTarget).attr('state') === 'true' ? true : false;
                var newState = !state;
                $(e.currentTarget).attr('state', newState ? 'true' : 'false');
                var group = _this.$groupTags.get(0);
                var checkSpan = $(_this.$groupTags).find('button.btn-state-switch').find('span').first();
                if (!newState) {
                    group.disabledElements = $(_this.tableTag).find('input, select, span, button').not(':disabled').not('.bf-hidden');
                    $(group.disabledElements).filter('span.btn-remove').hide();
                    $(group.disabledElements).prop('disabled', true);
                    $(group.disabledElements).addClass('bf-hidden');
                    $(_this.tableTag).parent().addClass('disabled-wrapper');
                    $(_this.tableTag).addClass('schedule-group-disabled');
                    checkSpan.removeClass('fa-check-square-o');
                    checkSpan.addClass('fa-square-o');
                } else {
                    if (group.disabledElements !== undefined) {
                        $(group.disabledElements).filter('span.btn-remove').show();
                        $(group.disabledElements).removeAttr('disabled');
                        $(group.disabledElements).removeClass('bf-hidden');
                        group.disabledElements = undefined;
                    }
                    $(_this.tableTag).parent().removeClass('disabled-wrapper');
                    $(_this.tableTag).removeClass('schedule-group-disabled');
                    checkSpan.removeClass('fa-square-o');
                    checkSpan.addClass('fa-check-square-o');
                }
            });
            this.$groupTags.find('.btn-remove').click(function (e) {
                _this.panel.removeGroup(_this);
                $(e.currentTarget).trigger('focusout');
            });
            this.$groupTags.find('[data-toggle="tooltip"]').tooltip();
            var elems = this.$groupTags.find('button.btn-duplicate, button.btn-state-switch');
            elems.hide();
            elems.addClass('bf-hidden');
        };
        Group.prototype.addRow = function (rowData, disableNew) {
            var row = new schedulePanel.Row(this, this.tableTag);
            this.rows.push(row);
            if (disableNew) {
                row.addedRow = true;
                row.removeCell.show(true);
            }
            if (rowData)
                row.setValue(rowData);
            if (this.rows.length > 1) {
                var elems = this.$groupTags.find('button.btn-duplicate, button.btn-state-switch');
                elems.show();
                elems.removeClass('bf-hidden');
            }
        };
        Group.prototype.rowDayOfWeekExist = function () {
            for (var i = 0; i < this.rows.length; i++)
                if (this.rows[i].typeCell.getValue() === 'daysOfWeek')
                    return true;
            return false;
        };
        Group.prototype.rowDayOfMonthExist = function () {
            for (var i = 0; i < this.rows.length; i++)
                if (this.rows[i].typeCell.getValue() === 'daysOfMonth')
                    return true;
            return false;
        };
        Group.prototype.removeRow = function (row, leaveOnList) {
            var index = $.inArray(row, this.rows);
            $(this.rows[index].rowTag).remove();
            if (!leaveOnList) {
                this.rows.splice(index, 1);
                if (this.rows.length === 1 && this.panel.groups.length > 1) {
                    this.panel.removeGroup(this);
                }
            }
        };
        Group.prototype.setValue = function (groupData) {
            for (var i = 0; i < this.rows.length; i++)
                this.removeRow(this.rows[i], true);
            this.rows = [];
            for (var i = 0; i < groupData.rowsData.length; i++)
                this.addRow(groupData.rowsData[i], true);
            this.addRow();
            var switchStateBtn = this.$groupTags.find('.btn-state-switch').get(0);
            var state = $(switchStateBtn).attr('state') === 'true' ? true : false;
            if (groupData.enabled !== state)
                $(switchStateBtn).click();
        };
        Group.prototype.getValue = function () {
            var groupData = {
                enabled: this.$groupTags.find('.btn-state-switch').last().attr('state') === 'true' ? true : false,
                rowsData: []
            };
            for (var i = 0; i < this.rows.length; i++) {
                var rVal = this.rows[i].getValue();
                if (rVal.type !== '---')
                    groupData.rowsData.push(rVal);
            }
            return groupData;
        };
        return Group;
    }());
    schedulePanel.Group = Group;
})(schedulePanel || (schedulePanel = {}));

'use strict';
var schedulePanel;
(function (schedulePanel) {
    var Panel = (function () {
        function Panel($buildTags, data) {
            this.enabled = true;
            this.defIntervalValue = 10;
            this.defIntervalUnit = 'm';
            this.defRefHour = '00:00';
            this.groups = [];
            this.setLang('pl');
            this.buildTag = $buildTags.get(0);
            this.build();
            if (data && (data.length === 0 || data[2].length === 0))
                data = undefined;
            if (!data)
                this.addGroup();
            else if (Array.isArray(data)) {
                this.setMinValue(data);
            } else {
                this.setValue(data);
            }
        }
        Panel.prototype.build = function () {
            var _this = this;
            $(this.buildTag).append(''
                    + '<div class="schedule-panel">'
                    + '   <div class="top-controll-panel">'
                    + '      <label for="active">Harmonogram aktywny: </label>'
                    + '      <button class="btn-state-switch" state="true">'
                    + '         <span class="fa fa-check-square-o"></span>'
                    + '      </button>'
                    + '   </div>'
                    + '   <form onsubmit="return false">'
                    + '      <table class="schedule-table"></table>'
                    + '   </form>'
                    + '   <div class="buttons-panels">'
                    + '      <button class="btn btn-default btn-clear">'
                    + '         <span class="fa fa-eraser"></span> Wyczyść '
                    + '      </button>'
                    + '      <button class="btn btn-default btn-add">'
                    + '         <span class="fa fa-plus"></span> Dodaj grupę '
                    + '      </button>'
                    + '   </div>'
                    + '</div>');
            this.panelTag = $(this.buildTag).find('div.schedule-panel').get(0);
            this.tableTag = $(this.panelTag).find('table').get(0);
            var buttonPanel = $(this.panelTag).find('div.buttons-panels');
            buttonPanel.find('.btn-add').click(function (e) {
                _this.addGroup();
            });
            buttonPanel.find('.btn-clear').click(function (e) {
                _this.setValue({
                    interval: 0,
                    intervalUnit: 's',
                    refHour: '0:0',
                    enabled: true,
                    groups: []
                });
            });
            $(this.panelTag).find('button.btn-state-switch').click(function (e) {
                var state = $(e.currentTarget).attr('state') === 'true' ? true : false;
                var newState = !state;
                $(e.currentTarget).attr('state', newState ? 'true' : 'false');
                var checkSpan = $(e.currentTarget).find('span').first();
                var panel = _this.panelTag;
                if (!newState) {
                    panel.disabledElements = $(_this.tableTag).find('input, select, span, button').not(':disabled').not('.bf-hidden');
                    $(panel.disabledElements).filter('span.btn-remove').hide();
                    $(panel.disabledElements).prop('disabled', true);
                    $(panel.disabledElements).addClass('bf-hidden');
                    $(_this.panelTag).find('.buttons-panels').find('button').prop('disabled', true);
                    $(_this.tableTag).addClass('not-active');
                    $(_this.tableTag).addClass('schedule-group-disabled');
                    checkSpan.removeClass('fa-check-square-o');
                    checkSpan.addClass('fa-square-o');
                } else {
                    if (panel.disabledElements !== undefined) {
                        $(panel.disabledElements).filter('span.btn-remove').show();
                        $(panel.disabledElements).removeAttr('disabled');
                        $(panel.disabledElements).removeClass('bf-hidden');
                        panel.disabledElements = undefined;
                    }
                    $(_this.panelTag).find('.buttons-panels').find('button').removeAttr('disabled');
                    $(_this.tableTag).removeClass('not-active');
                    $(_this.tableTag).removeClass('schedule-group-disabled');
                    checkSpan.removeClass('fa-square-o');
                    checkSpan.addClass('fa-check-square-o');
                }
            });
            this.intervalValueTag = $(this.panelTag).find('input[name="intervalValue"]').get(0);
            $(this.intervalValueTag).val(15);
            this.intervalUnitTag = $(this.panelTag).find('select[name="intervalUnit"]').get(0);
            var intervalOpt = [
                {key: 's', name: 'sekund'},
                {key: 'm', name: 'minut'},
                {key: 'h', name: 'godzin'},
                {key: 'd', name: 'dni'}
            ];
            butils.BUtils.addOptions(this.intervalUnitTag, intervalOpt);
            $(this.intervalUnitTag).find('option[value="' + this.defIntervalUnit + '"]').prop('selected', true);
            this.refTimeTag = $(this.panelTag).find('input[name="reftime"]').get(0);
            $(this.refTimeTag).datetimepicker({
                format: 'HH:mm',
                locale: window.moment.locale()
            });
            $(this.refTimeTag).val(this.defRefHour);
        };
        Panel.prototype.addGroup = function (groupData) {
            var group = new schedulePanel.Group(this, this.tableTag);
            this.groups.push(group);
            if (groupData)
                group.setValue(groupData);
            if (this.groups.length > 1) {
                var elems = this.groups[0].$groupTags.find('button.btn-remove');
                elems.show();
                elems.removeClass('bf-hidden');
            } else {
                var elems2 = this.groups[0].$groupTags.find('button.btn-remove');
                elems2.hide();
                elems2.addClass('bf-hidden');
            }
        };
        Panel.prototype.setLang = function (lang) {
            window.moment.locale(lang);
            window.moment.tz.add('Etc/UTC|WMT CET CEST EET EEST|-1o -10 -20 -20 -30|012121234312121212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121212121|-2ctdo 1LXo 11d0 1iO0 11A0 1o00 11A0 1on0 11A0 6zy0 HWP0 5IM0 WM0 1fA0 1cM0 1dz0 1mL0 1en0 15B0 1aq0 1nA0 11A0 1io0 17c0 1fA0 1a00 iDX0 LA0 1cM0 1cM0 1C00 Oo0 1cM0 1cM0 1zc0 Oo0 1zc0 Oo0 1zc0 Oo0 1C00 LA0 uso0 1a00 1fA0 1cM0 1cM0 1cM0 1fA0 1a00 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1fA0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cN0 1cM0 1cM0 1cM0 1cM0 1fA0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1cM0 1fA0 1o00 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00 11A0 1qM0 WM0 1qM0 WM0 1qM0 WM0 1qM0 11A0 1o00 11A0 1o00');
        };
        Panel.prototype.removeGroup = function (group, force) {
            if (this.groups.length === 1 && !force) {
                window.$alert('Błąd', 'Nie można usunąć ostatniej grupy');
                return;
            }
            var index = $.inArray(group, this.groups);
            this.groups[index].$groupTags.prev('tbody.schedule-group-empty').remove();
            this.groups[index].$groupTags.remove();
            this.groups.splice(index, 1);
            if (this.groups.length === 1) {
                var elems = this.groups[0].$groupTags.find('button.btn-remove');
                elems.hide();
                elems.addClass('bf-hidden');
            }
        };
        Panel.prototype.setValue = function (scheduleData) {
            $(this.intervalValueTag).val(scheduleData.interval);
            $(this.intervalUnitTag).val(scheduleData.intervalUnit);
            $(this.refTimeTag).val(scheduleData.refHour);
            for (var i = this.groups.length - 1; i >= 0; i--)
                this.removeGroup(this.groups[i], true);
            this.groups = [];
            for (var i = 0; i < scheduleData.groups.length; i++)
                this.addGroup(scheduleData.groups[i]);
            if (scheduleData.groups.length === 0)
                this.addGroup();
            var switchStateBtn = $(this.panelTag).find('.top-controll-panel').find('button.btn-state-switch').first().get(0);
            var state = $(switchStateBtn).attr('state') === 'true' ? true : false;
            if (scheduleData.enabled !== state)
                $(switchStateBtn).click();
        };
        Panel.prototype.getValue = function () {
            var groupsData = [];
            for (var i = 0; i < this.groups.length; i++)
                groupsData.push(this.groups[i].getValue());
            var scheduleData = {
                enabled: $(this.panelTag).find('.top-controll-panel').find('button.btn-state-switch').first().attr('state') === 'true' ? true : false,
                interval: $(this.intervalValueTag).val(),
                intervalUnit: 's',
                refHour: '0',
                groups: groupsData
            };
            return scheduleData;
        };
        Panel.prototype.setMinValue = function (minScheduleData) {
            var scheduleData;
            try {
                var groupsData = [], minGroupsData = minScheduleData[3];
                for (var i = 0; i < minGroupsData.length; i++) {
                    var minGroupData = minGroupsData[i], groupData = {
                        enabled: minGroupData.en,
                        rowsData: []
                    };
                    for (var prop in minGroupData) {
                        if (minGroupData.hasOwnProperty(prop)) {
                            switch (prop) {
                                case 'dw':
                                    groupData.rowsData.push({
                                        type: 'daysOfWeek',
                                        value: minGroupData.dw
                                    });
                                    break;
                                case 'dm':
                                    groupData.rowsData.push({
                                        type: 'daysOfMonth',
                                        value: minGroupData.dm
                                    });
                                    break;
                                case 'dr':
                                    for (var j = 0; j < minGroupData.dr.length; j++) {
                                        var dFrom = longToDate(minGroupData.dr[j][0]);
                                        var dTo = longToDate(minGroupData.dr[j][1]);
                                        groupData.rowsData.push({
                                            type: 'dateRange',
                                            value: [dFrom, dTo]
                                        });
                                    }
                                    break;
                                case 'tr':
                                    for (var k = 0; k < minGroupData.tr.length; k++) {
                                        var tFrom = secToTime(minGroupData.tr[k][0]);
                                        var tTo = secToTime(minGroupData.tr[k][1]);
                                        groupData.rowsData.push({
                                            type: 'timeRange',
                                            value: [tFrom, tTo]
                                        });
                                    }
                                    break;
                            }
                        }
                    }
                    groupsData.push(groupData);
                }
                var intervalData = secToInterval(parseInt(minScheduleData[1], 10));
                scheduleData = {
                    enabled: minScheduleData[0],
                    interval: intervalData[0],
                    intervalUnit: intervalData[1],
                    refHour: secToTime(minScheduleData[2]),
                    groups: groupsData
                };
            } catch (ex) {
                window.$alert('Błąd', 'Błąd parsowania harmonogramu');
                throw ex;
            }
            this.setValue(scheduleData);
        };
        Panel.prototype.getMinValue = function () {
            var scheduleData = this.getValue(), minGroupsData = [];
            try {
                for (var i = 0; i < scheduleData.groups.length; i++) {
                    var grp = scheduleData.groups[i], minGroupData = {
                        en: grp.enabled,
                        dw: [],
                        dm: [],
                        dr: [],
                        tr: []
                    };
                    for (var j = 0; j < grp.rowsData.length; j++) {
                        var row = grp.rowsData[j];
                        switch (row.type) {
                            case 'daysOfWeek':
                                minGroupData.dw = row.value;
                                break;
                            case 'daysOfMonth':
                                minGroupData.dm = row.value;
                                break;
                            case 'dateRange':
                                if (row.value[0] !== '' || row.value[1] !== '') {
                                    var dFrom = !row.value[0] ? null : moment(row.value[0], 'YYYY-MM-DD').toDate().getTime();
                                    var dTo = !row.value[1] ? null : moment(row.value[1], 'YYYY-MM-DD').toDate().getTime();
                                    minGroupData.dr.push([dFrom, dTo]);
                                }
                                break;
                            case 'timeRange':
                                if (row.value[0] !== '' || row.value[1] !== '') {
                                    var tFrom = !row.value[0] ? null : timeToSec(row.value[0]);
                                    var tTo = !row.value[1] ? null : timeToSec(row.value[1]);
                                    minGroupData.tr.push([tFrom, tTo]);
                                }
                                break;
                        }
                    }
                    var propsSize = 0;
                    for (var prop in minGroupData) {
                        if (minGroupData.hasOwnProperty(prop) && minGroupData[prop].length === 0)
                            delete minGroupData[prop];
                        else
                            propsSize++;
                    }
                    if (minGroupData.en !== undefined && propsSize > 1)
                        minGroupsData.push(minGroupData);
                }
            } catch (ex) {
                window.$alert('Błąd', 'Błąd parsowania harmonogramu');
                throw ex;
            }
            if (minGroupsData.length === 0)
                return undefined;
            var minScheduleData = [
                scheduleData.enabled,
                intervalToSec(parseInt('0', 10), 's'),
                timeToSec('0:0'),
                minGroupsData
            ];
            return minScheduleData;
        };
        return Panel;
    }());
    schedulePanel.Panel = Panel;
    function longToDate(long, format) {
        if (!long)
            return null;
        if (!format)
            format = 'YYYY-MM-DD';
        return moment(new Date(long)).format(format);
    }
    function timeToSec(time) {
        if (!time)
            return null;
        var timeInSec = 0, arr = time.split(':');
        timeInSec += parseInt(arr[0], 10) * 3600;
        timeInSec += parseInt(arr[1], 10) * 60;
        return timeInSec;
    }
    function secToTime(sec) {
        if (sec === 0)
            return '00:00';
        else if (!sec)
            return null;
        else {
            var hours = Math.floor(sec / 3600);
            var minutes = Math.floor((sec % 3600) / 60);
            return zeroPad(hours, 2) + ':' + zeroPad(minutes, 2);
        }
    }
    function zeroPad(num, places) {
        var zero = places - num.toString().length + 1;
        return Array(+(zero > 0 && zero)).join('0') + num;
    }
    function intervalToSec(num, unit) {
        switch (unit) {
            case 'd':
                num *= 24;
            case 'h':
                num *= 60;
            case 'm':
                num *= 60;
            case 's':
                return num;
            default:
                throw 'Nieznana jednostka interwału!';
        }
    }
    function secToInterval(sec) {
        if (!sec || isNaN(sec))
            return [10, 'm'];
        if (sec % (60 * 60 * 24) === 0)
            return [sec / (60 * 60 * 24), 'd'];
        if (sec % (60 * 60) === 0)
            return [sec / (60 * 60), 'h'];
        if (sec % (60) === 0)
            return [sec / 60, 'm'];
        return [sec, 's'];
    }
    function detailsObject(minScheduleData) {
        var scheduleData;
        if (!minScheduleData || !minScheduleData[3] || minScheduleData[3].length === 0)
            return null;
        try {
            var groupsData = [], minGroupsData = minScheduleData[3];
            for (var i = 0; i < minGroupsData.length; i++) {
                var minGroupData = minGroupsData[i], groupData = {
                    enabled: minGroupData.en,
                    rowsData: []
                };
                for (var prop in minGroupData) {
                    if (minGroupData.hasOwnProperty(prop)) {
                        switch (prop) {
                            case 'dw':
                                groupData.rowsData.push({
                                    type: 'daysOfWeek',
                                    value: minGroupData.dw
                                });
                                break;
                            case 'dm':
                                groupData.rowsData.push({
                                    type: 'daysOfMonth',
                                    value: minGroupData.dm
                                });
                                break;
                            case 'dr':
                                for (var j = 0; j < minGroupData.dr.length; j++) {
                                    var dFrom = longToDate(minGroupData.dr[j][0]);
                                    var dTo = longToDate(minGroupData.dr[j][1]);
                                    groupData.rowsData.push({
                                        type: 'dateRange',
                                        value: [dFrom, dTo]
                                    });
                                }
                                break;
                            case 'tr':
                                for (var k = 0; k < minGroupData.tr.length; k++) {
                                    var tFrom = secToTime(minGroupData.tr[k][0]);
                                    var tTo = secToTime(minGroupData.tr[k][1]);
                                    groupData.rowsData.push({
                                        type: 'timeRange',
                                        value: [tFrom, tTo]
                                    });
                                }
                                break;
                        }
                    }
                }
                groupsData.push(groupData);
            }
            var intervalData = secToInterval(parseInt(minScheduleData[1], 10));
            scheduleData = {
                enabled: minScheduleData[0],
                interval: intervalData[0],
                intervalUnit: intervalData[1],
                refHour: secToTime(minScheduleData[2]),
                groups: groupsData
            };
        } catch (ex) {
            window.$alert('Błąd', 'Błąd parsowania harmonogramu');
            return null;
        }
        if (scheduleData.groups.length === 0)
            return null;
        return scheduleData;
    }
    schedulePanel.detailsObject = detailsObject;
})(schedulePanel || (schedulePanel = {}));

'use strict';
var schedulePanel;
(function (schedulePanel) {
    var Row = (function () {
        function Row(scheduleGroup, parentTag) {
            this.scheduleGroup = scheduleGroup;
            this.parentTag = parentTag;
            this.addedRow = false;
            this.build();
        }
        Row.prototype.build = function () {
            $(this.parentTag).append(''
                    + '<tr>'
                    + '   <td>'
                    + '      <div class="input-row">'
                    + '         <div class="edit-inputs">'
                    + '            <div class="col-xs-4 row-type"></div>'
                    + '            <div class="col-xs-8 row-value"></div>'
                    + '         </div>'
                    + '         <div class="row-remove"></div>'
                    + '      </div>'
                    + '   </td>'
                    + '</tr>');
            this.rowTag = $(this.parentTag).find('tr').last().get(0);
            this.typeCell = new schedulePanel.Type(this, $(this.rowTag).find('div.row-type').get(0));
            this.valueCell = new schedulePanel.Value(this, $(this.rowTag).find('div.row-value').get(0));
            this.removeCell = new schedulePanel.Remove(this, $(this.rowTag).find('div.row-remove').get(0));
            this.removeCell.show(false);
        };
        Row.prototype.onTypeChange = function (value, callback) {
            if (this.valueCell)
                this.valueCell.changeType(value);
            if (value !== '---' && !this.addedRow) {
                this.scheduleGroup.addRow();
                this.removeCell.show(true);
                this.addedRow = true;
            }
            if (callback)
                callback();
        };
        Row.prototype.setValue = function (rowData, callback) {
            var _this = this;
            this.typeCell.setValue(rowData.type, function () {
                _this.valueCell.setValue(rowData.value, function () {
                    if (callback)
                        callback();
                });
            });
        };
        Row.prototype.getValue = function () {
            var rowData = {
                type: this.typeCell.getValue(),
                value: this.valueCell.getValue()
            };
            return rowData;
        };
        return Row;
    }());
    schedulePanel.Row = Row;
})(schedulePanel || (schedulePanel = {}));

var butils;
(function (butils) {
    var calendar;
    (function (calendar) {
        var TimeFormat = (function () {
            function TimeFormat(time) {
                console.log('dupa');
            }
            return TimeFormat;
        }());
        calendar.TimeFormat = TimeFormat;
    })(calendar = butils.calendar || (butils.calendar = {}));
})(butils || (butils = {}));

'use strict';
var schedulePanel;
(function (schedulePanel) {
    var Remove = (function () {
        function Remove(row, parentTag) {
            this.row = row;
            this.parentTag = parentTag;
            this.build();
        }
        Remove.prototype.build = function () {
            var _this = this;
            $(this.parentTag).append(''
                    + '<span class="glyphicon glyphicon-remove btn-remove" title="Usuń"></span>');
            this.removeTag = $(this.parentTag).find('span').get(0);
            $(this.removeTag).click(function (e) {
                return _this.onRemoveClick(e);
            });
        };
        Remove.prototype.onRemoveClick = function (event) {
            this.row.scheduleGroup.removeRow(this.row);
        };
        Remove.prototype.show = function (show) {
            if (show) {
                $(this.removeTag).show();
                $(this.removeTag).removeClass('bf-hidden');
            } else {
                $(this.removeTag).hide();
                $(this.removeTag).addClass('bf-hidden');
            }
        };
        return Remove;
    }());
    schedulePanel.Remove = Remove;
})(schedulePanel || (schedulePanel = {}));

'use strict';
var schedulePanel;
(function (schedulePanel) {
    var BUtils = butils.BUtils;
    var Type = (function () {
        function Type(row, parentTag) {
            this.row = row;
            this.parentTag = parentTag;
            this.build();
        }
        Type.prototype.build = function () {
            var _this = this;
            $(this.parentTag).append('<select name="type" class="form-control"></select>');
            this.typeTag = $(this.parentTag).find('select').get(0);
            $(this.typeTag).change(function (e, d) {
                return _this.onTypeChange(e, d);
            });
            BUtils.addOptions(this.typeTag, [
                {key: 'daysOfWeek', name: 'Dni tygodnia'},
                {key: 'daysOfMonth', name: 'Dni miesiąca'},
                {key: 'dateRange', name: 'Przedział dat'},
                {key: 'timeRange', name: 'Przedział czasu'}
            ], true);
            $(this.typeTag).change();
            $(this.typeTag).click(function (e) {
                return _this.checkSingleTypes(e);
            });
        };
        Type.prototype.checkSingleTypes = function (event) {
            this.setOptionState('daysOfWeek', this.row.scheduleGroup.rowDayOfWeekExist());
            this.setOptionState('daysOfMonth', this.row.scheduleGroup.rowDayOfMonthExist());
        };
        Type.prototype.setOptionState = function (optName, disable) {
            var opt = $(this.typeTag).find('option[value="' + optName + '"]');
            if (disable)
                opt.attr('disabled', 'disabled');
            else
                opt.removeAttr('disabled');
        };
        Type.prototype.onTypeChange = function (event, callback) {
            this.row.onTypeChange(this.typeTag.value, callback);
        };
        Type.prototype.setValue = function (value, callback) {
            if (value && $(this.typeTag).find('option[value="' + value + '"]').length > 0) {
                $(this.typeTag).val(value);
                $(this.typeTag).trigger('change', [callback]);
            }
        };
        Type.prototype.getValue = function () {
            return this.typeTag.value;
        };
        return Type;
    }());
    schedulePanel.Type = Type;
})(schedulePanel || (schedulePanel = {}));

'use strict';
var schedulePanel;
(function (schedulePanel) {
    var BUtils = butils.BUtils;
    var Value = (function () {
        function Value(row, parentTag) {
            this.row = row;
            this.parentTag = parentTag;
            this.build();
        }
        Value.prototype.build = function () {
            $(this.parentTag).append('<input type="text" class="form-control"/>');
            var $input = $(this.parentTag).find('input');
            $input.attr('disabled', 'disabled');
            $input.addClass('disabled-tag');
        };
        Value.prototype.destroyCurrentType = function () {
            $(this.parentTag).children().remove();
        };
        Value.prototype.changeType = function (newType) {
            var _this = this;
            this.type = newType;
            this.destroyCurrentType();
            switch (newType) {
                case 'daysOfWeek':
                    $(this.parentTag).append('<select name="value" class="w300" style="display: none;" multiple="multiple"></select>');
                    var selectW = $(this.parentTag).find('select').get(0);
                    var arrW = [];
                    for (var i = 1; i <= 31; i++)
                        arrW.push(i);
                    BUtils.addOptions(selectW, [
                        {key: '1', name: 'Pn'},
                        {key: '2', name: 'Wt'},
                        {key: '3', name: 'Śr'},
                        {key: '4', name: 'Cz'},
                        {key: '5', name: 'Pt'},
                        {key: '6', name: 'Sb'},
                        {key: '7', name: 'Nd'}
                    ], false);
                    $(selectW).multipleSelect({
                        selectAll: false,
                        minimumCountSelected: 20,
                        selectAllText: 'Zaznacz wszystkie',
                        allSelected: 'Zaznaczono wszystkie',
                        countSelected: 'Zaznaczono # z %',
                        noMatchesFound: 'Brak rezultatów',
                        width: '100%',
                        multiple: true,
                        multipleWidth: 60
                    });
                    break;
                case 'daysOfMonth':
                    $(this.parentTag).append('<select name="value" multiple="multiple"></select>');
                    var selectM = $(this.parentTag).find('select').get(0);
                    var arrM = [];
                    for (var i = 1; i <= 31; i++)
                        arrM.push(i);
                    BUtils.addOptions(selectM, arrM, false);
                    $(selectM).multipleSelect({
                        selectAll: false,
                        minimumCountSelected: 20,
                        selectAllText: 'Zaznacz wszystkie',
                        allSelected: 'Zaznaczono wszystkie',
                        countSelected: 'Zaznaczono # z %',
                        noMatchesFound: 'Brak rezultatów',
                        width: '100%',
                        multiple: true,
                        multipleWidth: 60
                    });
                    break;
                case 'dateRange':
                    $(this.parentTag).append(''
                            + '<input name="val1" type="text" class="form-control date-start"/>'
                            + '<input name="val2" type="text" class="form-control date-end"/>');
                    $(this.parentTag).find('input.date-start, input.date-end').datetimepicker({
                        format: 'YYYY-MM-DD',
                        locale: window.moment.locale()
                    });
                    var dateInputs = $(this.parentTag).find('input');
                    dateInputs.focusin(function (e) {
                        _this.inputsDateTimeChange(e, 'YYYY-MM-DD', 'date-start', 'date-end');
                    });
                    dateInputs.on('dp.change', function (e) {
                        _this.inputsDateTimeChange(e, 'YYYY-MM-DD', 'date-start', 'date-end');
                    });
                    break;
                case 'timeRange':
                    $(this.parentTag).append(''
                            + '<input name="val1" type="text" class="form-control time-start"/>'
                            + '<input name="val2" type="text" class="form-control time-end"/>');
                    $(this.parentTag).find('input.time-start, input.time-end').datetimepicker({
                        format: 'HH:mm',
                        locale: window.moment.locale()
                    });
                    var timeInputs = $(this.parentTag).find('input');
                    timeInputs.focusin(function (e) {
                        _this.inputsDateTimeChange(e, 'HH:mm', 'time-start', 'time-end');
                    });
                    timeInputs.on('dp.change', function (e) {
                        _this.inputsDateTimeChange(e, 'HH:mm', 'time-start', 'time-end');
                    });
                    break;
                default:
                    $(this.parentTag).append('<input name="value" type="text" class="form-control"/>');
                    var $input = $(this.parentTag).find('input');
                    $input.attr('disabled', 'disabled');
                    $input.addClass('disabled-tag');
                    break;
            }
        };

        Value.prototype.inputsDateTimeChange = function (event, format, startTagCls, endTagCls) {
            var values = this.getValue();
            if (event.type === 'focusin') {
                if ($(event.currentTarget).hasClass(endTagCls) && values[0] && !values[1]) {
                    this.setValue([values[0], values[0]], null);
                    return;
                } else if ($(event.currentTarget).hasClass(startTagCls) && !values[0] && values[1]) {
                    this.setValue([values[1], values[1]], null);
                    return;
                }
            } else {
                var start, end;
                if (values[0]) {
                    start = moment(values[0], format);
                    if (!values[1]) {
                        this.setValue([values[0], ''], null);
                        return;
                    }
                }
                if (values[1]) {
                    end = moment(values[1], format);
                    if (!values[0]) {
                        this.setValue(['', values[1]], null);
                        return;
                    }
                }
                if ($(event.currentTarget).hasClass(endTagCls) && end < start) {
                    this.setValue([values[1], values[1]], null);
                    return;
                } else if ($(event.currentTarget).hasClass(startTagCls) && start > end) {
                    this.setValue([values[0], values[0]], null);
                    return;
                }
                this.setValue([values[0], values[1]], null);
            }
        };
        Value.prototype.setValue = function (value, callback) {
            if (value) {
                switch (this.type) {
                    case 'daysOfWeek':
                    case 'daysOfMonth':
                        $(this.parentTag).find('select').multipleSelect('setSelects', value);
                        break;
                    case 'dateRange':
                        $(this.parentTag).find('input.date-start').val(value[0]);
                        $(this.parentTag).find('input.date-end').val(value[1]);
                        break;
                    case 'timeRange':
                        $(this.parentTag).find('input.time-start').val(value[0]);
                        $(this.parentTag).find('input.time-end').val(value[1]);
                        break;
                }
            }
        };
        Value.prototype.getValue = function () {
            var val = [];
            switch (this.type) {
                case 'daysOfWeek':
                case 'daysOfMonth':
                    val = $(this.parentTag).find('select').val();
                    if (val === null)
                        val = [];
                    break;
                case 'dateRange':
                    val = [
                        $(this.parentTag).find('input.date-start').val(),
                        $(this.parentTag).find('input.date-end').val()
                    ];
                    break;
                case 'timeRange':
                    val = [
                        $(this.parentTag).find('input.time-start').val(),
                        $(this.parentTag).find('input.time-end').val()
                    ];
                    break;
            }
            return val;
        };
        return Value;
    }());
    schedulePanel.Value = Value;
})(schedulePanel || (schedulePanel = {}));