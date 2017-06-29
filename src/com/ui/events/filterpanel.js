//# sourceURL=file:///events/filterpanel.js

var jFilterPanel;
(function (jFilterPanel) {
    var DataProvider = (function () {
        function DataProvider(options) {
            this.filtersList = undefined;
            this.activeFiltersList = undefined;
            this.conditionsList = undefined;
            this.options = options;
            this.setFilterList(options.filters);
            this.setActiveFiltersList(options.activeFilters);
            this.setConditionsList(jFilterPanel.conditions);
        }
        DataProvider.prototype.setFilterList = function (filterList) {
            this.filtersList = filterList;
        };
        DataProvider.prototype.setActiveFiltersList = function (activeFiltersList) {
            if ($.isArray(activeFiltersList) && activeFiltersList.length > 0) {
                var firstElem = activeFiltersList[0];
                if ($.isArray(firstElem) && firstElem.length > 0) {
                    var filterValues = [];
                    for (var i = 0; i < activeFiltersList.length; i++) {
                        filterValues.push({
                            key: activeFiltersList[i][0],
                            cond: activeFiltersList[i][1],
                            value: activeFiltersList[i][2]
                        });
                    }
                    this.activeFiltersList = [{
                            on: true,
                            filters: filterValues
                        }];
                } else if (typeof firstElem === 'object') {
                    var objElem = firstElem;
                    if (objElem.on !== undefined) {
                        if (objElem.filters.length > 0 && $.isArray(objElem.filters[0])) {
                            var tmpFltrGrpVals = [];
                            for (var i = 0; i < activeFiltersList.length; i++) {
                                var tmpFltrGrpVal = [], grp = activeFiltersList[i];
                                for (var j = 0; j < grp.filters.length; j++) {
                                    tmpFltrGrpVal.push({
                                        key: grp.filters[j][0],
                                        cond: grp.filters[j][1],
                                        value: grp.filters[j][2]
                                    });
                                }
                                tmpFltrGrpVals.push({
                                    on: activeFiltersList[i].on,
                                    filters: tmpFltrGrpVal
                                });
                            }
                            this.activeFiltersList = tmpFltrGrpVals;
                        } else {
                            this.activeFiltersList = activeFiltersList;
                        }
                    } else {
                        this.activeFiltersList = [{
                                on: true,
                                filters: activeFiltersList
                            }];
                    }
                }
            }
            this.getMultiFilterData();
        };
        DataProvider.prototype.setConditionsList = function (conditionsList) {
            var lConditionsLang = this.options.lang.lConditions;
            for (var i = 0; i < conditionsList.length; i++) {
                conditionsList[i].name = lConditionsLang[conditionsList[i].key];
                if (conditionsList[i].values) {
                    var values = conditionsList[i].values;
                    for (var j = 0; j < values.length; j++) {
                        if (values[j].key === 'less' || values[j].key === 'greater')
                            values[j].name = '';
                        else
                            values[j].name = lConditionsLang[values[j].key];
                    }
                }
            }
            this.conditionsList = conditionsList;
        };
        DataProvider.prototype.getMultiFilterData = function () {
            var tmpFilterList = [], aFltr = this.activeFiltersList;
            if (aFltr) {
                for (var i = 0; i < aFltr.length; i++) {
                    if (aFltr[i].filters) {
                        var fltr = aFltr[i].filters;
                        for (var j = 0; j < fltr.length; j++) {
                            if ($.inArray(fltr[j].key, tmpFilterList) < 0)
                                tmpFilterList.push(fltr[j].key);
                        }
                    }
                }
            }
            this.options.onGetMultiFilterData(tmpFilterList, function (filtersData) {
                if (filtersData) {
                    for (var i = 0; i < filtersData.length; i++) {
                        if (!sessionStorage[filtersData[i].key])
                            sessionStorage.setItem('filterdata_' + filtersData[i].key, JSON.stringify(filtersData[i]));
                    }
                }
            });
        };
        DataProvider.prototype.getFilterData = function (filterKey, callback) {
            var storageFilterData = sessionStorage.getItem('filterdata_' + filterKey);
            if (storageFilterData) {
                callback(JSON.parse(storageFilterData));
                return;
            }
            this.options.onGetFilterData(filterKey, function (datas) {
                console.log("filterKey: ", filterKey);
                console.log("datas: ", datas);
                if (!sessionStorage[datas.key])
                    sessionStorage.setItem('filterdata_' + datas.key, JSON.stringify(datas));
                callback(datas);
            });
        };
        return DataProvider;
    }());
    jFilterPanel.DataProvider = DataProvider;

    //--------------------------------------------------------------------------
    var FCondition = (function () {
        function FCondition(row, parentTag) {
            this.row = row;
            this.parentTag = parentTag;
            this.condOptions = this.row.dataProvider.conditionsList;
            this.build();
        }
        FCondition.prototype.build = function () {
            var _this = this;
            $(this.parentTag).append(''
                    + '<select name="cond" class="form-control">'
                    + '   <option value="---">---</option>'
                    + '</select>');
            this.cond1Tag = $(this.parentTag).find('select').get(0);
            $(this.cond1Tag).change(function (event, callback) {
                return _this.onCond1Change(event, callback);
            });
            $(this.cond1Tag).change();
        };
        FCondition.prototype.onCond1Change = function (event, callback) {
            var _this = this;
            var secondCondVals = ['similar', 'less', 'greater', 'older_than', 'younger_than'];
            if ($.inArray(this.cond1Tag.value, secondCondVals) >= 0) {
                if (!this.cond2Tag) {
                    $(this.parentTag).append('<select name="cond2" class="form-control"></select>');
//                    $(this.parentTag).append('<select name="cond2" class="form-control disabled"></select>');
                    this.cond2Tag = $(this.parentTag).find('select[name="cond2"]').get(0);
                }
                for (var i = 0; i < this.condOptions.length; i++)
                    if (this.condOptions[i].key === this.cond1Tag.value)
                        BUtils.addCustomOptions(this.cond2Tag, this.condOptions[i].values, false, this.cond2Options[this.cond1Tag.value]);
                $(this.cond2Tag).off('change').change(function (event, callback) {
                    return _this.onCond2Change(event, callback);
                });
                $(this.cond2Tag).change();
            } else {
                $(this.cond2Tag).remove();
                this.cond2Tag = undefined;
                this.row.onConditionChange(this.cond1Tag.value, callback);
            }
        };
        FCondition.prototype.onCond2Change = function (event, callback) {
            this.row.onConditionChange(this.cond2Tag.value, callback);
        };
        FCondition.prototype.setConditions = function (conditionsKeys) {
            var cond2Values = {};
            var realCondKeys = [];
            for (var i = 0; i < conditionsKeys.length; i++) {
                var multiCondValue = this.getMultiCondValue(conditionsKeys[i]);
                if (multiCondValue) {
                    if ($.inArray(multiCondValue, realCondKeys) < 0)
                        realCondKeys.push(multiCondValue);
                    if (cond2Values[multiCondValue])
                        cond2Values[multiCondValue].push(conditionsKeys[i]);
                    else
                        cond2Values[multiCondValue] = [conditionsKeys[i]];
                } else
                    realCondKeys.push(conditionsKeys[i]);
            }
            this.cond2Options = cond2Values;
            BUtils.addCustomOptions(this.cond1Tag, this.condOptions, false, realCondKeys);
        };
        FCondition.prototype.disable = function (disable) {
            if (disable === true) {
                $(this.cond1Tag).attr('disabled', 'disabled');
                $(this.cond1Tag).addClass('disabled-tag');
                $(this.cond2Tag).remove();
                this.cond2Tag = undefined;
                BUtils.addCustomOptions(this.cond1Tag, this.condOptions, true, undefined);
            } else if (disable === false) {
                $(this.cond1Tag).removeAttr('disabled');
                $(this.cond1Tag).removeClass('disabled-tag');
            }
            $(this.cond1Tag).change();
        };
        FCondition.prototype.setValue = function (value, callback) {
            var multiCondValue = this.getMultiCondValue(value);
            if (multiCondValue) {
                $(this.cond1Tag).val(multiCondValue);
                $(this.cond1Tag).change();
                $(this.cond2Tag).val(value);
                $(this.cond2Tag).trigger('change', [callback]);
            } else {
                $(this.cond1Tag).val(value);
                $(this.cond1Tag).trigger('change', [callback]);
            }
        };
        FCondition.prototype.getMultiCondValue = function (condition) {
            if (condition.indexOf('similar') === 0) {
                return 'similar';
            } else if (condition.indexOf('less') === 0) {
                return 'less';
            } else if (condition.indexOf('greater') === 0) {
                return 'greater';
            } else if (condition.indexOf('older_than') === 0) {
                return 'older_than';
            } else if (condition.indexOf('younger_than') === 0) {
                return 'younger_than';
            }
            return undefined;
        };
        FCondition.prototype.getValue = function () {
            return this.cond2Tag ? this.cond2Tag.value : this.cond1Tag.value;
        };
        return FCondition;
    }());
    jFilterPanel.FCondition = FCondition;

    //--------------------------------------------------------------------------
    var FFilter = (function () {
        function FFilter(row, parentTag) {
            this.row = row;
            this.parentTag = parentTag;
            this.build();
        }
        FFilter.prototype.build = function () {
            var _this = this;
            $(this.parentTag).append('<select name="filter" class="form-control"></select>');
            this.filterTag = $(this.parentTag).find('select').get(0);
            $(this.filterTag).change(function (e, d) {
                return _this.onFilterChange(e, d);
            });
            BUtils.addOptions(this.filterTag, this.row.dataProvider.filtersList, true);
            $(this.filterTag).change();
        };
        FFilter.prototype.onFilterChange = function (event, callback) {
            this.row.onFilterChange(this.filterTag.value, callback);
        };
        FFilter.prototype.setValue = function (value, callback) {
            if (value && $(this.filterTag).find('option[value="' + value + '"]').length > 0) {
                $(this.filterTag).val(value);
                $(this.filterTag).trigger('change', [callback]);
            }
        };
        FFilter.prototype.getValue = function () {
            return this.filterTag.value;
        };
        return FFilter;
    }());
    jFilterPanel.FFilter = FFilter;

    //--------------------------------------------------------------------------
    var FilterPanel = (function () {
        function FilterPanel($element, options) {
            this.filtersGroups = [];
            this.parentTag = $element.get(0);
            this.options = options;
            this.options.lang = jFilterPanel.i18n[this.options.locale];
            this.lang = this.options.lang;
            this.dataProvider = new jFilterPanel.DataProvider(this.options);
            this.build();
        }
        FilterPanel.prototype.build = function () {
            var _this = this;
            $(this.parentTag).append(''
                    + '<div class="panel panel-default filter-panel" style="margin-bottom: 0px; border: none">'
                    + '   <div class="panel-heading filter-header">'
                    + '      <div class="filter-title">'
                    + '               <span class="glyphicon glyphicon-menu-down"></span>'
                    + ' ' + this.lang.lFilterPanel
                    + '      </div>'
//                + '         <span class="filter-search-text">'
//                + ' ' + this.lang.lSearch
//                + '         </span>'
//                + '         <input class="filter-search form-control input-sm"></input>'
                    + '         <button class="btn btn-refresh">' //btn-success 
                    + '            <span class="glyphicon glyphicon-refresh"></span>'
//                + ' ' + this.lang.lRefresh
                    + '         </button>'
                    + '   </div>'
                    + '   <div class="panel-body">'
                    + '      <form onsubmit="return false">'
                    + '         <div class="filter-table-wrapper">'
                    + '             <table class="filters-groups">'
                    + '             </table>'
                    + '         </div>'
                    + '         <div class="profile-panel"></div>'
                    + '         <div class="buttons-panel">'
                    + '            <button class="btn btn-default btn-clear">'
                    + '               <span class="glyphicon glyphicon-erase"></span>'
                    + ' ' + this.lang.lClear
                    + '            </button>'
                    + '            <button class="btn btn-success btn-filter">'
                    + '               <span class="glyphicon glyphicon-filter"></span>'
                    + ' ' + this.lang.lFilter
                    + '            </button>'
                    + '         </div>'
                    + '         <div class="data-content"></div>'
                    + '      </form>'
                    + '   </div>'
                    + '</div>');
            this.panelTag = $(this.parentTag).find('div.panel.panel-default').get(0);
            this.dataListContent = $(this.parentTag).find('div.data-content').get(0);
            this.panelGroupsTag = $(this.panelTag).find('table.filters-groups').get(0);
            $(this.panelTag).find('button.btn-refresh').click(function (e) {
                return _this.onFilter(e);
            });
            $(this.panelTag).find('button.btn-filter').click(function (e) {
                return _this.onFilter(e);
            });
            $(this.panelTag).find('button.btn-clear').click(function (e) {
                return _this.onClear(e);
            });
            var titleTag = $(this.panelTag).find('.filter-title');
            $(titleTag).click(function () {
                var span = $(_this.panelTag).find('.filter-title span.glyphicon').get(0);
                if ($(span).hasClass('glyphicon-menu-down')) {
                    $(span).removeClass('glyphicon-menu-down');
                    $(span).addClass('glyphicon-menu-right');
                    $(_this.panelTag).find('.panel-body').hide();
                    sessionStorage.setItem('panelVisible', 'false');
                } else {
                    $(span).removeClass('glyphicon-menu-right');
                    $(span).addClass('glyphicon-menu-down');
                    $(_this.panelTag).find('.panel-body').show();
                    sessionStorage.setItem('panelVisible', 'true');
                }
            });
            this.profileManager = new jFilterPanel.ProfileManager(this, $(this.panelTag).find('.profile-panel').get(0), this.options, 'profile');
            this.addFilterGroup();
            if (sessionStorage.getItem('panelVisible') !== 'true') {
                $(titleTag).click();
            }
        };
        FilterPanel.prototype.onFilter = function (event) {
            this.options.onFilter(this.getValue(true));
        };
        FilterPanel.prototype.onClear = function (event) {
//            sessionStorage.clear();
            $(this.panelGroupsTag).find('input:enabled').val('').change();
            $(this.panelGroupsTag).find('select[name="value"]:enabled').each(function (index, element) {
                $(element).find('option:first').prop('selected', true);
            });
            this.options.onClear();
        };
        FilterPanel.prototype.setFiltersList = function (filtersList) {
            this.dataProvider.setFilterList(filtersList);
            this.loadData(this.dataProvider.activeFiltersList);
        };
        FilterPanel.prototype.addFilterGroup = function (filterGroupData) {
            var filterGroup = new jFilterPanel.FiltersGroup(this, this.panelGroupsTag);
            if (filterGroupData)
                filterGroup.loadData(filterGroupData);
            this.filtersGroups.push(filterGroup);
        };
        FilterPanel.prototype.loadData = function (filterGroupsData) {
            if (filterGroupsData)
                this.dataProvider.setActiveFiltersList(filterGroupsData);
            for (var i = 0; i < this.filtersGroups.length; i++) {
                $(this.filtersGroups[i].filtersGroupTag).next('tr.empty').first().remove();
                $(this.filtersGroups[i].filtersGroupTag).remove();
                $(this.filtersGroups[i].filtersGroupWrapper).remove();
            }
            this.filtersGroups = [];
            if (!this.dataProvider.filtersList || !this.dataProvider.activeFiltersList) {
                this.addFilterGroup();
                return;
            }
            for (var i = 0; i < this.dataProvider.activeFiltersList.length; i++)
                this.addFilterGroup(this.dataProvider.activeFiltersList[i]);
            var self = this;
            setTimeout(function () {
                if (self.dataProvider.activeFiltersList !== undefined) {
                    self.options.onFilter(self.convertGroupsValueFromObjToArr(self.dataProvider.activeFiltersList));
                }
            }, 0);
        };
        FilterPanel.prototype.getValue = function (arrayVersion) {
            var filterGroupValues = [];
            for (var i = 0; i < this.filtersGroups.length; i++)
                filterGroupValues.push(this.filtersGroups[i].getValue());
            if (arrayVersion === true)
                return this.convertGroupsValueFromObjToArr(filterGroupValues);
            return filterGroupValues;
        };
        FilterPanel.prototype.convertGroupsValueFromObjToArr = function (filterGroupValues) {
            var tmpFltrGrpVals = [];
            for (var i = 0; i < filterGroupValues.length; i++) {
                var tmpFltrGrpVal = [], grp = filterGroupValues[i];
                for (var j = 0; j < grp.filters.length; j++) {
                    tmpFltrGrpVal.push([
                        grp.filters[j].key,
                        grp.filters[j].cond,
                        grp.filters[j].value
                    ]);
                }
                tmpFltrGrpVals.push({
                    on: filterGroupValues[i].on,
                    filters: tmpFltrGrpVal
                });
            }
            return tmpFltrGrpVals;
        };
        FilterPanel.prototype.getCurrentFilters = function (callback) {
            callback(this.getValue(true));
        };
        FilterPanel.prototype.duplicate = function (filterGroupData) {
            filterGroupData.on = true;
            this.addFilterGroup(filterGroupData);
        };
        FilterPanel.prototype.removeGroup = function (filterGroup) {
            if (this.filtersGroups.length === 1) {
                BUtils.showAlert(null, this.lang.lErrors.cantDeleteLastGroup);
                return;
            }
            $(filterGroup.filtersGroupTag).next('tr.empty').first().remove();
            $(filterGroup.filtersGroupTag).remove();
            $(filterGroup.filtersGroupWrapper).remove();
            var index = $.inArray(filterGroup, this.filtersGroups);
            this.filtersGroups.splice(index, 1);
            if (this.filtersGroups.length === 1) {
                $(this.filtersGroups[0].filtersGroupTag).find('button.btn-remove')
                        .css("visibility", "hidden");
            }
        };
        FilterPanel.prototype.disableGroup = function (filterGroup, disable) {
            filterGroup.disable(disable);
        };
        return FilterPanel;
    }());
    jFilterPanel.FilterPanel = FilterPanel;

    //--------------------------------------------------------------------------
    var FiltersGroup = (function () {
        function FiltersGroup(filterPanel, parentTag) {
            this.rows = [];
            this.disabled = false;
            this.parentTag = parentTag;
            this.filterPanel = filterPanel;
            this.build();
        }
        FiltersGroup.prototype.build = function () {
            var _this = this;
            var lang = this.filterPanel.lang;
            $(this.parentTag).append(''
                    + '<tbody class="filter-group-wrapper">'
                    + '   <tr class="filter-group">'
                    + '      <td class="group-controll">'
                    + '         <button class="btn-duplicate" data-toggle="tooltip" data-placement="right" title="' + lang.lDuplicateGroup + '">'
                    + '            <span class="fa fa-files-o"></span>'
                    + '         </button>'
                    + '         <button class="btn-state-switch" data-toggle="tooltip" data-placement="right" title="' + lang.lOnOffGroup + '">'
                    + '            <span class="fa fa-check-square-o"></span>'
                    + '         </button>'
                    + '         <button class="btn-remove" data-toggle="tooltip" data-placement="right" title="' + lang.lRemoveGroup + '">'
                    + '            <span class="fa fa fa-trash-o"></span>'
                    + '         </button>'
                    + '      </td>'
                    + '      <td>'
                    + '         <table class="group-content"></table>'
                    + '      </td>'
                    + '   </tr>'
                    + '</tbody>');
            this.filtersGroupWrapper = $(this.parentTag).find('tbody').last().get(0);
            this.filtersGroupTag = $(this.parentTag).find('tr.filter-group').last().get(0);
            this.filtersGroupTableTag = $(this.filtersGroupTag).find('table').get(0);
            var btnStateSwitch = $(this.filtersGroupTag).find('button.btn-state-switch').get(0);
            $(btnStateSwitch).prop('disableState', false);
            $(btnStateSwitch).click(function (e) {
                return _this.filterPanel.disableGroup(_this, !$(btnStateSwitch).prop('disableState'));
            });
            $(this.filtersGroupTag).find('button.btn-remove').click(function (e) {
                return _this.filterPanel.removeGroup(_this);
            });
            $(this.filtersGroupTag).find('button.btn-duplicate').click(function (e) {
                return _this.filterPanel.duplicate(_this.getValue());
            });
            $(this.filtersGroupTag).find('[data-toggle="tooltip"]').tooltip();

            if (!this.filterPanel.filtersGroups.length) {
                $(this.filtersGroupTag).find('button.btn-remove')
                        .css("visibility", "hidden");
            } else {
                $(this.filterPanel.filtersGroups[0].filtersGroupTag).find('button.btn-remove')
                        .css("visibility", "visible");
            }
            this.addRow();
        };
        FiltersGroup.prototype.loadData = function (filterGroupData) {
            for (var i = 0; i < this.rows.length; i++)
                $(this.rows[i].rowTag).remove();
            this.rows = [];
            for (var i = 0; i < filterGroupData.filters.length; i++) {
                this.addRow(filterGroupData.filters[i]);
            }
            this.addRow();
            this.disable(!filterGroupData.on);
        };
        FiltersGroup.prototype.disable = function (disable) {
            if (this.disabled === disable)
                return;
            if (disable === true) {
                this.disabled = disable;
                $(this.filtersGroupWrapper).addClass('disabled-wrapper');
                this.$disabledElements = $(this.filtersGroupTableTag).find('input, select, span').not(':disabled').not(':hidden');
                $(this.$disabledElements).filter('span').hide();
                $(this.$disabledElements).attr('disabled', 'disabled');
                $(this.$disabledElements).addClass('disabled-tag');
                $(this.filtersGroupTag).find('button.btn-state-switch').prop('disableState', disable);
            } else if (disable === false && this.$disabledElements !== undefined) {
                this.disabled = disable;
                $(this.filtersGroupWrapper).removeClass('disabled-wrapper');
                $(this.$disabledElements).filter('span').show();
                $(this.$disabledElements).removeAttr('disabled');
                $(this.$disabledElements).removeClass('disabled-tag');
                this.$disabledElements = undefined;
                $(this.filtersGroupTag).find('button.btn-state-switch').prop('disableState', disable);
            }
        };
        FiltersGroup.prototype.addRow = function (filterData) {
            var row = new jFilterPanel.FRow(this, this.filtersGroupTableTag);
            if (this.rows.length > 0) {
                this.rows[this.rows.length - 1].remove.show(true);
            }
            if (filterData) {
                row.addedNewRow = true;
                row.loadData(filterData);
            }
            this.rows.push(row);
        };
        FiltersGroup.prototype.removeFilterRow = function (row) {
            var index = $.inArray(row, this.rows);
            $(this.rows[index].rowTag).remove();
            this.rows.splice(index, 1);
        };
        FiltersGroup.prototype.getValue = function () {
            var rowsValues = [];
            for (var i = 0; i < this.rows.length; i++)
                if (this.rows[i].filter.getValue() !== '---')
                    rowsValues.push(this.rows[i].getValue());
            return {
                on: !this.disabled,
                filters: rowsValues
            };
        };
        return FiltersGroup;
    }());
    jFilterPanel.FiltersGroup = FiltersGroup;

    //--------------------------------------------------------------------------
    var FRemove = (function () {
        function FRemove(row, parentTag) {
            this.row = row;
            this.parentTag = parentTag;
            this.build();
        }
        FRemove.prototype.build = function () {
            var _this = this;
            var lang = this.row.filtersGroup.filterPanel.lang;
            $(this.parentTag).append('<span class="glyphicon glyphicon-remove btn-remove" data-toggle="tooltip" data-placement="left" title="' + lang.lRemoveRecord + '"></span>');
            this.removeTag = $(this.parentTag).find('span').get(0);
            $(this.removeTag).click(function (e) {
                return _this.onRemoveClick(e);
            });
        };
        FRemove.prototype.onRemoveClick = function (event) {
            this.row.filtersGroup.removeFilterRow(this.row);
        };
        FRemove.prototype.show = function (show) {
            if (show)
                $(this.removeTag).show();
            else
                $(this.removeTag).hide();
        };
        return FRemove;
    }());
    jFilterPanel.FRemove = FRemove;

    //--------------------------------------------------------------------------
    var FRow = (function () {
        function FRow(filtersGroup, parentTag) {
            this.addedNewRow = false;
            this.filtersGroup = filtersGroup;
            this.parentTag = parentTag;
            this.dataProvider = this.filtersGroup.filterPanel.dataProvider;
            this.build();
        }
        FRow.prototype.build = function () {
            $(this.parentTag).append(''
                    + '<tr>'
                    + '   <td></td>'
                    + '   <td></td>'
                    + '   <td></td>'
                    + '   <td></td>'
                    + '</tr>');
            this.rowTag = $(this.parentTag).find('tr').last().get(0);
            var cells = $(this.rowTag).find('td');
            this.remove = new jFilterPanel.FRemove(this, $(cells).get(3));
            this.value = new jFilterPanel.FValue(this, $(cells).get(2));
            this.condition = new jFilterPanel.FCondition(this, $(cells).get(1));
            this.filter = new jFilterPanel.FFilter(this, $(cells).get(0));
            this.remove.show(false);
        };
        FRow.prototype.loadData = function (filterData) {
            var self = this;
            self.filter.setValue(filterData.key, function () {
                self.condition.setValue(filterData.cond, function () {
                    self.value.setValue(filterData.value, function () {
                    });
                });
            });
        };
        FRow.prototype.onFilterChange = function (value, callback) {
            var self = this;
            if (value === '---') {
                this.condition.disable(true);
            } else {
                this.dataProvider.getFilterData(value, function (data) {
                    self.condition.setConditions(data.conds);

                    if (self.value.fRealType)
                        delete self.value["fRealType"];
                    self.value.changeType(data.ftype, false, data.options, data.defValues);
                    self.condition.disable(false);
                    if (data.ftype === 'bool' || data.ftype === 'boolean' || data.ftype === 'enum')
                        $(self.value.valueTag).trigger('focus');
                    if (self.condition.cond1Tag.value === 'empty' || self.condition.cond1Tag.value === 'not_empty')
                        self.value.onValueFocus();
                    if (callback)
                        callback();
                });
            }
        };
        FRow.prototype.onConditionChange = function (value, callback) {
            if (value === '---') {
                this.value.disable(true);
            } else if (value.toLowerCase() === 'empty' || value.toLowerCase() === 'not_empty') {
                $(this.value.valueTag).trigger('focus');
                this.value.disable(true);
            } else {
                var cond = value.toLowerCase();
                if (cond.indexOf('younger_than') >= 0 || cond.indexOf('older_than') >= 0) {
                    if (this.value.fRealType === undefined)
                        this.value.fRealType = this.value.ftype;
                    this.value.changeType('uint');
                } else if (this.value.fRealType) {
                    this.value.changeType(this.value.fRealType);
                }
                this.value.disable(false);
                if (callback)
                    callback();
            }
        };
        FRow.prototype.onValueChange = function (value) {
        };
        FRow.prototype.getValue = function () {
            return {
                key: this.filter.getValue(),
                cond: this.condition.getValue(),
                value: this.value.getValue()
            };
        };
        return FRow;
    }());
    jFilterPanel.FRow = FRow;

    //--------------------------------------------------------------------------
    var FValue = (function () {
        function FValue(row, parentTag) {
            this.row = row;
            this.parentTag = parentTag;
            this.dataListContent = this.row.filtersGroup.filterPanel.dataListContent;
            this.build();
        }
        FValue.prototype.build = function () {
            var _this = this;
            $(this.parentTag).append('<input name="value" class="form-control">');
            this.valueTag = $(this.parentTag).find('input').get(0);
            this.ftype = 'text';
            $(this.valueTag).change(function (e) {
                return _this.onValueChange(e);
            });
            $(this.valueTag).on('input', function (e) {
                return _this.onValueInput(e);
            });
            $(this.valueTag).focus(function (e) {
                return _this.onValueFocus(e);
            });
            $(this.valueTag).change();
        };
        FValue.prototype.onValueChange = function (event, callback) {
            if (this.regex) {
                var match = this.matchRegex(this.valueTag.value);
                if (!match)
                    $(this.valueTag).addClass('error-tag');
                else
                    $(this.valueTag).removeClass('error-tag');
            }
        };
        FValue.prototype.onValueFocus = function (event) {
            if (this.row.addedNewRow !== true) {
                this.row.addedNewRow = true;
                this.row.filtersGroup.addRow();
            }
            $(this.valueTag).unbind('focus');
        };
        FValue.prototype.onValueInput = function (event) {
            $(this.valueTag).trigger('change');
        };
        FValue.prototype.matchRegex = function (value) {
            return this.regex.test(value);
        };
        FValue.prototype.changeType = function (newType, disabled, enumValues, defValues) {
            var _this = this;
            var selectTypes = ['enum', 'bool', 'boolean'];
            $(this.valueTag).removeClass('error-tag');
            if ($.inArray(this.ftype, selectTypes) >= 0) {
                if ($.inArray(newType, selectTypes) >= 0) {
                    $(this.valueTag).children().remove();
                    $(this.valueTag).val('');
                } else {
                    $(this.valueTag).remove();
                    $(this.parentTag).append('<input name="value" class="form-control">');
                    this.valueTag = $(this.parentTag).find('input').get(0);
                    $(this.valueTag).change(function (e) {
                        return _this.onValueChange(e);
                    });
                    $(this.valueTag).on('input', function (e) {
                        return _this.onValueInput(e);
                    });
                    $(this.valueTag).focus(function (e) {
                        return _this.onValueFocus(e);
                    });
                }
            } else {
                if ($.inArray(newType, selectTypes) >= 0) {
                    $(this.valueTag).remove();
                    $(this.parentTag).append('<select name="value" class="form-control"></select>');
                    this.valueTag = $(this.parentTag).find('select').get(0);
                    $(this.valueTag).change(function (e) {
                        return _this.onValueChange(e);
                    });
                    $(this.valueTag).focus(function (e) {
                        return _this.onValueFocus(e);
                    });
                } else {
                    $(this.valueTag).attr('placeholder', '');
                    $(this.valueTag).val('');
                    $(this.valueTag).attr('type', 'text');
                    $(this.valueTag).removeAttr('list');
                    $(this.valueTag).filterdatetimepicker('destroy');
                }
            }
            this.valueTag.onkeypress = function (e) {
                if (e.keyCode == 13) {
                    _this.row.filtersGroup.filterPanel.onFilter(e);
                    e.preventDefault();
                }
            };
            $(this.valueTag).attr('autocomplete', 'off');
            if (disabled === true)
                $(this.valueTag).addClass('disabled');
            var options = this.row.dataProvider.options;
            switch (newType) {
                case 'char':
                case 'character':
                    this.regex = /^.$/;
                    break;
                case 'text':
                case 'string':
                    this.regex = undefined;
                    break;
                case 'uint':
                    this.regex = /(^[0-9]+$)|^$/;
                    $(this.valueTag).attr('type', 'number');
                    break;
                case 'int':
                case 'integer':
                    this.regex = /(^-?[0-9]+$)|^$/;
                    $(this.valueTag).attr('type', 'number');
                    break;
                case 'float':
                case 'double':
                case 'number':
                    this.regex = /(^-?[0-9]+(\.[0-9]+$|$))|^$/;
                    $(this.valueTag).attr('type', 'number');
                    break;
                case 'time':
                    this.regex = /^\d{2}:\d{2}$|^$/;
                    $(this.valueTag).attr('placeholder', 'gg:mm');
                    $(this.valueTag).filterdatetimepicker({
                        format: options.date.formatTime,
                        timepicker: true,
                        datepicker: false,
                        dayOfWeekStart: 1,
                        closeOnDateSelect: true
                    });
                    break;
                case 'date':
                    this.regex = /(^\d{4}-\d{2}-\d{2}$)|^$/;
                    $(this.valueTag).attr('placeholder', 'rrrr-mm-dd');
                    $(this.valueTag).filterdatetimepicker({
                        format: options.date.formatDate,
                        timepicker: false,
                        datepicker: true,
                        dayOfWeekStart: 1,
                        closeOnDateSelect: true
                    });
                    $(this.valueTag).click(function () {
                        $.filterdatetimepicker.setLocale(options.locale);
                    });
                    break;
                case 'datetime':
                case 'timestamp':
                    this.regex = /^\d{4}-\d{2}-\d{2} \d{2}:\d{2}$|^$/;
                    $(this.valueTag).attr('placeholder', 'rrrr-mm-dd gg:mm');
                    $(this.valueTag).filterdatetimepicker({
                        format: options.date.format,
                        timepicker: true,
                        datepicker: true,
                        dayOfWeekStart: 1,
                        closeOnDateSelect: true
                    });
                    $(this.valueTag).click(function () {
                        $.filterdatetimepicker.setLocale(options.locale);
                    });
                    break;
                case 'bool':
                case 'boolean':
                    this.regex = /^true$|^false$|^$/;
                    BUtils.addOptions(this.valueTag, [
                        {key: 'true', name: options.lang.lTrue},
                        {key: 'false', name: options.lang.lFalse}
                    ]);
                    break;
                case 'enum':
                    BUtils.addOptions(this.valueTag, enumValues);
                    if (enumValues) {
                        var reg = '^$';
                        for (var i = 0; i < enumValues.length; i++)
                            reg += '|^' + enumValues[i].key + '$';
                        this.regex = new RegExp(reg);
                    } else {
                        this.regex = /^$/;
                    }
                    break;
            }
            this.ftype = newType;
            if ($.inArray(newType, ['text', 'string', 'int', 'integer', 'float', 'double', 'number']) >= 0 && this.row.filter) {
                var filterKey = this.row.filter.filterTag.value;
                if (filterKey && filterKey !== '---') {
                    var $dc = $(this.dataListContent).find('[id="dl_' + filterKey + '"]').first();
                    if ($dc.length === 0) {
                        $(this.dataListContent).append('<datalist id="dl_' + filterKey + '"></datalist>');
                        $dc = $(this.dataListContent).find('[id="dl_' + filterKey + '"]').first();
                    }
                    var datalistTag = $dc.get(0);
                    BUtils.addOptions(datalistTag, defValues);
                    $(this.valueTag).attr('list', 'dl_' + filterKey);
                }
            }
        };
        FValue.prototype.setValue = function (value, callback) {
            $(this.valueTag).val(value);
            $(this.valueTag).trigger('change', [callback]);
        };
        FValue.prototype.disable = function (disable) {
            if (disable === true) {
                if ($(this.valueTag).is('select'))
                    $(this.valueTag).find('option:selected').prop('selected', false);
                $(this.valueTag).val('');
                $(this.valueTag).attr('disabled', 'disabled');
                $(this.valueTag).addClass('disabled-tag');
            } else {
                $(this.valueTag).removeAttr('disabled');
                $(this.valueTag).removeClass('disabled-tag');
                if ($(this.valueTag).is('select'))
                    $(this.valueTag).find('option:first').prop('selected', true);
            }
        };
        FValue.prototype.getValue = function () {
            return this.valueTag.value;
        };
        return FValue;
    }());
    jFilterPanel.FValue = FValue;

    //--------------------------------------------------------------------------
    var ProfileManager = (function () {
        function ProfileManager(filterPanel, parentTag, options, storagePrefix) {
            this.parentTag = parentTag;
            this.storagePrefix = storagePrefix;
            this.options = options;
            this.filterPanel = filterPanel;
            this.lang = this.options.lang;
            this.build();
            this.loadProfilesListFromStorage();
            $(this.selectTag).change();
        }
        ProfileManager.prototype.build = function () {
            var _this = this;
            $(this.parentTag).append(''
                    + '<table class="profile">'
                    + '   <tr>'
                    + '      <td>'
                    + '         <label>' + this.lang.lProfile + ':</label>'
                    + '      </td>'
                    + '      <td>'
                    + '         <select class="form-control">'
                    + '            <option value="---">---</option>'
                    + '         </select>'
                    + '      </td>'
                    + '      <td>'
                    + '         <button class="save" title="' + this.lang.lSave + '">'
                    + '            <span class="save fa fa-lg fa-floppy-o"></span>'
                    + '         </button>'
                    + '      </td>'
                    + '      <td>'
                    + '         <button class="add" title="' + this.lang.lAdd + '">'
                    + '            <span class="add fa fa-lg fa-file-o"></span>'
                    + '         </button>'
                    + '      </td>'
                    + '      <td>'
                    + '         <button class="remove" title="' + this.lang.lRemove + '">'
                    + '            <span class="remove fa fa-lg fa-trash-o"></span>'
                    + '         </button>'
                    + '      </td>'
                    + '   </tr>'
                    + '</table>');
            this.profileTag = $(this.parentTag).find('.profile').get(0);
            this.selectTag = $(this.parentTag).find('select').get(0);
            this.saveTag = $(this.profileTag).find('.save').get(0);
            this.addTag = $(this.profileTag).find('.add').get(0);
            this.removeTag = $(this.profileTag).find('.remove').get(0);
            $(this.saveTag).click(function (e) {
                return _this.saveProfile(e);
            });
            $(this.addTag).click(function (e) {
                return _this.addProfile(e);
            });
            $(this.removeTag).click(function (e) {
                return _this.removeProfile(e);
            });
            $(this.selectTag).change(function (e) {
                return _this.onProfileChange(e);
            });
        };
        ProfileManager.prototype.onProfileChange = function (event) {
            var value = this.selectTag.value;
            this.storageData.last = value;
            if (value === '---') {
                $(this.saveTag).prop('disabled', true).addClass('disabled-tag');
                $(this.removeTag).prop('disabled', true).addClass('disabled-tag');
            } else {
                $(this.saveTag).prop('disabled', false).removeClass('disabled-tag');
                $(this.removeTag).prop('disabled', false).removeClass('disabled-tag');
            }
            this.loadProfile(value);
        };
        ProfileManager.prototype.loadProfilesListFromStorage = function () {
            var strStorageData = localStorage.getItem(this.storagePrefix + '_list');
            if (strStorageData) {
                this.storageData = JSON.parse(strStorageData);
                var names = this.storageData.names;
                if (names && $.isArray(names) && names.length > 0)
                    BUtils.addOptions(this.selectTag, names, true);
            } else {
                this.storageData = {last: '---', names: []};
            }
        };
        ProfileManager.prototype.saveProfilesListToStorage = function () {
            var strStorageData = JSON.stringify(this.storageData);
            localStorage.setItem(this.storagePrefix + '_list', strStorageData);
        };
        ProfileManager.prototype.saveProfile = function (event) {
            var profileName = $(this.selectTag).val();
            localStorage.setItem(this.storagePrefix + '_elem_' + profileName, JSON.stringify(this.filterPanel.getValue(true)));
        };
        ProfileManager.prototype.loadProfile = function (profileName) {
            if ($.inArray(profileName, this.storageData.names) < 0)
                return;
            var strProfileData = localStorage.getItem(this.storagePrefix + '_elem_' + profileName);
            if (!strProfileData)
                return;
            var profileData = JSON.parse(strProfileData);
            try {
                this.filterPanel.loadData(profileData);
            } catch (err) {
                this.removeProfile(undefined);
                location.reload();
            }
        };
        ProfileManager.prototype.addProfile = function (event) {
            var profileName = prompt(this.lang.lQuestions.setNewProfileName + ':');
            if (profileName === null)
                return;
            if ($.inArray(profileName, this.storageData.names) >= 0) {
                BUtils.showAlert(null, this.lang.lErrors.profileExists);
                return;
            }
            $(this.selectTag).append('<option value="' + profileName + '">' + profileName + '</option>');
            $(this.selectTag).find('option:last').prop('selected', true);
            $(this.selectTag).change();
            this.storageData.names.push(profileName);
            this.saveProfilesListToStorage();
            this.saveProfile(undefined);
        };
        ProfileManager.prototype.removeProfile = function (event) {
            var profileName = $(this.selectTag).val(), index = $.inArray(profileName, this.storageData.names);
            this.storageData.names.splice(index, 1);
            this.saveProfilesListToStorage();
            localStorage.removeItem(this.storagePrefix + '_elem_' + profileName);
            $(this.selectTag).find('option:selected').remove();
            $(this.selectTag).find('option:first').prop('selected', true);
            $(this.selectTag).change();
        };
        return ProfileManager;
    }());
    jFilterPanel.ProfileManager = ProfileManager;
})(jFilterPanel || (jFilterPanel = {}));
//# sourceMappingURL=profilemanager.js.map
var jFilterPanel;
(function (jFilterPanel) {
    var FilterPanelOptions = (function () {
        function FilterPanelOptions() {
            this.locale = 'pl';
            this.lang = jFilterPanel.i18n[this.locale];
            this.autocomplete = false;
            this.filterAfterLoad = true;
            this.date = {
                format: 'Y-m-d H:i',
                formatTime: 'H:i',
                formatDate: 'Y-m-d'
            };
            this.filters = [];
            this.activeFilters = [];
        }
        return FilterPanelOptions;
    }());
    jFilterPanel.FilterPanelOptions = FilterPanelOptions;
    jFilterPanel.i18n = {
        pl: {
            lConditions: {
                equal: 'Równe',
                not_equal: 'Różne',
                start_with: 'Rozpoczyna się od',
                ends_with: 'Kończy się na',
                contains: 'Zawiera',
                not_contain: 'Nie zawiera',
                similar: 'Podobne',
                similar_20: '20%',
                similar_50: '50%',
                similar_80: '80%',
                empty: 'Jest puste',
                not_empty: 'Nie jest puste',
                less: 'Mniejsze',
                less_or_equal: 'lub równe',
                greater: 'Większe',
                greater_or_equal: 'lub równe',
                older_than: 'Starsze niż',
                older_than_minutes: 'minuty',
                older_than_hours: 'godziny',
                older_than_days: 'dni',
                older_than_months: 'miesiące',
                older_than_years: 'lata',
                younger_than: 'Młodsze niż',
                younger_than_minutes: 'minuty',
                younger_than_hours: 'godziny',
                younger_than_days: 'dni',
                younger_than_months: 'miesiące',
                younger_than_years: 'lata'
            },
            lFilterPanel: 'Filtry',
            lFilter: 'Filtruj',
            lSearch: 'Szukaj:',
            lClear: 'Wyczyść',
            lDuplicateGroup: 'Powiel grupę',
            lOnOffGroup: 'Wł / Wył grupę',
            lRemoveGroup: 'Usuń grupę',
            lRemoveRecord: 'Usuń wiersz',
            lRemove: 'Usuń',
            lProfile: 'Profil',
            lAdd: 'Dodaj',
            lSave: 'Zapisz',
            lTrue: 'Prawda',
            lFalse: 'Fałsz',
            lRefresh: 'Odśwież',
            lQuestions: {
                setNewProfileName: 'Podaj nazwę nowego profilu'
            },
            lErrors: {
                cantDisableLastGroup: 'Nie można zablokować ostatniej grupy filtrów!',
                cantDeleteLastGroup: 'Nie można usunąć ostatniej grupy filtrów!',
                profileExists: 'Profil już istnieje'
            }
        },
        en: {
            lConditions: {
                equal: 'Equal',
                not_equal: 'Not equal',
                start_with: 'Start with',
                ends_with: 'Ends with',
                contains: 'Contains',
                not_contain: 'Not contain',
                similar: 'Similar',
                similar_20: '20%',
                similar_50: '50%',
                similar_80: '80%',
                empty: 'Empty',
                not_empty: 'Not empty',
                less: 'Less',
                less_or_equal: 'or equal',
                greater: 'Greater',
                greater_or_equal: 'or equal',
                older_than: 'Older than',
                older_than_minutes: 'minutes',
                older_than_hours: 'hours',
                older_than_days: 'days',
                older_than_months: 'months',
                older_than_years: 'years',
                younger_than: 'Younger than',
                younger_than_minutes: 'minutes',
                younger_than_hours: 'hours',
                younger_than_days: 'days',
                younger_than_months: 'months',
                younger_than_years: 'years'
            },
            lFilterPanel: 'Filters',
            lFilter: 'Filter',
            lSearch: 'Search:',
            lClear: 'Clear',
            lDuplicateGroup: 'Duplicate group',
            lOnOffGroup: 'On / Off group',
            lRemoveGroup: 'Remove group',
            lRemoveRecord: 'Remove record',
            lRemove: 'Remove',
            lProfile: 'Profile',
            lAdd: 'Add',
            lSave: 'Save',
            lTrue: 'True',
            lFalse: 'False',
            lRefresh: 'Refresh',
            lQuestions: {
                setNewProfileName: 'Set new profile name'
            },
            lErrors: {
                cantDisableLastGroup: 'Can\'t disable last filter group!',
                cantDeleteLastGroup: 'Can\'t remove last filter group!',
                profileExists: 'Profile already exists'
            }
        }
    };
    jFilterPanel.conditions = [{key: 'equal', name: 'Równe'},
        {key: 'not_equal', name: 'Różne'},
        {key: 'start_with', name: 'Rozpoczyna się od'},
        {key: 'ends_with', name: 'Kończy się na'},
        {key: 'contains', name: 'Zawiera'},
        {key: 'not_contain', name: 'Nie zawiera'},
        {key: 'empty', name: 'Jest puste'},
        {key: 'not_empty', name: 'Nie jest puste'},
        {
            key: 'similar', name: 'Podobne', values: [
                {key: 'similar_20', name: '20%'},
                {key: 'similar_50', name: '50%'},
                {key: 'similar_80', name: '80%'}
            ]
        },
        {
            key: 'greater', name: 'Większe', values: [
                {key: 'greater', name: ''},
                {key: 'greater_or_equal', name: 'lub równe'}
            ]
        },
        {
            key: 'less', name: 'Mniejsze', values: [
                {key: 'less', name: ''},
                {key: 'less_or_equal', name: 'lub równe'}
            ]
        },
        {
            key: 'older_than', name: 'Starsze niż', values: [
                {key: 'older_than_minutes', name: 'minuty'},
                {key: 'older_than_hours', name: 'godziny'},
                {key: 'older_than_days', name: 'dni'},
                {key: 'older_than_months', name: 'miesiące'},
                {key: 'older_than_years', name: 'lata'}
            ]
        },
        {
            key: 'younger_than', name: 'Młodsze niż', values: [
                {key: 'younger_than_minutes', name: 'minuty'},
                {key: 'younger_than_hours', name: 'godziny'},
                {key: 'younger_than_days', name: 'dni'},
                {key: 'younger_than_months', name: 'miesiące'},
                {key: 'younger_than_years', name: 'lata'}
            ]
        },
    ];
})(jFilterPanel || (jFilterPanel = {}));
(function ($) {
    if (!$)
        return false;
    $.fn.extend({
        jFilterPanel: function (opts, value) {
            return this.each(function () {
                var $this = $(this), data = $this.data('jQueryPanel');
                if (!data) {
                    var defaults = new jFilterPanel.FilterPanelOptions(), o = $.extend({}, defaults, opts);
                    $this.data('jQueryPanel', data = new jFilterPanel.FilterPanel($this, o));
                }
                if (typeof opts === 'string') {
                    if (opts.indexOf('options.') == 0) {
                        data.options[opts.substring(opts.indexOf('.') + 1, opts.length)] = value;
                    } else {
                        data[opts](value);
                    }
                }
            });
        }
    });
})(jQuery);
//# sourceMappingURL=plugin.js.map
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
    BUtils.showAlert = function (title, text, onDone) {
        if (!title)
            title = 'Info';
        if (!window.$alert) {
            alert(text);
            onDone();
        } else
            window.$alert(title, text, onDone);
    };
    return BUtils;
}());
//# sourceMappingURL=utils.js.map