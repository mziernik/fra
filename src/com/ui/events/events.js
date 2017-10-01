//# sourceURL=file:///events/events.js

/**
 * @param {SPAController} ctrl
 * @param {FrameworkApi} api
 * @returns {void}
 */
registerController(function (ctrl, api) {
    "use strict";

    ctrl.id = "events";
    ctrl.name = "Zdarzenia";
    ctrl.urls = ["events"];
    ctrl.htmlTemplate = "events/events.html";
    ctrl.clearOnUnload = false;

    this.api = api;

    var dsTable, filterPanel;


    ctrl.onNewInstance = (e) => {

        dsTable = new DsTable(opt => {
            opt.id = ctrl.id;
            opt.tag = $id("tblEvents");
            opt.webApiMethod = this.api.events.result;
        });

        dsTable.onClick = function (event, cell) {
            window.location.hash = 'event/' + cell.row.primaryCell.value;
        };

        filterPanel = new FilterPanel(this.api, dsTable, $id('fltrEvents'));

    };
});

class FilterPanel {
    constructor(api, tbl, tag) {
        this.api = api;
        this.tbl = tbl;
        this.tag = tag;
        this.firstFilter = true;

        $(tag).jFilterPanel({
            onFilter: (data) => {
                this.filter(data, tbl.displayLength);
            },
            onClear: function () {
            },
            onGetFilterData: function (filterKey, func) {
            },
            onGetMultiFilterData: function (filterKeys, callback) {
                callback([]);
            }
        });

        this.getInitData();
    }

    getInitData() {
        this.api.events.getInitData({
            onSuccess: (response) => {
                $(this.tag).jFilterPanel('options.onGetFilterData', function (filterKey, callback) {
                    callback(response.data.filters[filterKey]);
                });
                $(this.tag).jFilterPanel('options.onGetMultiFilterData', function (filterKeys, callback) {
                    callback(response.data.filters);
                });

                $(this.tag).jFilterPanel('setFiltersList', response.data.filters);
                $(this.tag).jFilterPanel('loadData', response.data.activeFilters);
            }
        });
    }

    filter(data, entries) {
        let sortData = this.getSortableData();

        this.api.events.filter({
            data: {
                filters: data,
                parameters: {
                    entries: entries,
                    order: sortData.order,
                    orderColumn: sortData.orderColumn
                }
            },
            onSuccess: (response) => {
                this.tbl.load(this.firstFilter ? response.data : response.data.rows);
                this.firstFilter = false;
            }
        });
    }

//    changePage(page, entries) {
//        let sortData = this.getSortableData();
//
//        this.api.events.filter({
//            data: {
//                filters: [],
//                parameters: {
//                    page: page,
//                    entries: entries,
//                    order: sortData.order,
//                    orderColumn: sortData.orderColumn
//                }
//            },
//            onSuccess: (response) => {
//                this.tbl.load(response.data);
//            }
//        });
//    }

    getSortableData() {
        let order, orderColumn;

        if (this.tbl.sortCol) {
            order = this.tbl.sortCol.name;
            orderColumn = this.tbl.sortCol.sortOrder > 0 ? 'DESC' : 'ASC';
        }

        return {
            order: order,
            orderColumn: orderColumn
        };
    }
}
