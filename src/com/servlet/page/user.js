/*jslint white: true, plusplus: true, browser: true*/
/*global jQuery, $id, ajax, document, localStorage, filterData, filterStruct, addEmptyRow*/

(function ($) {
    'use strict';

    //-------------------------------------------------------
    //-   WYŚWIETLANIE BŁĘDÓW AJAXA (SweetAlert i Utils)    -
    //-------------------------------------------------------


    if (window.jQuery) {
        $(document).ajaxSend(function (event, jqXHR, ajaxOptions) {
            jqXHR.setRequestHeader('X-Requested-With', 'XMLHttpRequest');
        });

        $(document).ajaxError(function (event, jqxhr, settings, thrownError) {
            var eerror = new EError(jqxhr);
            var err;
            if (jqxhr.responseText)
                err = JSON.parse(jqxhr.responseText);
            if (err && err.title && err.message && err.details !== undefined) {
                eerror.title = err.title;
                eerror.message = err.message;
            }
            window.$error(eerror);
            return true;
        });
    }


    //-------------------------------------------------------
    //-               OBŁSUGA FORMULARZA                    -
    //-------------------------------------------------------


    $(function () {
        $('form').on('submit', function (e) {
            e.preventDefault();

            var $notChecked = $('input.rightsCbx').not(':checked').filter(function (index, element) {
                return $(element).prop('indeterminate') !== true;
            });

            $notChecked.prop('checked', true);

            $.ajax({
                url: $(this).attr('action') || window.location.pathname,
                type: "POST",
                data: $(this).serialize(),
                success: function (data) {
                    var username = location.search.replace(/^.*?\=/, '');

                    $success(data, undefined, function () {
                        if (!username)
                            location.search = '?username=' + $('input[name="username"]').val();
                    });
                }
            });

            $notChecked.prop('checked', false);
        });

        // CHECKBOXY - Grupy (groups) -------------------------------------


        // CHECKBOXY - Uprawnienia (rights) -------------------------------
        var $rightsCbx = $('input.rightsCbx');
        $rightsCbx.filter('[emptyVal="true"]').prop('indeterminate', true);
        $rightsCbx.checkboxX({
            iconChecked: '<i class="glyphicon glyphicon-ok"></i>',
            iconUnchecked: '<i class="glyphicon glyphicon-remove"></i>',
            iconNull: ' ',
            valueChecked: 't',
            valueUnchecked: 'f',
            valueNull: 'n',
            size: 'xs'
        });
        $rightsCbx.on('stateChange', function () {
            // wywoływane po zmianie stanu checkboxa
        });

        // PANELS --------------------------------------------------
        $('div.panel-heading').click(function (event, fast) {
            if ($(this).prop("showed") === true) {
                $(this).prop("showed", false);
                localStorage.setItem(this.id, false);
            } else {
                $(this).prop("showed", true);
                localStorage.setItem(this.id, true);
            }

            if (fast)
                $(this).siblings('.panel-body').toggle();
            else
                $(this).siblings('.panel-body').slideToggle(300);
        });

        $('div.panel-heading').on('load', function () {
            var headerId = localStorage.getItem(this.id);

            if (headerId && JSON.parse(headerId) === true) {
                $(this).trigger('click', [true]);
            }
        });

        $('div.panel-heading').trigger('load');
    });

    //-------------------------------------------------------
    //-          OBŁSUGA ROZWIJANEGO MENU (ROLE)            -
    //-------------------------------------------------------

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

    function hasExpandedRowsInStorage() {
        var expRows = localStorage.getItem('rights.expanded_rows');

        if (expRows !== undefined && expRows !== null && expRows !== '' && expRows !== '[]' && expRows !== '[null]')
            return true;
        return false;
    }

    function loadExpandedRows() {
        var arr = JSON.parse(localStorage.getItem('rights.expanded_rows'));
        if (!arr || !(arr instanceof Array)) {
            return;
        }

        arr.forEach(function (element) {
            $('a[item-name="' + element + '"]').click();
        });
    }

    function saveExpandedRows() {
        if (saveRowChanges === false) {
            return;
        }

        var arr = [];

        $('.config-group').each(function () {
            if ($(this).children('span').hasClass('glyphicon-menu-down')) {
                arr.push($(this).attr("item-name"));
            }
        });

        localStorage.setItem('rights.expanded_rows', JSON.stringify(arr));
    }

    function colapseAllGroups() {
        $('.config-group').each(function () {
            if ($(this).children('span').hasClass('glyphicon-menu-down')) {
                $(this).click();
            }
        });
    }

    function expandAllGroups() {
        $('.config-group').each(function () {
            if ($(this).children('span').hasClass('glyphicon-menu-right')) {
                $(this).click();
            }
        });
    }

    function removeArrayElement(array, value) {
        var index = $.inArray(value, array);

        if (index >= 0) {
            array.splice(index, 1);
            return true;
        }
        return false;
    }

    function getParentsGroups(grpId, idArr) {
        var elem = filterStruct[grpId];
        if (elem && elem.parentId) {
            if ($.inArray(elem.parentId, idArr) === -1) {
                idArr.push(elem.parentId);
            }
            getParentsGroups(elem.parentId, idArr);
        }
    }

    function getChildrenGroupsAndItems(grpId, grpArr, itemArr) {
        var elem = filterStruct[grpId];
        if (elem && elem.items) {
            elem.items.forEach(function (val) {
                if ($.inArray(val, itemArr) === -1) {
                    itemArr.push(val);
                }
            });
        }
        if (elem && elem.groups) {
            elem.groups.forEach(function (val) {
                if ($.inArray(val, grpArr) === -1) {
                    grpArr.push(val);
                    getChildrenGroupsAndItems(val, grpArr, itemArr);
                }
            });
        }
    }

    function setFastOptionToggle() {
        // Zdarzenie rozwinięcia menu (szybkie) - tylko podczas wczytywania strony
        $('.config-group').unbind('click'); // usunięcie starego zdarzenia 'click'
        $('.config-group').click(function () {
            $(this).next('ul').toggle();
            updateExpandIcon($(this).children("span"));
        });
    }

    function setSlowOptionToggle() {
        // Zdarzenie rozwinięcia menu (powolne - animowane)
        $('.config-group').unbind('click'); // usunięcie starego zdarzenia 'click'
        $('.config-group').click(function (event) {
            $(this).next('ul').slideToggle(200);
            event.stopPropagation();
            updateExpandIcon($(this).children('span'));
            saveExpandedRows(); // zapisanie rozwiniętych opcji
        });
    }

    function convertToIdArray(array) {
        var idArray = [];

        array.forEach(function (element) {
            idArray.push('#' + element.split('.').join('\\.'));
        });

        return idArray;
    }

    $(document).ready(function () {
        setFastOptionToggle();

        if (hasExpandedRowsInStorage())
            loadExpandedRows(); // Rozwinięcie wierszy na podstawie listy z localstorage
        else
            expandAllGroups(); // Rozwinięcie wszystkich wierszy

        setSlowOptionToggle();

        // Filtrowanie danych
        $('#filterInput').keyup(function () {
            var value, grpToExpand, grpToFullExpand, grpToPartialExpand, itmToShow, grpIds, itmIds;

            value = $(this).val().toLowerCase();  // Szukana wartość
            grpToFullExpand = [];     // Grupy do całkowitego rozwinięcia (wyświetlane są wszystkie jego dzieci)
            grpToPartialExpand = [];  // Grupy do częściowego rozwinięcia (nie wszystkie jego dzieci są wyświetlane)
            grpToExpand = [];         // Wszystkie grupy do rozwinięcia (częściowe i całkowite)
            itmToShow = [];           // Elementy do wyświetlenia
            grpIds = [];              // Lista pomocnicza zawierająca grupy z dodatkowym znakiem '#' oznaczającym id
            itmIds = [];              // Lista pomocnicza zawierająca elementy z dodatkowym znakiem '#' oznaczającym id

            setFastOptionToggle();
            localStorage.setItem('rights.search_value', JSON.stringify(value));

            // Usunięcie koloru z wszystkich poprzednich rezultatów
            $('.searched-element').each(function () {
                $(this).removeClass('searched-element');
            });

            // Usunięcie filtru w przypadku pustego tekstu
            if (value.replace(' ', '').length === 0) {
                // Wyświetlenie wszystkich schowanych elementów
                $('.fAccordion').find('li:hidden, a:hidden').show();

                colapseAllGroups();
                saveExpandedRows();
                setSlowOptionToggle();
                return;
            }

            saveRowChanges = false;

            // Szukanie elementów
            filterData.forEach(function (elem) {
                if (elem.value.contains(value)) {
                    if (elem.type === 'GROUP') {
                        removeArrayElement(grpToPartialExpand, elem.id); // Usunięcie z grup do cześciowego rozwinięcia
                        grpToFullExpand.push(elem.id); // Dodanie do grup do całkowitego rozwinięcia
                    } else if (elem.type === 'ITEM') {
                        if ($.inArray(elem.parentId, grpToFullExpand) === -1) { // Jeśli element nie znajduje się w grupie do całkowitego rozwinięcia
                            if ($.inArray(elem.parentId, grpToPartialExpand) === -1) { // Jeśli element nie znajduje się w grupie do częściowego rozwinięcia
                                grpToPartialExpand.push(elem.parentId);
                            }
                            if ($.inArray(elem.id, itmToShow) === -1) {
                                itmToShow.push(elem.id);
                            }
                        }
                    }
                }
            });

            // KOLOROWANIE REZULTATÓW ------------------------------------------
            itmIds = convertToIdArray(itmToShow);
            grpIds = convertToIdArray(grpToFullExpand);

            $('.config-item').closest(itmIds.toString()).each(function () {
                $(this).addClass('searched-element');
            });
            $('.config-group').closest(grpIds.toString()).each(function () {
                $(this).addClass('searched-element');
            });

            // ROZWIJANIE/ZWIJANIE REZULTATÓW ----------------------------------
            grpToFullExpand.concat(grpToPartialExpand).forEach(function (grpId) { // pobranie pozostałych grup do rozwinięcia (wymagane do rozwinięcia szukanych)
                getParentsGroups(grpId, grpToPartialExpand);
            });
            grpToExpand = grpToFullExpand.concat(grpToPartialExpand);

            //----------
            itmIds = convertToIdArray(itmToShow);
            grpIds = convertToIdArray(grpToExpand);

            grpIds.forEach(function (grpId) { // Rozwinięcie znalezionych grup
                var group = $(grpId);
                if (group.children('span').hasClass('glyphicon-menu-right')) {
                    group.click();
                }
            });

            $('.config-group').not(grpIds.toString()).each(function () { // Zwinięcie pozostałych grup
                if ($(this).children('span').hasClass('glyphicon-menu-down')) {
                    $(this).click();
                }
            });

            // CHOWANIE/WYŚWIETLANIE ELEMENTÓW ---------------------------------
            grpToFullExpand.forEach(function (elem) { // Pobranie grup i elementów dla grup pełnych do rozwinięcia
                getChildrenGroupsAndItems(elem, grpToFullExpand, itmToShow);
            });

            grpToExpand = grpToFullExpand.concat(grpToPartialExpand); // Ponowne utworzenie list z identyfikatorami grup i elementów (do schowania)
            itmIds = convertToIdArray(itmToShow);
            grpIds = convertToIdArray(grpToExpand);

            // Schowanie niepotrzebnych elementów
            $('.config-item').not(itmIds.toString()).each(function () {
                $(this).parent().hide();
            });
            $('.config-group').not(grpIds.toString()).each(function () {
                $(this).parent().hide();
            });

            // Schowanie niepotrzebnych elementów
            $('.config-item').closest(itmIds.toString()).each(function () {
                $(this).parent().show();
            });
            $('.config-group').closest(grpIds.toString()).each(function () {
                $(this).parent().show();
            });
            //------------------------------------------------------------------

            setSlowOptionToggle();
            saveRowChanges = true;
        });

        // Usunięcie filtrowania
        $('#filterClear').click(function () {
            var filterInput = $('#filterInput');

            filterInput.val('');
            filterInput.keyup();
        });

        // Wczytanie poprzedniego rezultatu wyszukiwania
        var searchValue, filterInput;
        searchValue = JSON.parse(localStorage.getItem('rights.search_value'));
        if (searchValue && searchValue.replace(' ', '').length !== 0) {
            filterInput = $('#filterInput');

            filterInput.val(searchValue);
            filterInput.keyup();
        }
    });
}(jQuery));


function cbDefaultClick() {
    'use strict';

    var frm, cb;

    frm = $id('frmEdit');
    cb = $id('cbDef');

    if (!frm || !cb) {
        return;
    }

    if (cb.checked) {
        frm.setAttribute('defaults', '');
    } else {
        frm.removeAttribute('defaults');
    }
}

function cfgFieldChange() {
    'use strict';

    if ($id('cbDef')) {
        $id('cbDef').checked = false;
    }

    cbDefaultClick();
}

function addRow(endpointId, tblId, id) {
    'use strict';
    var tbl, div, tr;

    tbl = $id(tblId);
    if (!tbl) {
        return;
    }

    ajax.post(endpointId + '?addRow', {id: id}, function (http) {
        cfgFieldChange();
        div = document.createElement('div');
        div.innerHTML = http.responseText;
        tr = div.children[0].tBodies[0].children[0];
        if (tbl.tBodies.length === 0) {
            tbl.tag('tbody');
        }
        tbl.tBodies[0].appendChild(tr);
    });
}


