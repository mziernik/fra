/*jslint white: true, plusplus: true, browser: true*/
/*global jQuery, $id, document, parseFloat, registerSession*/
var confstatus = (function ($) {
    'use strict';

    $(document).ready(function () {
        setTimeout(function () {
            registerSession();
            window.view.autoReconnect = true;
        }, 0);
    });

    function unitFormat(sizeInBytes) {
        if (sizeInBytes > 0x40000000) {
            return (sizeInBytes / 0x40000000).round(1, true) + " GB";
        }
        if (sizeInBytes > 0x100000) {
            return (sizeInBytes / 0x100000).round(1, true) + " MB";
        }
        if (sizeInBytes > 0x400) {
            return (sizeInBytes / 0x400).round(1, true) + " KB";
        }

        return sizeInBytes + " B";
    }


    function printUsageBar(barId, used, commited, total) {
        var pctUsed, pctCommited, usedColorCls, pDiv;

        // Procenty
        pctUsed = ((parseFloat(used) / total) * 100).toFixed(1);
        if (commited) {
            pctCommited = (((parseFloat(commited) / total) * 100)).toFixed(1);
        }

        // Kolory
        if (pctUsed > 90) {
            usedColorCls = 'progress-bar-danger';
        } else if (pctUsed > 75) {
            usedColorCls = 'progress-bar-warning';
        } else {
            usedColorCls = 'progress-bar-success';
        }

        // Tworzenie paska postępu
        $id(barId).innerHTML = '';
        pDiv = $id(barId).tag("div")
                .cls('progress');
        pDiv.style.minWidth = '350px';
        pDiv.style.marginBottom = '0';

        // Used
        pDiv.tag("div").setText(isNaN(pctUsed) ? '0.0%' : pctUsed + '%')
                .cls('progress-bar ' + usedColorCls) //'progress-bar-striped'
                .attr({
                    role: 'progressbar',
                    style: 'width:' + pctUsed + '%'
                });

        // Commited
        if (commited && pctCommited - pctUsed > 0) {
            pDiv.tag("div").setText(pctCommited + '%')
                    .cls('progress-bar progress-bar-info')
                    .attr({
                        role: 'progressbar',
                        style: 'width:' + (pctCommited - pctUsed) + '%'
                    });
        }
    }

    function setStat(arrData) {
        var id, barId, pct, used, commited, total, result;

        //  4 - ["id", "pct", "usd", "max"]
        //  5 - ["id", "pct", "usd", "max", "cmt"]
        if (arrData.length === 4 || arrData.length === 5) {

            if (arrData.length === 5) {
                commited = arrData[4]; // Zaalokowane zasaoby
            }
            total = arrData[3]; // Maksymalny stan zasobów
            used = arrData[2];  // Zużyte zasoby
            pct = arrData[1];   // Czy wynik jest w procentach (boolean)
            id = arrData[0];    // Id słownych wartości zużycia
            barId = id + 'Bar'; // Id paska zużycia

            // Zużycie (słownie)
            if (pct === true) {
                result = used + '%';
            } else {
                result = unitFormat(used) + ' / ';

                if (commited) {
                    result += unitFormat(commited) + ' / ';
                }

                result += unitFormat(total);
            }
            $id(id).setText(result);

            // Pasek zużycia
            printUsageBar(barId, used, commited, total);

            return;
        }

        // 2 - ["id", "val"]
        if (arrData.length === 2) {
            result = arrData[1];  // Wartość
            id = arrData[0];   // Id słownych wartości zużycia

            $id(id).setText(result);

            return;
        }
    }

    return {
        updateData: function (arrData) {
            arrData.forEach(function (r) {
                setStat(r);
            });
        }
    };
}(jQuery));