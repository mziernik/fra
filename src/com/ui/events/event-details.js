//# sourceURL=file:///events/event-details.js

/**
 * @param {SPAController} ctrl
 * @param {FrameworkApi} api
 * @returns {void}
 */
registerController(function (ctrl, api) {
    "use strict";

    ctrl.id = "eventDetails";
    ctrl.name = "Szczegóły zdarzenia";
    ctrl.urls = ["event", "event/\\d+"];
    ctrl.htmlTemplate = "events/event-details.html";
    ctrl.clearOnUnload = true;

    this.api = api;

    ctrl.onNewInstance = (e) => {

        var detailsTag = $id('evntDetails');

        this.api.events.getDetails({
            params: {
                id: parseInt(e.path[1], 10)
            },
            onSuccess: function (response) {
                if (!response.data.event) {
                    detailsTag.txt('Brak danych');
                    return;
                }

                // Szczegóły zdarzenia -------------------------
                let evntData = response.data.event;
                let evntTbl = buildPanel(detailsTag, 'Szczegóły zdarzenia').tag('table');

                addRow(evntTbl, 'Id:', evntData['id']);
                addRow(evntTbl, 'Data:', evntData['date']);
                addRow(evntTbl, 'Typ:', evntData['type'].toUpperCase());
                addRow(evntTbl, 'Tagi:', evntData['tags']);
                addRow(evntTbl, 'Źródło:', evntData['source']);
                addRow(evntTbl, 'Zdarzenie:', evntData['event']);
                addRow(evntTbl, 'Użytkownik:', evntData['username']);
                addRow(evntTbl, 'Adres:', evntData['address']);
                addRow(evntTbl, 'URL:', evntData['url']);

                // Atrybuty -------------------------------------
                let attribData = response.data.attributes;
                if (attribData && attribData.length > 1) {
                    let attribTbl = buildPanel(detailsTag, 'Atrybuty').tag('table');

                    // ["*id", "tags", "name", "value"]
                    for (let i = 1; i < attribData.length; i++)
                        addRow(attribTbl, attribData[i][2] + ':', attribData[i][3]);
                }

                // Powiązania (klucze obce) ---------------------
                let fkData = response.data.foreignKeys;
                if (fkData && fkData.length > 1) {
                    let fkTbl = buildPanel(detailsTag, 'Powiązania').tag('table');

                    // ["*columnName", "keys"]
                    for (let i = 1; i < fkData.length; i++)
                        addRow(fkTbl, fkData[i][0] + ':', fkData[i][1]);
                }

                // Szczegóły ------------------------------------
                let detailsData = response.data.details;
                if (detailsData && detailsData.length > 1) {
                    let detailsPnl = buildPanel(detailsTag, 'Szczegóły');

                    // ["*id", "tags", "name", "value", "contentType"]
                    for (let i = 1; i < detailsData.length; i++) {
                        //             [parentTag,  editorId,     title,             text,              contentType]
                        buildTextEditor(detailsPnl, "editor" + i, detailsData[i][2], detailsData[i][3], detailsData[i][4]);
                    }
                }
            }
        });

        return true;
    };

    function buildPanel(parentTag, title) {
        $(parentTag).append(`
                <div class='panel panel-default'>
                   <div class='panel-heading'>${title}</div>
                   <div class='panel-body'></div>
                </div>`);

        var $panel = $(parentTag).find('.panel-default').last(),
                $heading = $panel.find('.panel-heading'),
                body = $panel.find('.panel-body').get(0);

        $heading.click((event) => {
            $(body).toggle();
        });

        return body;
    }

    function buildTextEditor(parentTag, editorId, title, text, contentType) {
        $(parentTag).append(`
                <div class='editor-tile'>${title}:</div>
                <div class='editor-frame'>
                   <div id='${editorId}' class='editor-body'>${text}</div>
                </div>`);

        var editor = ace.edit(editorId);
        editor.$blockScrolling = Infinity;
        editor.setOptions({
            readOnly: true,
            showGutter: false,
            highlightActiveLine: false,
            highlightSelectedWord: false,
            autoScrollEditorIntoView: false,
            printMargin: false,
            maxLines: editor.session.getLength()
        });
        editor.session.setMode('ace/mode/' + getEditorMode(contentType));
    }

    function getEditorMode(contentType) {
        if (!contentType)
            return 'sh';

        let type = contentType.toLowerCase();

        if (type.contains('xml'))
            return 'xml';
        if (type.contains('json'))
            return 'json';
        if (type.contains('javascript') || type.contains('js'))
            return 'javascript';
        if (type.contains('pgsql'))
            return 'pgsql';
        if (type.contains('sql'))
            return 'sql';
        if (type.contains('java'))
            return 'java';
        if (type.contains('htm'))
            return 'html';
        if (type.contains('css'))
            return 'css';
        return 'sh'
    }

    function addRow(tblTag, ...txtValues) {
        if (!txtValues || txtValues.length === 0)
            return;

        let row = tblTag.tag('tr');

        for (let i = 0; i < txtValues.length; i++) {
            let value = !txtValues[i] ? '' : txtValues[i];
            let cell = row.tag('td').txt(value);
        }
    }
});