//# sourceURL=file:///translations/translations.js

registerController(function (ctrl, api) {
    "use strict";

    ctrl.id = "translations";
    ctrl.name = "TĹ‚umaczenia";
    ctrl.urls = ["translations"];
    ctrl.htmlTemplate = "translations/translations.html";
    ctrl.clearOnUnload = false;
    ctrl.language = spa.language.translations;

    ctrl.onUnload = function (e) {
    };

    let dsTable;
    let translateDialog;
    ctrl.onNewInstance = function (e) {

        let translationLangSelect = $id("tranlation-language-select");
        let referenceLangSelect = $id("reference-language-select");
        let referenceLangLabel = $id("ref-lang-label");
        let referenceLangText = $id("ref-lang-text");
        let translationInputField = $id("trans-lang-text");
        let completeCheckbox = $id("complete-checkbox");

        if (e.newInstance) {
            let languageList;
            let selectedCells;
            let currentTranslationLanguage;
            let currentReferenceLanguage;

            dsTable = new DsTable((opt) => {
                opt.controller = ctrl;
                opt.tag = $id("tblTranslations");
                opt.webApiMethod = (arg) => {
                    return api.language.getEntries(arg);
                };
            });

            new FwBootstrapDialog({
                title: 'Dodaj język',
                bodyElements: [
                    {
                        type: 'input',
                        id: 'dialog-new-lang-name-input',
                        label: 'Nazwa języka',
                        required: true
                    },
                    {
                        type: 'input',
                        id: 'dialog-key-lang-name-input',
                        label: 'Klucz języka',
                        required: true
                    }
                ],
                trigger: $id('add-lang-button'),
                buttons: [
                    {
                        title: 'Dodaj',
                        class: 'btn btn-info',
                        dismissOnClick: true,
                        action: (window, bodyElements) => {
                            api.language.add({
                                params: {
                                    key: bodyElements['dialog-key-lang-name-input'].value,
                                    name: bodyElements['dialog-new-lang-name-input'].value
                                }
                            });
                        }
                    },
                    {
                        title: 'Zamknij',
                        class: 'btn btn-default',
                        dismissOnClick: true
                    }
                ]
            });

            api.language.getAll()
                    .then((res) => {
                        languageList = res.data;

                        //Rysowanie opcjii dla selectów z lista jęazyków
                        for (let lang in languageList) {
                            let langOption = document.createElement('option');
                            langOption.value = lang;
                            langOption.innerText = languageList[lang];
                            translationLangSelect.appendChild(langOption);
                            referenceLangSelect.appendChild(langOption.cloneNode(true));
                        }

                        currentTranslationLanguage = translationLangSelect.value;
                        currentReferenceLanguage = referenceLangSelect.value !== 'none'
                                ? referenceLangSelect.value : undefined;

                        if (!currentReferenceLanguage)
                            referenceLangLabel.style.display = 'none';

                        updateTable();
                    });

            api.language.googleTranslate.getLanguages()
                    .then((res) => {
                        $id("google-translate-all-button").style.display = 'initial';

                        //Inicjalizacja okna Google Translate
                        translateDialog = new TranslateDialog({
                            trigger: $id("google-translate-all-button"),
                            languages: res.data,
                            strings: dsTable.dataSet,
                            translationAPI: api.language.googleTranslate,
                            srcLang: currentTranslationLanguage,
                            onFinish: (res) => {
                                debugger;
                            }
                        });
                    });

            dsTable.onDataLoaded = (data) => {
                let totalCount = dsTable.rows.list.length;
                let translatedCount = 0;

                for (let i = 0; i < dsTable.rows.list.length; i++) {
                    let row = dsTable.rows.list[i];

                    if (row.cells.complete.value)
                        translatedCount++;
                }

                $id('trans-count-text').txt(`${translatedCount}/${totalCount}`);

                referenceLangText.value = '';
                translationInputField.value = '';

                $id("trans-lang-label").txt(languageList[currentTranslationLanguage]);
                if (referenceLangLabel)
                    referenceLangLabel.txt(languageList[currentReferenceLanguage]);

                if (translateDialog)
                    translateDialog.updateStrings(dsTable.dataSet);
            };

            dsTable.drawCell = (cell, td, value) => {
                if (!value)
                    td.style.backgroundColor = "#faa";

                td.innerText = value || "\xa0";
            };

            dsTable.onClick = (e, cell) => {
                selectedCells = cell.row.cells;

                translationInputField.value = selectedCells.val.value;
                completeCheckbox.checked = selectedCells.complete.value;

                if (selectedCells.ref)
                    referenceLangText.innerText = selectedCells.ref.value;
            };

            translationLangSelect.onchange = (e) => {
                currentTranslationLanguage = translationLangSelect.value;

                updateTable();
            };

            referenceLangSelect.onchange = (e) => {
                currentReferenceLanguage = referenceLangSelect.value !== 'none'
                        ? referenceLangSelect.value : undefined;

                if (!currentReferenceLanguage)
                    referenceLangLabel.style.display = 'none';
                else
                    referenceLangLabel.style.display = 'initial';

                updateTable();
            };

            $id('filter-complete-checkbox').onchange = (e) => {
                updateTable();
            };

            $id('remove-lang-button').onclick = (e) => {
                api.language.remove({
                    params: {
                        key: currentTranslationLanguage
                    }
                });
            };

            $id("google-translate-button").onclick = (e) => {
                if (currentReferenceLanguage && selectedCells)
                    api.language.googleTranslate.translate({
                        params: {
                            sl: currentReferenceLanguage,
                            tl: currentTranslationLanguage,
                            text: selectedCells.ref.value
                        }
                    }).then((res) => {
                        translationInputField.value = res.data.result[0][0][0];
                    });
            };

            $id("save-button").onclick = (e) => {
                if (!selectedCells)
                    return;

                selectedCells.val.value = translationInputField.value;
                selectedCells.val.redraw();
                selectedCells.complete.value = completeCheckbox.checked;
                selectedCells.complete.redraw();
//                debugger;
                api.language.editEntry({
                    params: {
                        language: currentTranslationLanguage,
                        key: selectedCells.val.row.cells.id.value,
                        value: translationInputField.value,
                        complete: completeCheckbox.checked
                    }
                });
            };

            $id("cancel-button").onclick = (e) => {
                if (!selectedCells.val)
                    return;

                translationInputField.value = selectedCells.val.value;
            };

            translationInputField.onchange = (e) => {
                selectedCells.complete.value = true;
                completeCheckbox.checked = true;
                selectedCells.complete.redraw();
            };

            function updateTable() {
                let params = {};
                params.langKey = currentTranslationLanguage;
                params.refKey = currentReferenceLanguage;
                params.incomplete = $id('filter-complete-checkbox').checked;

                dsTable.reloadData(params);
            }
        }
    };
    ctrl.onLoad = (e) => {

    };
});