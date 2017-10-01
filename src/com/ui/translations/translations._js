//# sourceURL=file:///translations/translations.js

registerController(function (ctrl, spa, api) {
    "use strict";

    ctrl.id = "translations";
    ctrl.name = "Tłumaczenia";
    ctrl.urls = ["translations"];
    ctrl.htmlTemplate = "translations/translations.html";
    ctrl.clearOnUnload = false;
    ctrl.language = spa.language.translations;

    ctrl.onUnload = function (e) {
    };

    ctrl.onNewInstance = (e) => {

    };


    var dsTable;
    ctrl.onLoad = function (e) {
        let translationLangSelect = $id("tranlation-language-select");
        let referenceLangSelect = $id("reference-language-select");
        let filterCompleteCheckbox = $id('filter-complete-checkbox');
        let translatedCountText = $id('trans-count-text');
        let googleTranslateAllButton = $id("google-translate-all-button");
        let googleTranslateButton = $id("google-translate-button");
        let referenceLangLabel = $id("ref-lang-label");
        let referenceLangText = $id("ref-lang-text");
        let translationLangLabel = $id("trans-lang-label");
        let translationInputField = $id("trans-lang-text");
        let completeCheckbox = $id("complete-checkbox");
        let saveButton = $id("save-button");
        let cancelButton = $id("cancel-button");

        if (e.newInstance) {
            let languageList;
            let translatedCount;
            let totalCount;
            let selectedValueCell;
            let selectedReferenceCell;
            let selectedCompleteCell;
            let currentTranslationLanguage;
            let currentReferenceLanguage;
            let showNotApproved;

            Promise.all([api.language.googleTranslate.getLanguages(),
                api.language.getAll()])
                    .then((res) => {
                        //Pobieranie listy języków z serwera
                        languageList = res[1].data;

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
                        showNotApproved = filterCompleteCheckbox.checked;

                        if (!currentReferenceLanguage)
                            referenceLangLabel.style.display = 'none';

                        updateTable(() => {
                            let strings = [];

                            debugger;
                            dsTable.dataSet

                            //Inicjalizacja okna Google Translate
                            new TranslateDialog({
                                trigger: googleTranslateButton,
                                languages: res.data,
                                strings: ['strings'],
                                translationAPI: api.language.googleTranslate,
                                srcLang: currentTranslationLanguage,
                                onFinish: (res) => {
                                    updateTable();
                                    debugger;
                                }
                            });
                        });
                    });

            dsTable = new DsTable((opt) => {
                opt.id = ctrl.id;
                opt.tblTag = $id("tblTranslations");
                opt.webApiMethod = (arg) => {
                    return api.language.getEntries(arg);
                };
            });

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

            dsTable.onClick = (e, cell) => {
                selectedValueCell = cell.row.cells.val;
                selectedReferenceCell = cell.row.cells.ref;
                selectedCompleteCell = cell.row.cells.complete;

                translationInputField.value = selectedValueCell.value;
                completeCheckbox.checked = selectedCompleteCell.value;

                if (selectedReferenceCell)
                    referenceLangText.innerText = selectedReferenceCell.value;
            };

            dsTable.drawCell = (cell, td, value) => {
                if (!value)
                    td.style.backgroundColor = "#faa";

                td.innerText = value || "\xa0";
            };

            saveButton.onclick = (e) => {
                if (!selectedValueCell)
                    return;

                selectedValueCell.value = translationInputField.value;
                selectedValueCell.redraw();
                selectedCompleteCell.value = true;
                completeCheckbox.checked = true;
                selectedCompleteCell.redraw();
                debugger;
                api.language.editEntry({
                    params: {
                        language: currentTranslationLanguage,
                        key: selectedValueCell.row.cells.id.value,
                        value: translationInputField.value,
                        complete: completeCheckbox.checked
                    }
                });
            };

            cancelButton.onclick = (e) => {
                if (!selectedValueCell)
                    return;

                translationInputField.value = selectedValueCell.value;
            };

            filterCompleteCheckbox.onchange = (e) => {
                showNotApproved = filterCompleteCheckbox.checked;
                updateTable();
            };

            completeCheckbox.onchange = (e) => {
                selectedCompleteCell.value = completeCheckbox.checked;
                selectedCompleteCell.redraw();
            };

            function updateTable(callback) {
                let params = {};
                params.langKey = currentTranslationLanguage;
                params.refKey = currentReferenceLanguage;
                params.incomplete = showNotApproved;
                params.onSuccess = () => {
                    if (callback && typeof callback === 'function')
                        callback();
                };
                dsTable.reloadData(params);

                totalCount = dsTable.visibleRowsCount;
                translatedCount = 0;
                for (let i = 0; i < dsTable.rows.list.length; i++) {
                    let row = dsTable.rows.list[i];

                    if (row.cells.complete)
                        translatedCount++;
                }

                translatedCountText.txt(`${translatedCount}/${totalCount}`);

                referenceLangText.value = '';
                translationInputField.value = '';

                translationLangLabel.txt(languageList[currentTranslationLanguage]);
                if (referenceLangLabel)
                    referenceLangLabel.txt(languageList[currentReferenceLanguage]);
            }
        }
    };
});