//# sourceURL=file:///translations/TranslateDialog.js

/**
 * @param {HTMLAnchorElement|HTMLButtonElement} trigger - link or button that opens the dialog
 * @param {Array} strings - array of strings to translate
 * @param {string} srcLang - input strings language
 * @param {Array} languages - array of strings to translate
 * @param {Array} translationAPI - translation API
 * @param {string} [text.title] - alternative title
 * @param {string} [text.closeBtnText] - alternative label text for close button
 * @param {string} [text.translateBtnText] - alternative label text for translate button
 * @param {function} [onOpen] - callback triggered, when dialog opens
 * @param {function} [onFinish] - callback triggered, when translation finishes, returning the result
 * @returns {HTMLDivElement} instance - dialog instance
 */

class TranslateDialog extends FwBootstrapDialog {
    constructor(options) {
        if (!options.strings)
            options.strings = [];

        if (!options.text)
            options.text = {};

        super({
            trigger: options.trigger,
            title: 'Google translate',
            buttons: [
                {
                    title: options.text.translateBtn || 'Tłumacz',
                    class: 'btn btn-info',
                    dismissOnClick: false,
                    action: () => {
                        this._getTranslations()
                                .then((res) => {
                                    if (options.onFinish && typeof options.onFinish === 'function')
                                        options.onFinish(res);

                                    $(this.containerDiv).modal('hide');

                                    setTimeout(() => {
                                        this.progressBar.style.display = 'none';
                                    }, 500);
                                });
                    }
                },
                {
                    title: options.text.closeBtn || 'Zamknij',
                    class: 'btn btn-default',
                    dismissOnClick: false,
                    action: () => {
                        if (this.autoTranstaleRequest)
                            this.autoTranstaleRequest.cancel();

                        this.hide();
                    }
                }
            ]
        });

        this.text = {
            title: options.text.title || 'Automatyczne tłumaczenie',
            translateBtn: options.text.translateBtnText || 'Tłumacz',
            closeBtn: options.text.closeBtnText || 'Zamknij',
            selectLang: options.text.selectLangText || 'Wybierz język'
        };

        this.langs = options.languages;
        this.strings = options.strings;
        this.translate = options.translationAPI;
        this.sourceLanguage = options.srcLang || 'pl';
        this.targetLanguage = '';
        this.onFinish = options.onFinish;

        this.progressBar = $tag('progress')
                .css({
                    display: 'none',
                    float: 'left'
                })
                .attr('max', Object.keys(options.strings).length);

        this.errorMessage = $tag('span')
                .css({
                    'font-size': '11px',
                    color: '#cc1313',
                    display: 'none'
                });

        this.appendFooter(this.progressBar);
        this.appendBody(this._generateBody());
    }
    updateStrings(strings) {
        this.strings = strings;
        this.progressBar.attr('max', Object.keys(strings).length);
    }
    _generateBody(container) {
        let bodyContainer = this.bodyContainer.tag('div');

        let languageDiv = bodyContainer.tag('div').attr('class', 'form-group');

        let languageLabel = languageDiv.tag('label')
                .attr('for', 'translate-dialog-lang-input')
                .txt(this.text.selectLang);

        let languageInput = languageDiv.tag('select')
                .attr({
                    class: 'form-control',
                    id: 'translate-dialog-lang-input'
                });

        for (let lang in this.langs.nativeNames) {
            if (!this.targetLanguage)
                this.targetLanguage = lang;

            languageInput.tag('option')
                    .attr('value', lang)
                    .txt(this.langs.nativeNames[lang]);
        }

        languageInput.addEventListener('change', (event) => {
            this.targetLanguage = event.target.value;
        });

        languageDiv.appendChild(this.errorMessage);
        return bodyContainer;
    }
    _putErrorMessage(text) {
        this.errorMessage.innerText = text;
        this.errorMessage.style.display = 'initial';
    }
    _getTranslations() {
        let defer = $.Deferred();
        let result = [];

        this.progressBar.style.display = 'initial';

        if (this.strings.length === 0)
            defer.resolve(null);

        this.autoTranstaleRequest = this.translate.autoTransplate({
            params: {
                sl: this.sourceLanguage,
                tl: this.targetLanguage
            },
            onEvent: (res) => {
                result.push(res.data.phrase);
                this.progressBar.attr('max', res.data.total);
                this.progressBar.attr('value', res.data.index);
            },
            onSuccess: (res) => {
                debugger;
                defer.resolve(result);
            }
        });

//        for (let i = 0; i < this.strings.length; i++) {
//            let currString = this.strings[i];

//            if (currString.complete)

//        }
        return defer.promise();
    }
}