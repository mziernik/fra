//# sourceURL=file:///translations/FwBootstrapDialog.js

/**
 * @param {HTMLAnchorElement|HTMLButtonElement} - trigger link or button that opens the dialog
 * @param {string} [title] - modal title
 * @param {Array} [bodyElements] - modal body elements
 * @param {string} bodyElements.type - body element type (HTML node name)
 * @param {string} [bodyElements.id] - body element id
 * @param {string} [bodyElements.label] - body element label text
 * @param {boolean} [bodyElements.required=false] - body element label text
 * @param {Arraay} [buttons] - modal buttons
 * @param {string} [buttons.text] - title of the button
 * @param {callback} [buttons.action] - button click callback
 * @param {string} [buttons.class] - button CSS class
 * @param {boolean} [buttons.dismissOnClick=true] - close modal on button action
 * @param {callback} [onOpen] - callback triggered, when dialog opens
 */

class FwBootstrapDialog {
    constructor(options) {
        this.title = options.title || '';

        this.modalContainer = document.body.tag('div')
                .css('display', 'block')
                .attr({
                    class: 'modal fade',
                    id: Utils.randomId(6),
                    role: 'dialog'
                });

        let modalDialogDiv = this.modalContainer.tag('div')
                .attr('class', 'modal-dialog');

        let modalContentDiv = modalDialogDiv.tag('div')
                .attr('class', 'modal-content');

        this.headerContainer = modalContentDiv.tag('div')
                .attr('class', 'modal-header');

        this.bodyContainer = modalContentDiv.tag('div')
                .attr('class', 'modal-body');

        this.bodyElements = {};

        if (options.bodyElements)
            for (let i = 0; i < options.bodyElements.length; i++) {
                let currElem = options.bodyElements[i];
                currElem.id = currElem.id || Utils.randomId(6);

                let bodyElement = document.createElement(currElem.type)
                        .attr({
                            id: currElem.id,
                            class: 'form-control',
                            required: currElem.required || 'false'
                        });

                if (currElem.label) {
                    let elemLabel = document.createElement('label')
                            .attr('for', bodyElement.id)
                            .txt(currElem.label);

                    this.bodyContainer.appendChild(elemLabel);
                }
                this.bodyElements[currElem.id] = bodyElement;

                this.bodyContainer.appendChild(bodyElement);
            }

        this.footerContainer = modalContentDiv.tag('div')
                .attr('class', 'modal-footer');

        let modalTitle = this.headerContainer.tag('span')
                .css({
                    'font-size': '14px',
                    color: '#ffffff'
                })
                .txt(this.title);

        if (options.buttons)
            this._appendButtons(options.buttons);

        options.trigger.attr('data-target', '#' + this.modalContainer.id);
        options.trigger.attr('data-toggle', 'modal');

        options.trigger.onclick = (e) => {
            if (options.onOpen)
                options.onOpen(this.modalContainer);
        };
    }
    appendFooter(node) {
        this.footerContainer.appendChild(node);
    }
    appendBody(node) {
        this.bodyContainer.appendChild(node);
    }
    hide() {
        $(this.modalContainer).modal('hide');
    }
    _appendButtons(buttonOptions) {
        for (let i = 0; i < buttonOptions.length; i++) {
            let currButton = buttonOptions[i];

            let button = this.footerContainer.tag('button')
                    .attr('class', currButton.class)
                    .txt(currButton.title);

            if (typeof currButton.dismissOnClick === 'undefined' || currButton.dismissOnClick)
                button.attr('data-dismiss', 'modal');

            button.onclick = (e) => {
                attachActionEvent(button, currButton, this);
            };
        }
        function attachActionEvent(DOMButton, button, container) {
            if (button.action && typeof button.action === 'function')
                button.action(container.bodyContainer, container.bodyElements);
        }
    }
}