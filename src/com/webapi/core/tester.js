
var editor;
var code = undefined;
var fields = {};
var endpointName;

function format(val) {

    if (!(typeof val === "string"))
        val = editor.getValue();

    if (!val) {
        code = null;
        buildHash();
        return;
    }

    var xhttp = new XMLHttpRequest();
    xhttp.onreadystatechange = function () {
        if (this.readyState === 4) {
            if (this.status === 200) {
                editor.setValue(this.responseText);
                editor.selection.clearSelection();
                eval("code=" + this.responseText);
                buildHash();
                return;
            }
            if (this.responseText)
                alert(this.responseText);
        }
    };
    xhttp.open("POST", "/$/tools/json", true);
    xhttp.setRequestHeader("Content-type", "text/plain");
    xhttp.send(val);
}

function buildHash() {
    var hash = JSON.stringify([fields, code]);
    location.hash = hash;
    if (code !== undefined)
        window.localStorage.setItem("webapi-tester-" + endpointName, hash);
}

function onHashChange() {
    var hash = window.location.hash.substring(1).trim();
    if (!hash)
        return;

    var data = JSON.parse(hash);
    if (data instanceof Array) {
        var rows = document.getElementById("tblParams").getElementsByTagName("tr");
        var idx = 0;
        fields = {};
        for (var name in data[0]) {
            var inputs = rows[idx++].getElementsByTagName("input");
            inputs[0].value = name;
            inputs[1].value = data[0][name];
            fields[inputs[0].value] = inputs[1].value;
        }

        for (; idx < rows.length; idx++) {
            var inputs = rows[idx].getElementsByTagName("input");
            inputs[0].value = "";
            inputs[1].value = "";
        }

        if (data[1])
            format(JSON.stringify(data[1]));
    }

}

addEventListener("hashchange", onHashChange);

addEventListener("load", function () {

    endpointName = document.getElementById("endpointName").value;

    editor = ace.edit("ta-data");
    editor.setOptions({
        showGutter: false,
        highlightActiveLine: false,
        printMargin: false
    });

    editor.commands.addCommand({
        name: "format1",
        bindKey: "Ctrl-Alt-F",
        exec: format
    });

    editor.commands.addCommand({
        name: "format2",
        bindKey: "Shift-Alt-F",
        exec: format
    });

    editor.commands.addCommand({
        name: "format3",
        bindKey: "Shift-Ctrl-F",
        exec: format
    });

    //  editor.setTheme("ace/theme/eclipse");
    editor.session.setUseWorker(false);
    editor.session.setMode("ace/mode/javascript");
    editor.session.setTabSize(2);
    editor.$blockScrolling = Infinity;



    if (!window.location.hash.substring(1))
        window.location.hash = window.localStorage.getItem("webapi-tester-" + endpointName);
    else
        onHashChange();

    setTimeout(function () {
        send(true);
    }, 100);
});

function send() {

    fields = {};
    var rows = document.getElementById("tblParams").getElementsByTagName("tr");

    var frm = document.createElement("form");
    frm.setAttribute("target", "result");
    frm.setAttribute("method", "post");
    frm.setAttribute("action", document.getElementById("endpointUrl").value);

    for (var i = 0; i < rows.length; i++) {
        var inputs = rows[i].getElementsByTagName("input");
        inputs[0].onkeydown = inputs[1].onkeydown = function (e) {
            if (e.keyCode === 13)
                send();
        };

        if (!inputs[0].value)
            continue;
        var input = document.createElement("input");
        frm.appendChild(input);
        input.name = inputs[0].value;
        input.value = inputs[1].value;

        fields[inputs[0].value] = inputs[1].value;
    }

    var input = document.createElement("input");
    frm.appendChild(input);

    input.name = "%data";
    input.value = editor.getValue();


    buildHash();

    format();

    document.body.appendChild(frm);
    frm.submit();
    document.body.removeChild(frm);

}

