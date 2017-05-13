

function onWsMessage(action, data, source) {

    var tbl = $id("tblCache");

    var h = tbl.tHead;
    tbl.innerHTML = "";
    tbl.appendChild(h);


    for (var hdr in data) {

        var td = tbl.tag("tbody")
                .tag("tr")
                .tag("td")
                .text(hdr)
                .attr({
                    colspan: 10
                })
                .css({
                    textAlign: "center",
                    fontWeight: "bold",
                    background: "none",
                    backgroundColor: "#eef4fa"
                });

        var btn = td.tag("button")
                .text("Wyczyść")
                .css({
                    float: "right"
                });
        btn.hdr = hdr;
        btn.onclick = function (e) {
            ajax.post("?removeGroup", {
                group: e.currentTarget.hdr,
                confirm: "Czy na pewno usunąć całą grupę \"" + e.currentTarget.hdr + "\"?"
            });
        };

        var tbody = tbl.tag("tbody");

        var items = data[hdr].items;
        var cnt = 0;
        for (var name in items) {
            var tr = tbody.tag("tr");
            var item = items[name];

            tr.tag("td")
                    .text(++cnt)
                    .css({
                        //  paddingLeft : (level * 20 + 4) + "px"
                    });

            tr.tag("td").text(item.name);
            tr.tag("td").text(item.size);
            tr.tag("td").text(item.file);
            tr.tag("td").text(item.time);
            tr.tag("td").text(item.access);
            tr.tag("td").text(name);
            tr.tag("td").text(item.user);
            tr.tag("td").text(item.url);

            var td = tr.tag("td");

            td.tag("input")
                    .attr({
                        type: "checkbox",
                        checked: item.locked ? "checked" : undefined
                    })
                    .onclick = function () {
                        return false;
                    };

            tr.id = name;

            tr.onclick = function (e) {
                new Layer(view.id + "?details&id=" + e.currentTarget.id);
            };
        }
    }
}