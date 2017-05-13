package com.html.js;

import com.utils.Utils;
import com.utils.Is;
import com.html.core.styles.Selector;
import com.json.JObject;

/**
 * Klasa ułatwia aktualizowanie danych w tabeli w dynamicznych stronach. W
 * połączeniu z metodą Utils.updatetable z pliku utils.js zawartość tableli
 * zostanie zaktualizowana na podstawie danych (json) zwróconych przez klasę.
 *
 * @author user
 */
public class UpdateTable {

    private final String id; // table id
    public final JObject json = new JObject();
    public final Selector styles = new Selector(null);

    @Override
    public String toString() {
        return json.toString();
    }

    public Eval getUpdateScript() {
        return new Eval("Utils.updateTable(" + toString() + ");");
    }

    /**
     * Odwrotna kolejność dodawania wierszy (na początku)
     *
     * @return
     */
    public UpdateTable reverseOrder() {
        json.put("reverse", true);
        return this;
    }

    public UpdateTable(String id, boolean removeMissingRows) {
        this.id = id;
        json.options.compactMode(true);
        json.put("id", id);

        if (removeMissingRows)
            json.put("removeMissing", true);
        styles.callback = (Selector builder, String name, String value) -> {
            json.objectC("styles").put(name, value);
        };
    }

    ;

    public static class UpdateTableCell {

        final JObject json;
        public final Selector styles = new Selector(null);

        public UpdateTableCell(final JObject json) {
            this.json = json;
            styles.callback = new Selector.StyleBuilderCallback() {
                @Override
                public void setStyle(Selector builder, String name, String value) {
                    json.objectC("styles").put(name, value);
                }
            };

        }

        public UpdateTableCell text(Object text) {
            json.put("text", Utils.toString(text));
            return this;
        }

        public UpdateTableCell html(String html) {
            json.put("html", html);
            return this;
        }

        public UpdateTableCell field(String name, String value) {
            json.objectC("fields").put(name, value);
            return this;
        }

        public UpdateTableCell attribute(String name, String value) {
            json.objectC("attrs").put(name, value);
            return this;
        }
    }

    public static class UpdateTableRow {

        final JObject json;
        public final Selector styles = new Selector(null);

        public UpdateTableRow(final JObject json) {
            this.json = json;
            styles.callback = new Selector.StyleBuilderCallback() {
                @Override
                public void setStyle(Selector builder, String name, String value) {
                    json.objectC("styles").put(name, value);
                }
            };
        }

        public UpdateTableRow field(String name, String value) {
            json.objectC("fields").put(name, value);
            return this;
        }

        public UpdateTableRow attribute(String name, String value) {
            json.objectC("attrs").put(name, value);
            return this;
        }

        public UpdateTableCell cell(String id) {
            return new UpdateTableCell(json.arrayC("cells")
                    .object().put("id", id)
            );

        }

        public UpdateTableCell cell(int idx) {
            return new UpdateTableCell(json.arrayC("cells")
                    .object().put("idx", idx)
            );
        }

    }

    public UpdateTableRow addRow(String id) {
        return new UpdateTableRow(json.objectC("rows").objectC(id));
    }

    public UpdateTable removeRow(String rowId) {
        json.objectC("rows").value(rowId, null);
        return this;
    }

    /*
     var _data = {
     rows: {
     row_id: {
     cells: [
     {
     idx: 0,
     text: "aaa",
     styles: {
     fontFamily: "Arial"
     },
     fields: {
     rowNr: 12
     }
     },
     {
     id: "tblA_tr1",
     text: "bbbb",
     class: "style"
     },
     {
     idx: 3,
     html: "<div>dsd</div>"
     }

     ],
     styles: {
     textDecoration: "line-through"
     },
     fields: {
     rowNr: 12
     }
     }
     }
     };
     */
}
