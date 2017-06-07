package com.webapi.core.client;

import com.database.model.DsTable;
import com.json.JObject;
import com.model.dataset.DsColumn;
import com.servlet.requests.HttpRequest;
import com.utils.text.NameFormat;
import com.utils.text.StrWriter;
import com.webapi.core.WebApi;
import com.webapi.core.WebApiController;
import sun.security.pkcs11.Secmod.DbMode;

/**
 *
 * @author milosz
 */
public class Repositories extends WebApiClientBuilder {

    public Repositories(WebApiController controller, StrWriter writer) {
        super(controller, writer);
    }

    @Override
    public void build(HttpRequest http, Class<? extends WebApi> cls, String parent) {

        build();
    }

    @Override
    public void build() {
        writer.setAutoIntent(true);

        writer.append("import {Field, DataType, Repository, Record} from \"../core/core\";").br().br();

        StrWriter exports = new StrWriter();

        writer.br().br();

        for (DsTable<?, ?> tbl : DsTable.getTables().values()) {

            String name = tbl.getClass().getSimpleName();

            String format = new NameFormat().sourceCamelCase(true).upperCase().format(name);
            name += "Repo";

            exports.add("export const ", format, ": ", name, " = Repository.register(new ", name, "());").br();

            writer.add("export class ", name, " extends Repository {").br().br();

            writer.nextLevel(() -> {

                writer.br().append("constructor() {").br();

                writer.nextLevel(() -> {
                    writer.append("super(")
                            .escape(tbl.key).append(", ")
                            .escape(tbl.name).append(", ")
                            .append("DataType.INT").append(", ")
                            .append(tbl.getClass().getSimpleName())
                            .append(");");
                });
                writer.br().append("}").br();

//             
            });

            writer.br().append("}").br();
            writer.br();

            writer.append("export class ")
                    .append(tbl.getClass().getSimpleName())
                    .append(" extends Record {")
                    .br()
                    .br();

            writer.nextLevel(() -> {

                for (DsColumn<?, ?, ?, ?> col : tbl.getColumns().values())
                    writer.append(col.getKey().toUpperCase())
                            .append(": Field = new Field(DataType.")
                            .append(col.type.name)
                            .add(").name(").escape(col.getKey())
                            .add(").title(").escape(col.getName())
                            .add(")", tbl.getPrimaryKeyColumn() == col ? ".primaryKey()" : "", ";")
                            .br().br(); //ID: Field = new Field(DataType.INT).primaryKey();

                writer.br().append("constructor() {").br();

                writer.nextLevel(() -> {
                    writer.append("super(...arguments);").br()
                            .append("this.init();").br();
                });

                writer.append("};").br().br();
            });
//          
            writer.br().append("}").br();
            writer.br();
        }

        writer.append(exports.toString());
    }

}
