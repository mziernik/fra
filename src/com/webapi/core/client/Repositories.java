package com.webapi.core.client;

import com.model.repository.Column;
import com.model.repository.Repository;
import com.servlet.requests.HttpRequest;
import com.utils.Utils;
import com.utils.collections.Params.Param;
import com.utils.text.NameFormat;
import com.utils.text.StrWriter;
import com.webapi.core.WebApi;
import com.webapi.core.WebApiController;

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

    public static String formatFieldName(String source) {
        return new NameFormat().sourceCamelCase(true).upperCase().format(source);
    }

    @Override
    public void build() {
        writer.setAutoIntent(true);

        writer.append("import {Field, Column, RepoConfig, Repository, Record} from \"../core/core\";").br().br();

        writer.br().br();

        for (Repository<?> repo : Repository.ALL.values()) {

            writer.add("//--------------------------------- ", repo.config.name,
                    " ----------------------------------------------").br().br();

            writer.append("export class ")
                    .append(repo.getClass().getSimpleName())
                    .append(" extends Repository {")
                    .br()
                    .br();

            writer.nextLevel(() -> {

                for (Column<?> col : repo.getColumns().values()) {
                    writer.add("static ", formatFieldName(col.getKey()), ": Column = new Column((c: Column) => {");

                    writer.nextLevel(() -> {
                        for (Param p : col.config.getParams())
                            if (p.value != null)
                                writer.br().add("c.", p.name, " = ", Utils.toString(p.value), ";");
                    });

                    writer.br().add("});").br().br(); //ID: Field = new Field(DataType.INT).primaryKey();
                }

                writer.br().append("constructor() {").br();

                writer.nextLevel(() -> {
                    writer.append("super((c: RepoConfig) => {");

                    writer.nextLevel(() -> {
                        for (Param p : repo.config.getParams())
                            if (p.value != null)
                                writer.br().add("c.", p.name, " = ", Utils.toString(p.value), ";");
                    });
                    writer.br().append("});");

                });
                writer.br().append("}").br();
            });

            writer.br().append("}").br();
            writer.br();

            //-----------------------------------------------------
            writer.append("export class ")
                    .append(repo.getClass().getSimpleName() + "Record")
                    .append(" extends Record {")
                    .br()
                    .br();

            writer.nextLevel(() -> {

                for (Column<?> col : repo.getColumns().values())
                    writer.add(formatFieldName(col.getKey()), ": Field = new Field(",
                            repo.getClass().getSimpleName(), ".", formatFieldName(col.getKey()), ", this);").br();
            });

            writer.br().add("}").br().br(); //ID: Field = new Field(DataType.INT).primaryKey();
        }

        for (Repository<?> repo : Repository.ALL.values()) {
            String name = repo.getClass().getSimpleName();
            String format = formatFieldName(name);
            writer.add("export const ", format, ": ", name, " = Repository.register(new ", name, "());").br();
        }
    }

}
