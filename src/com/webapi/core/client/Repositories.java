package com.webapi.core.client;

import com.model.repository.Column;
import com.model.repository.ForeignColumn;
import com.model.repository.ForeignColumns;
import com.model.repository.Repository;
import com.model.repository.Repository.RepoAction;
import com.model.repository.Repository.RepoReference;
import com.model.repository.intf.IForeignColumn;
import com.servlet.requests.HttpRequest;
import com.utils.Utils;
import com.utils.collections.Params;
import com.utils.collections.Params.Param;
import com.utils.collections.TList;
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
        
        writer.append("import {Cell, Column, RepoConfig, Repository, Record} from \"../core/core\";").br().br();
        
        writer.br().br();
        
        for (Repository<?> repo : Repository.ALL.values()) {
            
            Params params = repo.config.getParams();
            
            writer.add("//--------------------------------- ", repo.config.name,
                    " ----------------------------------------------").br().br();
            
            writer.add("export class ", repo.config.clientRepoClassName,
                    " extends Repository {").br().br();
            
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
                        for (Param p : params)
                            if (p.value != null)
                                writer.br().add("c.", p.name, " = ", Utils.toString(p.value), ";");
                    });
                    writer.br().append("});");
                    
                });
                
                writer.br().append("}").br();
            });
            
            if (!repo.config.actions.isEmpty()) {
                writer.br();
                repo.config.actions.forEach((String key, RepoAction act) -> {
                    if (!act.record)
                        writer.add(key, " = (params: ?Object = null)",
                                " => this._execute('", act.key, "', params);",
                                " // akcja ", Utils.escape(act.name)).br();
                    
                });
            }
            
            writer.br().append("}").br();
            writer.br();

            //-----------------------------------------------------
            writer.add("export class ", repo.config.clientRecordClassName, " extends Record {").br().br();
            
            writer.nextLevel(() -> {
                
                TList<IForeignColumn<?>> fCols = new TList<>();
                
                for (Column<?> col : repo.getColumns().values()) {
                    if (col instanceof IForeignColumn)
                        fCols.add((IForeignColumn<?>) col);
                    writer.add(formatFieldName(col.getKey()), ": Cell = new Cell(this, ",
                            repo.config.clientRepoClassName, ".", formatFieldName(col.getKey()), ");",
                            " // ", Utils.escape(col.config.name)
                    ).br();
                }
                
                if (!fCols.isEmpty()) {
                    writer.br();
                    fCols.forEach((IForeignColumn<?> iCol) -> {
                        
                        Column<?> col = (Column<?>) iCol;
                        Column<?> fCol = iCol.getForeignColumn();
                        Repository fRepo = fCol.getRepository(true);
                        
                        writer.add(col.getKey(), "Foreign", " = (context: any): ",
                                fRepo.config.clientRecordClassName,
                                col instanceof ForeignColumns ? "[]" : "",
                                " => this._getForeign(context, ",
                                repo.config.clientRepoClassName,
                                ".", formatFieldName(col.getKey()), ");",
                                " // klucz obcy ", col.getKey(), " -> ", fCol.toString())
                                .br();
                        
                    });
                    
                }
                
                if (!repo.config.references.isEmpty()) {
                    writer.br();
                    repo.config.references.forEach((String key, RepoReference ref) -> {
                        Repository fRepo = ref.column.getRepository(true);
                        writer.add(key, " = (context: any): ",
                                fRepo.config.clientRecordClassName,
                                "[]", " => this._getReferences(context, ",
                                fRepo.config.clientRepoClassName,
                                ".", formatFieldName(ref.column.getKey()), ");",
                                " // referencja ", ref.column.toString(), " -> ", repo.config.primaryKey.getKey())
                                .br();
                    });
                }
                
                if (!repo.config.actions.isEmpty()) {
                    writer.br();
                    repo.config.actions.forEach((String key, RepoAction act) -> {
                        if (act.record)
                            writer.add(key, " = (params: ?Object = null)",
                                    " => this._execute('", act.key, "', params);",
                                    " // akcja ", Utils.escape(act.name)).br();
                        
                    });
                }
                
            });
            
            writer.br().add("}").br().br(); //ID: Field = new Field(DataType.INT).primaryKey();
        }
        
        for (Repository<?> repo : Repository.ALL.values()) {
            String name = repo.config.clientRepoClassName;
            String format = formatFieldName(name);
            writer.add("export const ", format, ": ", name, " = Repository.register(new ", name, "());").br();
        }
    }
    
}
