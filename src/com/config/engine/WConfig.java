package com.config.engine;

import com.config.CService;
import com.exceptions.ServiceError;
import com.json.*;
import com.lang.core.LStr;
import com.model.dataset.AbstractDataSet;
import com.model.dataset.DataSet;
import com.servlet.interfaces.Arg;
import com.webapi.core.*;
import com.webapi.core.DataType;

public class WConfig implements WebApi {

    public final ConfigNode node;

    public WConfig(ConfigNode node) {
        this.node = node;
    }

    @WebApiEndpoint
    public String exportConf(@Arg(name = "usersValue") Boolean usersValue) throws Exception {
        return node.export(usersValue);
    }

    @WebApiEndpoint
    public JObject importConf(WebApiRequest req, @Arg(name = "usersValue") Boolean usersValue) throws Exception {
        try {
            node.import_(JObject.parse(req.getJson().asString()), usersValue);
            return node.getStructure();
        } catch (Exception ex) {
            throw new ServiceError(new LStr("Błąd importu konfiguracji"), ex);
        }
    }

    @WebApiEndpoint
    public DataSet getList() throws Exception {
        DataSet<ConfigField, String> dataSet = new DataSet<>("configList",
                new LStr("Konfiguracja"));

        dataSet.column(String.class, "key", new LStr("Klucz"),
                cf -> cf.getKey()).primaryKey();

        dataSet.column(String.class, "name", new LStr("Nazwa"),
                cf -> cf.getName());

        dataSet.column(String.class, "type", new LStr("Type"),
                cf -> cf.getTypeName());

        dataSet.column(String.class, "value", new LStr("Wartość"),
                cf -> cf.getDisplayValue(null, true));

        dataSet.column(Boolean.class, "def", new LStr("Domyślna"),
                cf -> cf.isDefaultState());

        dataSet.column(String.class, "def_val", new LStr("Wart. domyślna"),
                cf -> cf.getDisplayValue(ValueSource.USER, false));

        dataSet.column(String.class, "user_val", new LStr("Wart. użytkownika"),
                cf -> cf.getDisplayValue(ValueSource.DEFAULT, false));

        dataSet.column(Boolean.class, "multiple", new LStr("Multiple"),
                cf -> cf.isMultiple());

        dataSet.column(Boolean.class, "req", new LStr("Wymagane"),
                cf -> cf.isRequired());

        dataSet.column(Boolean.class, "miss", new LStr("Brakujące"),
                cf -> cf.isMissing());

        for (ConfigElement ce : HConfig.instance().getAll())
            if (ce instanceof ConfigField)
                dataSet.fillRow((ConfigField) ce);

        return dataSet;
    }

    @WebApiEndpoint
    public JObject getStructure() throws Exception {
        return node.getStructure();
    }

    @WebApiEndpoint
    public JObject getItem(@Arg(name = "id") String id) throws Exception {
        ConfigField<?, ?, ?> field = node.getFieldF(id);
        JObject json = field.getStructure(false);
        if (!CService.releaseMode())
            json.insert("field", field.field.getFullName());
        return json;
    }

    @WebApiEndpoint
    public JElement getValue(@Arg(name = "id") String id) {
        return JSON.serialize(node.getFieldF(id).value());
    }

    @WebApiEndpoint(dataType = DataType.JSON)
    public JObject validate(WebApiRequest req, @Arg(name = "id") String id) throws Exception {
        return node.getFieldF(id).store().validateRow(req.getJson());
    }

    @WebApiEndpoint(dataType = DataType.OBJECT)
    public JObject save(WebApiRequest request, @Arg(name = "id") String id) throws Exception {
        ConfigField<?, ?, ?> field = node.getFieldF(id);
        field.store().set(request.getJson().asObject());
        field.save();
        //  return field.getStructure(true);
        return node.getStructure();
    }

    @WebApiEndpoint(dataType = DataType.OBJECT)
    public JObject getDisplayValue(WebApiRequest request, @Arg(name = "id") String id) throws Exception {
        ConfigField<?, ?, ?> field = node.getFieldF(id);
        field = field.clone();
        field.store().set(request.getJson().asObject());
        return field.getStructure(true);
    }

}
