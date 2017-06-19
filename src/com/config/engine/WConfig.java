package com.config.engine;

import com.config.CService;
import com.exceptions.ServiceError;
import com.json.*;
import com.lang.core.LStr;
import com.model.repository.DynamicRepo;
import com.model.repository.Repository;
import com.servlet.interfaces.Arg;
import com.utils.reflections.datatype.DataType;
import com.webapi.core.*;

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
    public Repository getList() throws Exception {
        DynamicRepo<ConfigField, String> dataSet = new DynamicRepo<>("configList",
                new LStr("Konfiguracja"));

        dataSet.column(String.class, "key", DataType.KEY, new LStr("Klucz"),
                cf -> cf.getKey());

        dataSet.column(String.class, "name", DataType.STRING, new LStr("Nazwa"),
                cf -> cf.getName());

        dataSet.column(String.class, "type", DataType.STRING, new LStr("Type"),
                cf -> cf.getTypeName());

        dataSet.column(String.class, "value", DataType.STRING, new LStr("Wartość"),
                cf -> cf.getDisplayValue(null, true));

        dataSet.column(Boolean.class, "def", DataType.BOOLEAN, new LStr("Domyślna"),
                cf -> cf.isDefaultState());

        dataSet.column(String.class, "def_val", DataType.STRING, new LStr("Wart. domyślna"),
                cf -> cf.getDisplayValue(ValueSource.USER, false));

        dataSet.column(String.class, "user_val", DataType.STRING, new LStr("Wart. użytkownika"),
                cf -> cf.getDisplayValue(ValueSource.DEFAULT, false));

        dataSet.column(Boolean.class, "multiple", DataType.BOOLEAN, new LStr("Multiple"),
                cf -> cf.isMultiple());

        dataSet.column(Boolean.class, "req", DataType.BOOLEAN, new LStr("Wymagane"),
                cf -> cf.isRequired());

        dataSet.column(Boolean.class, "miss", DataType.BOOLEAN, new LStr("Brakujące"),
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

    @WebApiEndpoint()
    public JObject validate(WebApiRequest req, @Arg(name = "id") String id) throws Exception {
        return node.getFieldF(id).store().validateRow(req.getJson());
    }

    @WebApiEndpoint()
    public JObject save(WebApiRequest request, @Arg(name = "id") String id) throws Exception {
        ConfigField<?, ?, ?> field = node.getFieldF(id);
        field.store().set(request.getJson().asObject());
        field.save();
        //  return field.getStructure(true);
        return node.getStructure();
    }

    @WebApiEndpoint()
    public JObject getDisplayValue(WebApiRequest request, @Arg(name = "id") String id) throws Exception {
        ConfigField<?, ?, ?> field = node.getFieldF(id);
        field = field.clone();
        field.store().set(request.getJson().asObject());
        return field.getStructure(true);
    }

}
