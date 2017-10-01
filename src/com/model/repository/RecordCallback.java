package com.model.repository;

import com.exceptions.EError;
import com.intf.runnable.RunnableEx1;
import com.json.JObject;
import com.mlogger.Log;
import com.model.repository.Column.RepoFieldConfig;
import com.utils.reflections.datatype.DataType;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Objects;

public class RecordCallback {

    final Repository<?> repo;
    final Column<?> column;
    final Record record;
    final Object value;

    public final boolean isCreate;
    public final boolean isEdit;
    public final boolean isValidate;

    final Map<Column, RecordField> fields = new LinkedHashMap<>();

    public RecordCallback(Repository<?> repo, Object primaryKey, Column column, Object value) {
        this.repo = repo;
        this.column = column;
        this.record = primaryKey != null ? ((Repository) repo).read(primaryKey) : null;
        this.value = value;

        this.isCreate = record == null;
        this.isEdit = record != null && column == null;
        this.isValidate = record != null && column != null;
    }

    public JObject getJson() {
        JObject json = new JObject();
        fields.forEach((Column col, RecordField rf) -> json.put(col.getKey(), rf.getJson()));
        return json;
    }

    public <T> RecordCallback field(Column<T> column, RunnableEx1<RecordField<T>> consumer) {
        RecordField rf = new RecordField(repo.getColumnF(column.getKey()));
        fields.put(column, rf);
        try {
            consumer.run(rf);
        } catch (Exception ex) {
            rf.error = ex;
            Log.warning(ex);
        }
        return this;
    }

    public class RecordField<T> {

        public final Column<?> column;
        public final RepoFieldConfig cfg;
        public Exception error;
        public String warning;

        public T value;
        public DataType type;
        public Boolean readOnly;
        public Boolean required;
        public Boolean unique;
        public CharSequence name;
        public CharSequence description;
        public CharSequence hint;
        public boolean visible = true;
        public Integer min;
        public Integer max;
        public Boolean trimmed;
        public String regex;
        public String defaultUnit;

        public final LinkedHashMap<String, String> enumerate = new LinkedHashMap<>();

        RecordField(Column<T> column) {
            this.column = column;
            this.cfg = column.config;
            this.type = cfg.type;
            this.value = record != null ? record.get(column) : null;
            this.readOnly = cfg.readOnly;
            this.required = cfg.required;
            this.unique = cfg.unique;
            this.name = cfg.name;
            this.description = cfg.description;
            this.hint = cfg.hint;
            this.min = cfg.min;
            this.max = cfg.max;
            this.trimmed = cfg.trimmed;
            this.regex = cfg.regex;
            this.defaultUnit = cfg.defaultUnit;
        }

        public JObject getJson() {
            JObject json = new JObject();

            if (value != null)
                json.put("value", cfg.type.serialize(value));

            if (error != null)
                json.put("error", EError.toString(error));
            if (warning != null)
                json.put("warning", warning);

            if (!this.enumerate.isEmpty())
                json.put("enumerate", this.enumerate);

            if (!Objects.equals(this.type, cfg.type))
                json.put("type", this.type.name);
            
            if (!Objects.equals(this.readOnly, cfg.readOnly))
                json.put("readOnly", this.readOnly);
            
            if (!Objects.equals(this.required, cfg.required))
                json.put("required", this.required);
            
            if (!Objects.equals(this.unique, cfg.unique))
                json.put("unique", this.unique);
            
            if (!Objects.equals(this.name, cfg.name))
                json.put("name", this.name);
            
            if (!Objects.equals(this.description, cfg.description))
                json.put("description", this.description);
            
            if (!Objects.equals(this.hint, cfg.hint))
                json.put("hint", this.hint);
            
            if (!Objects.equals(this.min, cfg.min))
                json.put("min", this.min);
            
            if (!Objects.equals(this.max, cfg.max))
                json.put("max", this.max);
            
            if (!Objects.equals(this.trimmed, cfg.trimmed))
                json.put("trimmed", this.trimmed);
            
            if (!Objects.equals(this.regex, cfg.regex))
                json.put("regex", this.regex);
            
            if (!Objects.equals(this.defaultUnit, cfg.defaultUnit))
                json.put("defaultUnit", this.defaultUnit);

            return json;
        }

    }

}
