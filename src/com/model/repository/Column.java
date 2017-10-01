package com.model.repository;

import com.database.Database;
import com.events.Dispatcher;
import com.intf.callable.CallableEx1;
import com.model.repository.intf.Align;
import com.intf.runnable.Runnable1;
import com.intf.runnable.RunnableEx2;
import com.intf.runnable.RunnableEx3;
import com.json.JObject;
import com.json.JSON;
import com.model.repository.intf.CRUDE;
import com.model.repository.intf.CaseConvert;
import com.model.repository.intf.IForeignColumn;
import com.utils.Is;
import com.utils.TObject;
import com.utils.Utils;
import com.utils.collections.Pair;
import com.utils.collections.Params;
import com.utils.collections.TList;
import com.utils.reflections.datatype.DataType;
import com.utils.reflections.datatype.EnumDataType;
import com.utils.reflections.datatype.EnumsDataType;
import com.utils.text.NameFormat;
import com.webapi.core.client.Repositories;
import java.util.LinkedHashMap;
import java.util.Objects;

public class Column<RAW> {

    private final String UNDEFINED_NAME = "UNDEFINED:d343f0fb-cfca-4fcf-a8e3-9c943d2ee89a";

    Repository<?> repository;

    public final RepoFieldConfig config = new RepoFieldConfig();

    public Column(Runnable1<RepoFieldConfig> cfg) {

        try {
            cfg.run(config);
        } catch (Throwable e) {
            throw new RepositoryException(null, cfg.getClass().getName(), e);
        }
        config.validate();
    }

    public String getKey() {
        return config.key;
    }

    public String getId() {
        return (repository != null ? repository.getKey() + "." : "") + getKey();
    }

    @Override
    public String toString() {
        return getId();
    }

    public Column<RAW> config(Runnable1<RepoFieldConfig> cfg) {
        try {
            cfg.run(config);
        } catch (Throwable e) {
            throw new RepositoryException(null, cfg.getClass().getName(), e);
        }
        config.validate();
        return this;
    }

    public Repository<?> getRepository(boolean mustExists) {
        if (mustExists && repository == null)
            throw new RepositoryException(null, "Kolumna " + getKey() + " nie ma przypisanego repozytorium (" + config.repository + ")");
        return repository;
    }

    RAW set(Record rec, final Object value) {
        Repository<?> repo = rec.repo;
        try {

//            if (rec.crude != CRUDE.CREATE && rec.crude != CRUDE.UPDATE)
//                throw new RepositoryException(repo, "Nie można modyfikować rekordu " + getId());
            int idx = new TList<>(repo.columns.values()).indexOf(this);
            if (idx < 0)
                throw new RepositoryException(repo, "Repozytorium nie posiada kolumny " + getKey());

            config.onBeforeSet.dispatch(this, r -> r.run(rec, value));

            final TObject<RAW> val = new TObject<RAW>(
                    value == null ? null
                            : config.parser != null
                                    ? config.parser.run(Utils.coalesce(value, config.defaultValue))
                                    : config.type.parse(Utils.coalesce(value, config.defaultValue)));

            if (rec.crude == CRUDE.UPDATE && this == repo.config.primaryKey) {
                Object pk = rec.getPrimaryKeyValue();
                if (pk != null && !pk.equals(val.get()))
                    throw new RepositoryException(repo, "Nie można modyfikować klucza głównego (" + Utils.escape(val) + ")");
            }

            config.validator.dispatch(this, r -> r.run(val, rec));

            Object src = rec.cells[idx];
            RAW v = val.get();

            validate(rec, v);

            if (Utils.equals(src, v))
                return v;

            Object prev = rec.cells[idx];
            rec.cells[idx] = v;
            config.onAfterSet.dispatch(this, r -> r.run(rec, v, null));
            rec.changed.put(this, new Pair<>(prev, v));

            return v;
        } catch (Throwable e) {
            config.onAfterSet.dispatch(this, r -> r.run(rec, null, null));
            throw new RepositoryException(repo, repo.config.key + "." + getKey(), e);
        }
    }

    public JObject getJson() {
        return config.getJson();
    }

    //ToDo: walidacja min, max, regex itp
    public void validate(Record record, Object value) {
        if (value == null && Boolean.TRUE.equals(config.required)
                && config.defaultValue == null
                && !Boolean.TRUE.equals(config.autoGenerated))
            throw new RepositoryException(record.repo, "Wartość pola " + record + "."
                    + getKey() + " nie może być pusta");
    }

    Object _serialize(Object value) throws Exception {
        return value == null ? null : config.serializer != null
                ? config.serializer.run((RAW) value)
                : ((DataType<RAW>) config.type).serialize((RAW) value);
    }

    public class RepoFieldConfig {

        public Class<? extends Repository<?>> repository;
        public DataType<? extends RAW> type;
        public Class<? extends RAW> clazz;
        public String key;

        public Boolean list;
        public Integer min;
        public Integer max;
        public Boolean trimmed;
        public String regex;
        public Align align;
        public CaseConvert caseConvert;
        public String allowedChars;
        public String defaultUnit;
        public Boolean onDemand; // wartość zostanie wczytana na żądanie pobrania rekordu - zalecane dla dużych danych
        /**
         * Wartość domyślna ustawiana, gdy pojawi się null
         */
        //ToDo: Przerobić na funkcję
        public RAW defaultValue;
        /**
         * Tekst lub tablica nie mogą być puste
         */
        public Boolean nonEmpty;
        public Boolean autoGenerated;
        public Boolean readOnly;
        public Boolean required;
        public Boolean unique;
        public CharSequence name;
        public CharSequence description;
        public CharSequence hint;
        public String daoName = UNDEFINED_NAME;
        public String daoType;
        //public Column<?> foreign;
        public final LinkedHashMap<String, String> enumerate = new LinkedHashMap<>();

        //----------------------------------------------------------------------
        public Boolean sortOrder; // true - rosnący, false malejący
        public CharSequence subtitle;
        public Boolean disabled; // kolumna niewidoczna, zawiera daodatkowe dane, nie można jej wyświetliś
        public Boolean hidden; // kolumna niewidoczna domyslnie (można wyświetlić z menu kontekstowego)
        public Boolean sortable; // można sortować
        public Boolean filtered; // można filtrowac po tej kolumnie
        public String dateFormat;
        public String[] dateFormats;
        public Boolean searchable;
        //----------------------------------------------------------------------
        public final Dispatcher<RunnableEx2<Record, Object>> onBeforeSet = new Dispatcher<>();
        public final Dispatcher<RunnableEx3<Record, RAW, Throwable>> onAfterSet = new Dispatcher<>();
        public CallableEx1<RAW, Object> parser;
        public CallableEx1<RAW, Object> daoParser;
        public CallableEx1<Object, RAW> serializer;
        public CallableEx1<Object, RAW> daoSerializer;
        public final Dispatcher<RunnableEx2<TObject<RAW>, Record>> validator = new Dispatcher<>();

        public void validate() {

            Objects.requireNonNull(key, "Wymagane pole 'klucz'");
            Objects.requireNonNull(type, "Wymagane pole 'type'");

            if (clazz == null)
                clazz = (Class<? extends RAW>) type.clazz;

            if (UNDEFINED_NAME.equals(daoName)) {
                daoName = NameFormat.camelCaseToUnderscore(key);
                if (Is.in(daoName.toUpperCase(), Database.RESERVED))
                    daoName = '"' + daoName + '"';
            }
        }

        private Params buildParams() {

            Column<?> foreign = null;
            if (Column.this instanceof IForeignColumn)
                foreign = ((IForeignColumn) Column.this).getForeignColumn();

            Object enumerate = !this.enumerate.isEmpty() ? this.enumerate : null;

            if (enumerate == null && type instanceof EnumDataType
                    && !((EnumDataType) type).enumerate.isEmpty()
                    && !((EnumDataType) type).isEmbeddedEnum())
                enumerate = ((EnumDataType) type).enumerate;

            if (enumerate == null && type instanceof EnumsDataType
                    && !((EnumsDataType) type).enumerate.isEmpty())
                enumerate = ((EnumsDataType) type).enumerate;

            return new Params()
                    .escape("key", key)
                    .escape("name", name)
                    .escape("hint", hint)
                    .escape("type", type.name)
                    .escape("onDemand", onDemand)
                    .escape("subtitle", subtitle)
                    .escape("description", description)
                    .escape("autoGenerated", autoGenerated)
                    .escape("defaultValue", defaultValue)
                    .escape("defaultUnit", defaultUnit)
                    .escape("hidden", hidden)
                    .escape("readOnly", readOnly)
                    .escape("required", required)
                    .escape("unique", unique)
                    .escape("subtitle", subtitle)
                    .escape("sortOrder", sortOrder)
                    .escape("align", align != null ? align.key : null)
                    .escape("searchable", searchable)
                    .escape("filtered", filtered)
                    .escape("disabled", disabled)
                    .escape("sortable", sortable)
                    .extra("foreign", foreign)
                    .extra("enumerate", enumerate);
        }

        public Params getParams() {
            Params params = buildParams();
            Is.notNullV(params.extra.get("foreign"), foreign -> params.add("foreign",
                    "() => " + Repositories.formatFieldName(((Column<?>) foreign).getRepository(true).getClass().getSimpleName())));
            Is.notNullV(params.extra.get("enumerate"), enumerate -> params.add("enumerate",
                    JSON.serialize(enumerate).asCollection().options.quotaNames(false).element.toString()));
            return params;
        }

        public JObject getJson() {
            JObject json = new JObject(key);
            json.options.singleLine(true);
            json.options.acceptNulls(false);

            Params params = buildParams();
            params.forEach(p -> json.put(p.name, p.raw));
            Is.notNullV(params.extra.get("foreign"), f -> json.put("foreign", ((Column<?>) f).repository.getKey()));
            Is.notNullV(params.extra.get("enumerate"), enumerate -> json.add("enumerate", enumerate));
            return json;
        }

    }

}
