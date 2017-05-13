package com.database;

import com.json.JArray;
import com.json.JCollection;
import com.json.JObject;
import com.json.exceptions.JException;
import com.mlogger.Log;
import com.resources.core.ResData;
import com.resources.core.Resources;
import com.script.ConfFile;
import com.utils.*;
import com.utils.collections.TList;
import com.utils.reflections.TClass;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.sql.SQLException;

//ToDo: dodać plik konfiguracyjny (skrypt), w którym będzie zdefiniowana struktura danych oraz proces migracji
public class DbStructure extends DbStructureEntry {

    private final Class<? extends Database> dbCls;
    public String sqlReleaseFilesPath = "META-INF/db";
    public String sqlUpdateFilesPath = "META-INF/db/update";
    public String tableName = "meta_data";
    public String keyColumnName = "key";
    public String valueColumnName = "value";
    public String dbRevisionKeyValue = "db.revision";
    public String dbDateKeyValue = "db.date";
    final TList<DbStructureEntry> updates = new TList<>();
    private final JObject json;
    final String confFilePath;

    public DbStructure(Class<? extends Database> dbCls, String confFilePath) throws SQLException {
        this.confFilePath = confFilePath;
        this.dbCls = dbCls;
        try {

            DBConnectionData cd = db().connData();

            ConfFile conf = new ConfFile(Resources.getStrF(null, confFilePath));
            conf.constants.put("DATABASE", cd.url);

            json = conf.process();
            read(json);
            JObject jmeta = json.objectF("meta");
            tableName = jmeta.getStr("table");

            keyColumnName = jmeta.objectF("column").getStr("key");
            valueColumnName = jmeta.objectF("column").getStr("value");

            dbRevisionKeyValue = jmeta.objectF("key").getStr("revision");
            dbDateKeyValue = jmeta.objectF("key").getStr("date");

            for (JArray arr : json.objectD("update").getArrays())
                updates.add(new DbStructureEntry().read(arr));

            updates.sort((o1, o2) -> {
                return o1.revision - o2.revision;
            });

        } catch (IOException ex) {
            throw new SQLException(ex);
        }

    }

    public void process(boolean isEmptyDatabase) throws SQLException {
        boolean created = false;

        try {
            if (isEmptyDatabase) {
                create();
                created = true;
            }
            Database db = db();

            Integer rev = Utils.strInt(db
                    .execute("SELECT ? FROM ? WHERE ? = ?",
                            new Unquoted(valueColumnName),
                            new Unquoted(tableName),
                            new Unquoted(keyColumnName),
                            dbRevisionKeyValue)
                    .firstD().getStr(valueColumnName, null), null);

            assert !created || rev != null;

            if (rev == null) {
                try {
                    updateInfo(db, 1, true);
                } catch (SQLException ex) {
                    throw ex;
                } catch (Exception ex) {
                    throw new SQLException(ex);
                }
                rev = 1;
            }

            if (rev == revision)
                return;

            update(rev);
        } catch (SQLException e) {
            throw e;
        } catch (Exception e) {
            throw new SQLException(e);
        }
    }

    protected Database db() {
        return new TClass<>(dbCls).newInstance(null);
    }

    protected void create() throws SQLException, IOException {
        TList<ResData> scripts = getScripts(this);
        db().transaction((Database db) -> {
            for (ResData res : scripts)
                db.execute(res);
            // zapisz wersję bazy
            updateInfo(db, revision, true);

        });
    }

    protected void updateInfo(Database db, int rev, boolean create) throws Exception {
        if (create) {
            db.insert(tableName)
                    .arg(keyColumnName, dbRevisionKeyValue)
                    .arg(valueColumnName, revision)
                    .execute();
            return;
        }
        db.update(tableName, "? = ?", new Unquoted(keyColumnName), dbRevisionKeyValue)
                .arg(valueColumnName, rev)
                .execute();
    }

    protected void update(int rev) throws SQLException, IOException {
        Log.info("Wymagana aktualizacja bazy danych " + rev + " -> " + revision);

        TList<ResData> scripts = new TList<>();

        int newVer = 0;
        for (DbStructureEntry en : updates)
            if (en.revision > rev && en.revision <= revision) {
                scripts.addAll(en.getScripts(this));
                if (en.revision > newVer)
                    newVer = en.revision;
            }

        if (newVer != revision || scripts.isEmpty()) {
            Log.warning("Brak możliwości aktualizacji wersji usługi z "
                    + rev + " do " + revision);
            return;
        }

        int _newVer = newVer;

        db().transaction((Database db) -> {
            for (ResData res : scripts)
                db.execute(res);
            // zapisz wersję bazy
            updateInfo(db, _newVer, false);

        });
    }

}

class DbStructureEntry {

    public final TList<String> files = new TList<>();
    public int revision;

    DbStructureEntry read(JCollection json) throws JException {

        if (json.isObject()) {
            JObject obj = json.asObject();
            revision = obj.getInt("revision");
            files.addAll(obj.arrayF("files").getValuesStr());
            return this;
        }

        JArray arr = json.asArray();
        revision = Utils.strInt(arr.getName());
        files.addAll(json.asArray().getValuesStr());

        return this;
    }

    TList<ResData> getScripts(DbStructure struct) throws IOException {

        Path dir = new Path(struct.confFilePath).getParent();

        TList<ResData> scripts = new TList<>();
        for (String s : files) {
            ResData res = Resources.get(dir.getPath(s), false);

            if (res == null)
                res = Resources.get(s, false);
            if (res == null)
                throw new FileNotFoundException(s);

            scripts.add(res);
        }

        return scripts;
    }

}
