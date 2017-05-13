package com.context;

import com.context.fra.Framework;
import com.context.unit_test.FraUnitTestContext;
import com.database.QueryRows;
import com.database.queries.MultipleQuery;
import com.database.service.ServiceDB;
import com.mlogger.Log;
import com.resources.core.Resources;
import com.servlet.Handlers;
import com.utils.Utils;
import com.utils.Is;
import com.utils.collections.TList;
import com.utils.date.TDate;
import com.xml.XML;
import com.xml.XmlNode;
import java.io.InputStream;
import java.net.URL;
import java.sql.SQLException;
import java.text.ParseException;
import java.util.Properties;

public class ServiceStatus {

    public String version;
    public int update; // kolejny numer aktualizacji usługi na danm serwerze 
    public int build;
    public int revision;
    public Integer svnRev;
    public TDate updateDate = new TDate();
    public TDate buildDate = new TDate();

    //==============
    public int prevSvnRev;
    public int prevRev = revision;
    public String prevVersion = version;
    public int prevBuild = build;
    public TDate prevBuildDate = buildDate;
    //========================

    public final TList<SvnHistoryEntry> svnHistory = new TList<>();
    public final String prefix;
    public final boolean isFramework;
    private final String metaInfPath;

    public ServiceStatus(boolean isFramework) {
        this.prefix = isFramework ? "status.fra." : "status.svr.";
        metaInfPath = "/META-INF/" + (isFramework ? "fra/" : "");
        this.isFramework = isFramework;
    }

    public void load() throws Exception {

        if (!isFramework && Framework.isRunning())
            return;

        QueryRows meta = new ServiceDB().execute("SELECT * FROM meta_data");

        prevVersion = version = meta.findFirstD("key", prefix + "build.version").getStr("value", "1");
        prevBuild = build = meta.findFirstD("key", prefix + "build.number").getInt("value", 1);
        prevRev = revision = meta.findFirstD("key", prefix + "build.revision").getInt("value", 1);
        prevSvnRev = svnRev = meta.findFirstD("key", prefix + "svn.revision").getInt("value", 1);
        prevBuildDate = buildDate = meta.findFirstD("key", prefix + "build.date").getDate("value", new TDate());
        update = meta.findFirstD("key", prefix + "update.number").getInt("value", 0);
        updateDate = meta.findFirstD("key", prefix + "update.date").getDate("value", new TDate());

        if (!FraUnitTestContext.isRunning()) {

            Properties properties = new Properties();
            try (InputStream in = Resources.getF(
                    metaInfPath + "version.properties", false).getInputStream()) {
                properties.load(in);
            }
            build = Utils.strIntForce(properties.getProperty("build"), 0);
            revision = Utils.strIntForce(properties.getProperty("revision"), 0);
            version = properties.getProperty("version");
            try {
                buildDate = new TDate(properties.getProperty("date"));
            } catch (Exception e) {
                Log.error(e);
            }

        }

        URL infoX = getClass().getResource(metaInfPath + "svn-info.xml");
        if (infoX != null)
            try {
                XML xml = new XML(infoX);
                svnRev = xml.nodeD("entry").attrInt("revision", 0);
            } catch (Throwable e) {
                Log.error(e);
            }

        URL logX = getClass().getResource(metaInfPath + "svn-log.xml");
        if (logX != null)
            try {
                XML xml = new XML(logX);

                for (XmlNode node : xml.getNodes()) {
                    SvnHistoryEntry en = new SvnHistoryEntry(node);
                    if (en.rev > prevSvnRev)
                        svnHistory.add(en);
                }
            } catch (Throwable e) {
                Log.error(e);
            }

        if (!isFramework && !Framework.isRunning() && prevRev != revision)
            Handlers.events.getInstance().onServiceVersionUpdate(this);

        save(); // zapisz bieżące wartości

    }

    public void save() throws SQLException {

        ServiceDB db = new ServiceDB();

        MultipleQuery qry = db.multipleQuery();

        qry.add(db.merge("meta_data", "key")
                .arg("key", prefix + "build.version")
                .arg("value", version));

        qry.add(db.merge("meta_data", "key")
                .arg("key", prefix + "build.revision")
                .arg("value", revision));

        qry.add(db.merge("meta_data", "key")
                .arg("key", prefix + "svn.revision")
                .arg("value", svnRev));

        qry.add(db.merge("meta_data", "key")
                .arg("key", prefix + "build.number")
                .arg("value", build));

        qry.add(db.merge("meta_data", "key")
                .arg("key", prefix + "build.date")
                .arg("value", buildDate));

        qry.add(db.merge("meta_data", "key")
                .arg("key", prefix + "update.number")
                .arg("value", update));

        qry.add(db.merge("meta_data", "key")
                .arg("key", prefix + "update.date")
                .arg("value", updateDate));

        qry.execute();

    }

    public class SvnHistoryEntry {

        public int rev;
        public String author;
        public String message;
        public TDate date;

        private SvnHistoryEntry(XmlNode node) throws ParseException {

            rev = node.attrInt("revision", 0);
            author = node.getStr("author", null);
            String sDate = node.getStr("date", null);
            message = node.getStr("msg", null);
            if (sDate != null)
                date = new TDate(sDate, "yyyy-MM-dd'T'HH:mm:ss.SSS'Z'");
        }

    }

}


/*
 * <info>
  <entry kind="dir" path="." revision="3561">
    <url>svn://10.0.3.6/lincall/trunk/Framework</url>
    <relative-url>^/trunk/Framework</relative-url>
    <repository>
      <root>svn://10.0.3.6/lincall</root>
      <uuid>1e664b20-4808-43cc-9119-e8873f3e0592</uuid>
    </repository>
    <wc-info>
      <wcroot-abspath>X:/Workspace/Lincall</wcroot-abspath>
      <schedule>normal</schedule>
      <depth>infinity</depth>
    </wc-info>
    <commit revision="3556">
      <author>milosz.ziernik</author>
      <date>2017-02-01T13:07:52.013283Z</date>
    </commit>
  </entry>
</info>
 */
