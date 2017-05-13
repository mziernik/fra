/*
 */
package com.xml;

import com.xml.elements.XmlTextNode;
import java.util.List;
import org.junit.Assert;
import org.junit.Test;

/**
 *
 * @author user
 */
public class XMLTest {

    @Test
    public void t0() throws XmlException {

        XML xml = new XML("<abc/>");

        XmlNode nn = xml.nodeD("dddd");
        nn.toString();

        Integer aInt = nn.getInt("aaaa", null);

    }

    //@Test
    public void t1() throws XmlException {

        XML xml = new XML(serverXML);

        List<XmlAttribute> getAttributes = xml.getAttributes("Listener/className");

        for (XmlAttribute attr : getAttributes)
            System.out.println(attr.value);

    }

    //  @Test
    public void lineBreak() throws XmlException {
        String value = "1 linia,\n2linia\n\ttabulator\n3linia";

        XML xml = new XML();
        xml.setText(value);
        xml.options.header = null;
        xml.options.singleLine = true;

        String sXML = xml.toString();

        xml = new XML(sXML);

        String val = xml.getText();
        Assert.assertEquals(value, val);

    }

    public final static String serverXML = "<?xml version='1.0' encoding='utf-8'?>\n"
            + "\n"
            + "<Server port=\"8005\" shutdown=\"SHUTDOWN\">\n"
            + "  <Listener className=\"org.apache.catalina.core.AprLifecycleListener\" SSLEngine=\"on\" />\n"
            + "  <Listener className=\"org.apache.catalina.core.JasperListener\" />\n"
            + "  <Listener className=\"org.apache.catalina.core.JreMemoryLeakPreventionListener\" />\n"
            + "  <Listener className=\"org.apache.catalina.mbeans.GlobalResourcesLifecycleListener\" />\n"
            + "  <Listener className=\"org.apache.catalina.core.ThreadLocalLeakPreventionListener\" />\n"
            + "\n"
            + "	<GlobalNamingResources>\n"
            + "		<Resource name=\"UserDatabase\" auth=\"Container\"\n"
            + "			type=\"org.apache.catalina.UserDatabase\"\n"
            + "            description=\"User database that can be updated and saved\"\n"
            + "            factory=\"org.apache.catalina.users.MemoryUserDatabaseFactory\"\n"
            + "            pathname=\"conf/tomcat-users.xml\" />\n"
            + "	</GlobalNamingResources>\n"
            + "\n"
            + "	<!-- protocol=\"org.apache.coyote.http11.Http11NioProtocol\" -->\n"
            + "	<Service name=\"Catalina\">\n"
            + "\n"
            + "		<Connector \n"
            + "			port=\"8080\" \n"
            + "			connectionTimeout=\"20000\"\n"
            + "			maxHttpHeaderSize = \"1048576\"\n"
            + "			maxPostSize = \"0\"							  \n"
            + "			URIEncoding=\"UTF-8\"\n"
            + "			redirectPort=\"8443\" \n"
            + "			protocol=\"org.apache.coyote.http11.Http11NioProtocol\"\n"
            + "		/>\n"
            + "	   \n"
            + "		<Connector \n"
            + "			SSLEnabled=\"true\" \n"
            + "			acceptCount=\"100\" \n"
            + "			clientAuth=\"false\"\n"
            + "			disableUploadTimeout=\"true\" \n"
            + "			enableLookups=\"false\" \n"
            + "		/>\n"
            + "\n"
            + "    <!--   <Connector port=\"8009\" protocol=\"AJP/1.3\" redirectPort=\"8443\" />  -->\n"
            + "\n"
            + "    <Engine name=\"Catalina\" defaultHost=\"localhost\">\n"
            + "		<Realm className=\"org.apache.catalina.realm.LockOutRealm\">\n"
            + "		<Realm className=\"org.apache.catalina.realm.UserDatabaseRealm\" resourceName=\"UserDatabase\"/> </Realm>\n"
            + "		<Host appBase=\"webapps\" autoDeploy=\"true\" name=\"localhost\" unpackWARs=\"true\">\n"
            + "			<Valve className=\"org.apache.catalina.valves.AccessLogValve\" directory=\"logs\" suffix=\".txt\"/>\n"
            + "		</Host>\n"
            + "    </Engine>\n"
            + "	\n"
            + "  </Service>\n"
            + "\n"
            + "</Server>";

    public final String xhtml1 = "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.1//EN\" \"http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd\">\n"
            + "<html xmlns=\"http://www.w3.org/1999/xhtml\">\n"
            + "  <head>\n"
            + "    <link rel=\"stylesheet\" href=\"style.css\" type=\"text/css\" />\n"
            + "    <meta http-equiv=\"Content-Type\" content=\"application/xhtml+xml; charset=utf-8\" />\n"
            + "    <title>WolneLektury.pl</title>\n"
            + "  </head>\n"
            + "  <body>\n"
            + "    <div id=\"book-text\">\n"
            + "      <h1>\n"
            + "        <span class=\"author\">Hamlet</span>\n"
            + "      </h1>\n"
            + "      <div class=\"person-list\">\n"
            + "        <h3>OSOBY</h3>\n"
            + "        <ol>\n"
            + "          <li>KLAUDIUSZ &mdash; król duński</li>\n"
            + "          <li>HAMLET &mdash; syn poprzedniego, a synowiec teraźniejszego króla</li>\n"
            + "          <li>POLONIUSZ &mdash; szambelan</li>\n"
            + "          <li>HORACY &mdash; przyjaciel Hamleta</li>\n"
            + "          <li>LAERTES &mdash; syn Poloniusza</li>\n"
            + "          <li>WOLTYMAND, KORNELIUSZ, ROZENKRANC, GILDENSTERN, OZRYK &mdash; dworzanie</li>\n"
            + "        </ol>\n"
            + "      </div>\n"
            + "      <div class=\"place-and-time\">Rzecz się odbywa w Elzynorze.</div>\n"
            + "    </div>\n"
            + "  </body>\n"
            + "</html>";

    public final String contentOpf = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
            + "<package unique-identifier=\"uid\" version=\"2.0\" xmlns=\"http://www.idpf.org/2007/opf\">\n"
            + "  <metadata xmlns:dc=\"http://purl.org/dc/elements/1.1/\" xmlns:opf=\"http://www.idpf.org/2007/opf\">\n"
            + "    <title>tytul</title>\n"
            + "    <dc:title>Eprasa1 nr 11/2013</dc:title>\n"
            + "    <dc:creator>Jacek Piekara</dc:creator>\n"
            + "    <dc:subject></dc:subject>\n"
            + "    <dc:description>Eprasa1 opis nr 11/2013</dc:description>\n"
            + "    <dc:publisher>Akcent</dc:publisher>\n"
            + "    <dc:date>2013-09-10</dc:date>\n"
            + "    <dc:language>pl</dc:language>\n"
            + "    <dc:source>Infover</dc:source>\n"
            + "    <dc:relation>www.eclicto.pl</dc:relation>\n"
            + "    <dc:identifier id=\"uid\">6e19e651-68c7-4814-b495-ceaa9b6be215</dc:identifier>\n"
            + "    <meta content=\"21386\" name=\"pid\"></meta>\n"
            + "    <meta content=\"0\" name=\"fid\"></meta>\n"
            + "  </metadata>\n"
            + "</package>";

    public final static String prvlist = "  <prvlist>\n"
            + "    <prv>\n"
            + "      <name>Operator 1</name>\n"
            + "      <id>1</id>\n"
            + "    </prv>\n"
            + "    <prv>\n"
            + "      <name>Operator 2</name>\n"
            + "      <id>2</id>\n"
            + "    </prv>\n"
            + "  </prvlist>";

}
