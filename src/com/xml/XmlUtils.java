package com.xml;

import com.json.Escape;
import com.utils.Str;
import java.util.Collection;

public class XmlUtils {

    public static XmlNode getSoapBody(final XmlNode xml) {
        if (xml != null)
            for (XmlNode nd : xml.getNodes())
                if (nd.getName().toLowerCase().contains("body"))
                    return nd.getNodes().getFirst();

        return null;
    }

    public static void removeSoapElements(final XmlNode xml) throws XmlException {
        xml.visit(new XmlVisitor() {

            final Collection<String> nss = xml.getDeclaredNamespaces().values();

            @Override
            public boolean visit(XmlNode node, int level) throws XmlException {

                // usun atrybuty schematów
                for (XmlAttribute attr : node.getAttributes()) {
                    final String name = attr.getName();

                    if (new Str(name).startsWith("xmlns:", "soapenv:")) {
                        attr.delete();
                        continue;
                    }
                    for (String s : nss)
                        if (name.startsWith(s + ":type")) {
                            attr.delete();
                            continue;
                        }
                }

                String name = node.getName();
                if (name.contains(":"))
                    node.setName(name.substring(name.indexOf(":") + 1, name.length()));

                for (XmlNode nd : node.getNodes()) {
                    name = nd.getName();
                    if (name.contains(":"))
                        nd.setName(name.substring(name.indexOf(":") + 1, name.length()));

                }

                return true;
            }
        });

    }

}
