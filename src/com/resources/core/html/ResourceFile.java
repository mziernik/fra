package com.resources.core.html;

import com.html.core.Html;
import com.html.core.dict.LinkType;
import com.html.core.tag.intfs.Parent;
import com.html.core.tag.meta.Link;
import com.html.core.tag.programming.Script;
import com.lang.LResource;
import com.utils.Url;
import com.utils.reflections.TClass;
import java.util.*;

public abstract class ResourceFile {

    final static List<ResourceFile> all = new LinkedList<>();

    public ResourceFile() {
        all.add(this);
    }

    protected final List<ScriptFile> dependencies = new LinkedList<>();

    public ResourceFile depends(ScriptFile other) {
        dependencies.add(other);
        return this;
    }

    public ResourceFile depends(Class<? extends ScriptFile> other) {
        dependencies.add(new TClass<ScriptFile>(other).newInstance(null));
        return this;
    }

    public void link(Parent parent, Url url) {
        String surl = url.path().toString().toLowerCase();

        Html html = parent.getHTML();
        if (html.properties.containsKey("res-file-" + surl))
            return;

        html.properties.put("res-file-" + surl, true);

        if (surl.endsWith(".js")) {
            new Script(parent)
                    .src(url)
                    .type("application/javascript");
            return;
        }

        if (surl.endsWith(".css")) {
            new Link(parent)
                    .type("text/css")
                    .rel(LinkType.stylesheet)
                    .href(url);
            return;
        }

        throw new RuntimeException(LResource.INVALID_FILE_FORMAT.toString(url.toString()));

    }

}
